use std::collections::HashMap;

use crate::chunk::{Chunk, Instruction};
use crate::chunk::{OpCode, Operand};
use crate::compiler::Parser;
use crate::config::VMConfig;
use crate::string_pool::{InternedString, StringPool};
use crate::value::{Value, print_value};

macro_rules! binary_op {
    ($vm:expr, $value_type:path, $op:tt) => {{
        let b = $vm.peek(0).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;
        let a = $vm.peek(1).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;

        $vm.pop().unwrap();
        $vm.pop().unwrap();

        $vm.push($value_type(a $op b))?;
    }};
}

type Table = HashMap<InternedString, Value>;

pub struct VM {
    chunk: Chunk,
    // 始终指向即将执行的指令
    ip: usize,
    stack: Stack,
    globals: Table,
    strings: StringPool,
}

struct Stack {
    inner: Vec<Value>,
    max_stack_depth: usize,
}

impl Stack {
    fn new(max_depth: usize) -> Self {
        Self {
            inner: vec![],
            max_stack_depth: max_depth,
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        if self.inner.len() > distance {
            Some(&self.inner[self.inner.len() - 1 - distance])
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<Value> {
        self.inner.pop()
    }

    fn popn(&mut self, n: usize) -> Result<Vec<Value>, InterpretError> {
        if n == 0 {
            return Ok(vec![]);
        }
        if n > self.inner.len() {
            return Err(InterpretError::RuntimeError(format!(
                "insufficient stack elements: need {}, got {}",
                n,
                self.inner.len()
            )));
        }
        let split_idx = self.inner.len().saturating_sub(n);

        let popped = self.inner.split_off(split_idx);

        Ok(popped)
    }

    fn push<T: Into<Value>>(&mut self, v: T) -> Result<(), InterpretError> {
        if self.inner.len() > self.max_stack_depth {
            return Err(InterpretError::RuntimeError(format!(
                "StackOverflow: max depth {} exceeded",
                self.max_stack_depth
            )));
        }
        self.inner.push(v.into());
        Ok(())
    }

    fn reset_stack(&mut self) {
        self.inner.clear();
    }

    fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    fn get_by_index(&self, index: usize) -> Option<&Value> {
        self.inner.get(index)
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn set_by_index(&mut self, index: usize, v: Value) {
        self.inner[index] = v;
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError(String),
}

impl VM {
    pub fn new(config: VMConfig) -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Stack::new(config.max_stack_depth),
            globals: HashMap::new(),
            strings: Default::default(),
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.peek(distance)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
    fn popn(&mut self, n: usize) -> Result<Vec<Value>, InterpretError> {
        self.stack.popn(n)
    }

    fn push<T: Into<Value>>(&mut self, v: T) -> Result<(), InterpretError> {
        self.stack.push(v)
    }

    fn reset_stack(&mut self) {
        self.stack.reset_stack();
    }

    fn intern_interned(&mut self, s: &InternedString) -> InternedString {
        self.strings.intern_interned(s)
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let mut chunk = Chunk::new();

        let mut parser = Parser::new(source, &mut chunk);
        if !parser.compile() {
            return Err(InterpretError::CompileError);
        }

        self.interpret_chunk(chunk)
    }

    pub fn interpret_chunk(&mut self, chunk: Chunk) -> Result<(), InterpretError> {
        self.intern_chunk_constants(&chunk);
        self.chunk = chunk;
        self.ip = 0;
        self.run()
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                use crate::debug::disassemble_instruction;
                print!("          ");
                if !self.stack.is_empty() {
                    for v in self.stack.inner.iter().rev() {
                        print!("[ ");
                        print_value(v);
                        print!(" ]");
                    }
                }
                println!();
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte()?;
            match instruction.op {
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.read_constant(&instruction.operand)?;
                    self.push(constant.clone())?
                }
                OpCode::Negate => {
                    let number = self
                        .peek(0)
                        .and_then(|v| v.as_number().ok())
                        .ok_or_else(|| self.runtime_error("Operand must be a number."))?;
                    self.pop().unwrap();
                    self.push(-number)?;
                }
                OpCode::Add => {
                    let b = self.peek(0).cloned();
                    let a = self.peek(1).cloned();
                    match (b, a) {
                        (Some(Value::String(s2)), Some(Value::String(s1))) => {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            // 动态字符串不驻留
                            self.push(s1 + s2)?
                        }
                        (Some(Value::Number(n2)), Some(Value::Number(n1))) => {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            self.push(n1 + n2)?
                        }
                        _ => {
                            return Err(
                                self.runtime_error("Operands must be two numbers or two strings.")
                            );
                        }
                    }
                }
                OpCode::Subtract => binary_op!(self,Value::Number,-),
                OpCode::Multiply => binary_op!(self,Value::Number,*),
                OpCode::Divide => binary_op!(self,Value::Number,/),
                OpCode::True => self.push(true)?,
                OpCode::False => self.push(false)?,
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::Not => {
                    // `false` and `nil` are false, and all other values are true.
                    let pop = match self.pop().unwrap() {
                        Value::Bool(b) => b,
                        Value::Nil => false,
                        Value::Number(_) => true,
                        Value::String(_) => true,
                    };
                    self.push(!pop)?;
                }
                OpCode::Equal => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a.eq(&b))?;
                }
                OpCode::Gerater => binary_op!(self,Value::Bool,>),
                OpCode::Less => binary_op!(self,Value::Bool,<),
                OpCode::Print => {
                    print_value(
                        &self
                            .pop()
                            .ok_or(InterpretError::RuntimeError("print: stack is empty".into()))?,
                    );
                    println!();
                    Ok(())?
                }
                OpCode::Pop => {
                    self.pop()
                        .ok_or(InterpretError::RuntimeError("pop: stack is empty".into()))?;
                }
                OpCode::Popn => {
                    if let Operand::U8(n) = &instruction.operand {
                        self.popn(*n as usize)?;
                    } else {
                        return Err(self.runtime_error("popn: operand is empty."));
                    }
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string(&instruction.operand).ok_or(
                        InterpretError::RuntimeError(
                            "Failed to read global variable name from constants.".into(),
                        ),
                    )?;
                    self.globals.insert(
                        name.as_string()?.to_owned(),
                        // 允许全局变量声明为nil
                        self.peek(0).cloned().unwrap_or(Value::Nil),
                    );
                    self.pop().ok_or(InterpretError::RuntimeError(
                        "define global: stack is empty".into(),
                    ))?;
                }
                OpCode::GetGlobal => {
                    let name = self
                        .read_string(&instruction.operand)
                        .ok_or(InterpretError::RuntimeError(
                            "Failed to read global variable name from constants.".into(),
                        ))?
                        .as_string()?;

                    let value = self.globals.get(name);
                    match value {
                        Some(v) => {
                            self.push(v.clone())?;
                        }
                        None => {
                            return Err(
                                self.runtime_error(&format!("Undefined variable {}", &name))
                            );
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = self.read_string(&instruction.operand).ok_or(
                        InterpretError::RuntimeError(
                            "Failed to read global variable name from constants.".into(),
                        ),
                    )?;
                    let name = name.as_string()?;
                    if self.globals.contains_key(name) {
                        self.globals
                            .insert(name.to_owned(), self.peek(0).cloned().unwrap_or(Value::Nil));
                    } else {
                        // 如果变量未声明,返回错误
                        return Err(
                            self.runtime_error(&format!("Undefined variable {}", name.as_str()))
                        );
                    }
                }
                OpCode::GetLocal => match instruction.operand {
                    Operand::U8(index) => {
                        let v = self
                            .stack
                            .get_by_index(index as usize)
                            .ok_or_else(||
                                 InterpretError::RuntimeError(format!("GetLocal instruction: index {} is out of bounds for stack (length: {})",                     index, self.stack.len())))?;
                        self.push(v.clone())?;
                    }
                    Operand::None => {
                        return Err(self.runtime_error("The instruction requires an index operand, but no operand was provided."));
                    }
                },
                OpCode::SetLocal => match instruction.operand {
                    Operand::U8(index) => {
                        let v = self.peek(0).cloned().unwrap_or(Value::Nil);
                        self.stack.set_by_index(index.into(), v);
                    }
                    Operand::None => {
                        return Err(self.runtime_error("The instruction requires an index operand, but no operand was provided."));
                    }
                },
            }
        }
    }

    fn read_string(&self, operand: &Operand) -> Option<&Value> {
        self.chunk.read_constant(operand)
    }

    fn runtime_error(&mut self, arg: &str) -> InterpretError {
        let line = self.chunk.get_line(self.ip - 1).unwrap_or(1);
        self.reset_stack();
        InterpretError::RuntimeError(format!("[line {line}] in script: {arg}"))
    }

    fn read_byte(&mut self) -> Result<Instruction, InterpretError> {
        let instruction = self
            .chunk
            .read_opcode(self.ip)
            .ok_or(InterpretError::RuntimeError("No next opcode".to_string()))?;
        self.ip += instruction.len;
        Ok(instruction)
    }

    fn read_constant(&self, operand: &Operand) -> Result<&Value, InterpretError> {
        self.chunk
            .read_constant(operand)
            .ok_or(InterpretError::RuntimeError(
                "Cannot find the constant".to_string(),
            ))
    }

    fn intern_chunk_constants(&mut self, chunk: &Chunk) {
        for ele in chunk.constants().into_iter() {
            if let Value::String(s) = ele {
                self.intern_interned(s);
            }
        }
    }
}
