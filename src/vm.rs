use crate::chunk::{Chunk, Instruction};
use crate::chunk::{OpCode, Operand};
use crate::compiler::Parser;
use crate::config::VMConfig;
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

// todo 优化栈,使其支持最大容量限制
pub struct VM {
    chunk: Chunk,
    // 始终指向即将执行的指令
    ip: usize,
    stack: Stack,
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
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.peek(distance)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn push<T: Into<Value>>(&mut self, v: T) -> Result<(), InterpretError> {
        self.stack.push(v)
    }

    fn reset_stack(&mut self) {
        self.stack.reset_stack();
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
                    print_value(&self.pop().ok_or(InterpretError::RuntimeError(
                        "return: stack is empty".to_string(),
                    ))?);
                    println!();
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.read_constant(&instruction.operand)?;
                    self.push(*constant)?;
                }
                OpCode::Negate => {
                    let number = self
                        .peek(0)
                        .and_then(|v| v.as_number().ok())
                        .ok_or_else(|| self.runtime_error("Operand must be a number."))?;
                    self.pop().unwrap();
                    self.push(-number)?;
                }
                OpCode::Add => binary_op!(self,Value::Number,+),
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
            }
        }
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
}
