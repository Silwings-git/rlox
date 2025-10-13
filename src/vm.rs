use crate::chunk::{Chunk, Instruction};
use crate::chunk::{OpCode, Operand};
use crate::compiler::Parser;
#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;
use crate::value::{Value, print_value};

macro_rules! binary_op {
    ($vm:expr, $value_type:path, $op:tt) => {{
        let b = $vm.peek(0).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;
        let a = $vm.peek(1).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;

        $vm.pop().unwrap();
        $vm.pop().unwrap();

        $vm.push($value_type(a $op b));
    }};
}

type Stack = Vec<Value>;

pub struct VM {
    chunk: Chunk,
    // 始终指向即将执行的指令
    ip: usize,
    stack: Stack,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError(String),
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Default::default(),
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        if self.stack.len() > distance {
            Some(&self.stack[self.stack.len() - 1 - distance])
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn push<T: Into<Value>>(&mut self, v: T) {
        self.stack.push(v.into());
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
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
                print!("          ");
                if !self.stack.is_empty() {
                    for v in self.stack.iter().rev() {
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
                    self.push(*constant);
                }
                OpCode::Negate => {
                    let number = self
                        .peek(0)
                        .and_then(|v| v.as_number().ok())
                        .ok_or_else(|| self.runtime_error("Operand must be a number."))?;
                    self.pop().unwrap();
                    self.push(-number);
                }
                OpCode::Add => binary_op!(self,Value::Number,+),
                OpCode::Subtract => binary_op!(self,Value::Number,-),
                OpCode::Multiply => binary_op!(self,Value::Number,*),
                OpCode::Divide => binary_op!(self,Value::Number,/),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::Not => {
                    // `false` and `nil` are false, and all other values are true.
                    let pop = match self.pop().unwrap() {
                        Value::Bool(b) => b,
                        Value::Nil => false,
                        Value::Number(_) => true,
                    };
                    self.push(!pop);
                }
                OpCode::Equal => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a.eq(&b));
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
