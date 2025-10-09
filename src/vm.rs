use crate::chunk::{Chunk, Instruction};
use crate::chunk::{OpCode, Operand};
#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;
use crate::value::{Value, print_value};

macro_rules! binary_op {
    ($vm:expr, $op:tt) => {{
        if $vm.peek(0).is_none() || $vm.peek(1).is_none(){
            return Err(InterpretError::RuntimeError("Operand must be a number.".to_string()));
        }
        let b =  $vm.pop().unwrap();
        let a =  $vm.pop().unwrap();
        $vm.push(a $op b);
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

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), InterpretError> {
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
                    print_value(&self.stack.pop().ok_or(InterpretError::RuntimeError(
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
                    let value = self.stack.pop().ok_or(InterpretError::RuntimeError(
                        "negate: stack is empty".to_string(),
                    ))?;
                    self.stack.push(-value);
                }
                OpCode::Add => binary_op!(self,+),
                OpCode::Subtract => binary_op!(self,-),
                OpCode::Multiply => binary_op!(self,*),
                OpCode::Divide => binary_op!(self,/),
            }
        }
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
