use std::fmt::Display;

use crate::value::{Value, ValueArray};

macro_rules! opcodes {
    ($($name:ident = $val:expr),*) => {
        #[derive(Debug)]
        #[repr(u8)]
        #[non_exhaustive]
        pub enum OpCode {
            $($name = $val),*
        }

        impl TryFrom<u8> for OpCode {
            type Error = u8;

            fn try_from(v: u8) -> Result<Self, Self::Error> {
                match v {
                    $($val => Ok(Self::$name)),*,
                    _ => Err(v)
                }
            }
        }
    };
}

opcodes! {
    Return = 0x01,
    Constant = 0x02,
    // 负号
    Negate = 0x03,
    // +
    Add=0x04,
    // -
    Subtract=0x05,
    // *
    Multiply=0x06,
    // /
    Divide=0x07
}

// 操作数类型定义
#[derive(Debug)]
pub enum Operand {
    // 无操作数
    None,
    // 1字节无符号整数
    U8(u8),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::None => write!(f, "-"),
            Operand::U8(v) => write!(f, "{v}"),
        }
    }
}

// 操作数结构体：存储完整指令
#[derive(Debug)]
pub struct Instruction {
    pub op: OpCode,
    pub operand: Operand,
    pub len: usize,
}

impl Instruction {
    pub fn new(op: OpCode, operand: Operand, len: usize) -> Self {
        Self { op, operand, len }
    }
}

/// 指令动态数组
#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: ValueArray,
    pub lines: Vec<(u32, u32)>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: ValueArray::new(),
            lines: vec![],
        }
    }

    pub fn get_line(&self, offset: usize) -> Option<u32> {
        let mut sum = 0;
        for (line, count) in &self.lines {
            sum += *count as usize;
            if offset <= sum {
                return Some(*line);
            }
        }
        None
    }

    fn write_byte(&mut self, op: u8, line: u32) {
        self.code.push(op);
        match self.lines.last_mut() {
            Some((last_line, count)) if *last_line == line => {
                *count += 1;
            }
            _ => self.lines.push((line, 1)),
        }
    }

    pub fn write_chunk_op_code(&mut self, op_code: OpCode, line: u32) {
        self.write_byte(op_code as u8, line);
    }

    pub fn write_chunk_operand(&mut self, operand: Operand, line: u32) {
        match operand {
            Operand::None => {}
            Operand::U8(u) => self.write_byte(u, line),
        }
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn read_opcode(&self, offset: usize) -> Option<Instruction> {
        let byte = self.code.get(offset);

        let op_code = byte.and_then(|b| OpCode::try_from(*b).ok())?;

        match op_code {
            OpCode::Return => Some(Instruction::new(OpCode::Return, Operand::None, 1)),
            OpCode::Constant => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => {
                        Some(Instruction::new(OpCode::Constant, Operand::U8(*operand), 2))
                    }
                    _ => {
                        println!(
                            "读取 OpConstant 失败：在偏移量 {offset} 处缺少操作数（需要访问偏移量 {operand_offset}，总长度：{})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::Negate => Some(Instruction::new(OpCode::Negate, Operand::None, 1)),
            OpCode::Add => Some(Instruction::new(OpCode::Add, Operand::None, 1)),
            OpCode::Subtract => Some(Instruction::new(OpCode::Subtract, Operand::None, 1)),
            OpCode::Multiply => Some(Instruction::new(OpCode::Multiply, Operand::None, 1)),
            OpCode::Divide => Some(Instruction::new(OpCode::Divide, Operand::None, 1)),
        }
    }

    /// 向常量池添加常量
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write_value_array(value);
        self.constants.len() - 1
    }

    pub fn read_constant(&self, operand: &Operand) -> Option<&Value> {
        match operand {
            Operand::None => None,
            Operand::U8(index) => self.constants.get_value(*index as usize),
        }
    }
}
