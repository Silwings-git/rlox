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
    // return
    OpReturn = 0x01,
    OpConstant = 0x02
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
#[derive(Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: ValueArray,
    lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: ValueArray::new(),
            lines: vec![],
        }
    }

    fn write_chunk(&mut self, op: u8, line: u32) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub fn write_chunk_op_code(&mut self, op_code: OpCode, line: u32) {
        self.write_chunk(op_code as u8, line);
    }

    pub fn write_chunk_operand(&mut self, operand: Operand, line: u32) {
        match operand {
            Operand::None => {}
            Operand::U8(u) => self.write_chunk(u, line),
        }
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn read_opcode(&self, offset: usize) -> Option<Instruction> {
        let byte = self.code.get(offset);
        match byte {
            Some(0x01) => Some(Instruction::new(OpCode::OpReturn, Operand::None, 1)),
            Some(0x02) => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(
                        OpCode::OpConstant,
                        Operand::U8(*operand),
                        2,
                    )),
                    _ => {
                        println!(
                            "读取 OpConstant 失败：在偏移量 {} 处缺少操作数（需要访问偏移量 {}，总长度：{}）",
                            offset,
                            operand_offset,
                            self.code.len()
                        );
                        None
                    }
                }
            }
            Some(byte) => {
                println!("未知操作码：在偏移量 {offset} 处发现 0x{byte:02X}（十进制：{byte})");
                None
            }
            None => {
                println!(
                    "读取操作码失败：偏移量 {offset} 超出字节码范围（总长度：{})",
                    self.code.len()
                );
                None
            }
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

    pub fn get_line(&self, index: usize) -> u32 {
        *self.lines.get(index).unwrap_or(&0)
    }
}
