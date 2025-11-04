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
    // 常量
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
    Divide=0x07,
    // nil
    Nil=0x08,
    // true
    True=0x09,
    // false
    False=0x10,
    // !
    Not=0x11,
    // =
    Equal=0x12,
    // >
    Gerater=0x13,
    // <
    Less=0x14,
    // 弹出栈顶值并将其遗弃
    Pop=0x15,
    // 弹出n个栈顶值并将其遗弃
    Popn=0x16,
    // 定义全局变量
    DefineGlobal=0x17,
    // 获取全局变量
    GetGlobal=0x18,
    // 赋值全局变量
    SetGlobal=0x19,
    // 获取局部变量
    GetLocal=0x20,
    // 赋值局部变量
    SetLocal=0x21,
    // 如果为false则跳转(16位操作数)
    JumpIfFalse=0x22,
    // 跳转(16位操作数)
    Jump=0x23,
    Print=0x99
}

// 操作数类型定义
#[derive(Debug)]
pub enum Operand {
    // 无操作数
    None,
    // 1字节无符号整数
    U8(u8),
    // 2字节无符号整数
    U16(u16),
}

/// 通用 trait：表示类型可与字节相关类型（如 (u8, u8)）相互转换
/// 关联类型 Target 定义转换目标（如 u16 ↔ (u8, u8)）
pub trait ByteConvertible {
    /// 转换的目标类型
    type Target;

    /// 转换为目标类型
    fn to(&self) -> Self::Target;
}

// (u8, u8) → u16
impl ByteConvertible for (u8, u8) {
    type Target = u16;

    fn to(&self) -> Self::Target {
        (self.0 as u16) << 8 | (self.1 as u16)
    }
}

// u16 → (u8, u8)
impl ByteConvertible for u16 {
    type Target = (u8, u8);

    fn to(&self) -> Self::Target {
        let high = (self >> 8) as u8 & u8::MAX;
        let low = *self as u8 & u8::MAX;
        (high, low)
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::None => write!(f, "-"),
            Operand::U8(v) => write!(f, "{v}"),
            Operand::U16(v) => write!(f, "{v}"),
        }
    }
}

// 操作数结构体：存储完整指令
#[derive(Debug)]
pub struct Instruction {
    pub op: OpCode,
    pub operand: Operand,
    // 完整指令的长度
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

    fn replace_by_index(&mut self, index: usize, op: u8) {
        self.code[index] = op;
    }

    pub fn write_chunk_op_code(&mut self, op_code: OpCode, line: u32) {
        self.write_byte(op_code as u8, line);
    }

    pub fn write_chunk_placeholder(&mut self, line: u32) {
        self.write_byte(0xFF, line);
    }

    pub fn replace_operand_by_index(&mut self, index: usize, operand: Operand) {
        match operand {
            Operand::None => {}
            Operand::U8(u) => self.replace_by_index(index, u),
            Operand::U16(u) => {
                let (hight_byte, low_byte) = u.to();
                self.replace_by_index(index, hight_byte);
                self.replace_by_index(index + 1, low_byte);
            }
        }
    }

    pub fn write_chunk_operand(&mut self, operand: Operand, line: u32) {
        match operand {
            Operand::None => {}
            Operand::U8(u) => self.write_byte(u, line),
            Operand::U16(u) => {
                let (hight_byte, low_byte) = u.to();
                self.write_byte(hight_byte, line);
                self.write_byte(low_byte, line);
            }
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
                            "Failed to read OpConstant: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
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
            OpCode::True => Some(Instruction::new(OpCode::True, Operand::None, 1)),
            OpCode::False => Some(Instruction::new(OpCode::False, Operand::None, 1)),
            OpCode::Nil => Some(Instruction::new(OpCode::Nil, Operand::None, 1)),
            OpCode::Not => Some(Instruction::new(OpCode::Not, Operand::None, 1)),
            OpCode::Equal => Some(Instruction::new(OpCode::Equal, Operand::None, 1)),
            OpCode::Gerater => Some(Instruction::new(OpCode::Gerater, Operand::None, 1)),
            OpCode::Less => Some(Instruction::new(OpCode::Less, Operand::None, 1)),
            OpCode::Print => Some(Instruction::new(OpCode::Print, Operand::None, 1)),
            OpCode::Pop => Some(Instruction::new(OpCode::Pop, Operand::None, 1)),
            OpCode::Popn => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(OpCode::Popn, Operand::U8(*operand), 2)),
                    _ => {
                        println!(
                            "Failed to read Popn: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::DefineGlobal => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(
                        OpCode::DefineGlobal,
                        Operand::U8(*operand),
                        2,
                    )),
                    None => {
                        println!(
                            "Failed to read OpDefineGlobal: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::GetGlobal => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(
                        OpCode::GetGlobal,
                        Operand::U8(*operand),
                        2,
                    )),
                    None => {
                        println!(
                            "Failed to read OpGetGlobal: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::SetGlobal => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(
                        OpCode::SetGlobal,
                        Operand::U8(*operand),
                        2,
                    )),
                    None => {
                        println!(
                            "Failed to read OpSetGlobal: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::GetLocal => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => {
                        Some(Instruction::new(OpCode::GetLocal, Operand::U8(*operand), 2))
                    }
                    None => {
                        println!(
                            "Failed to read OpGetLocal: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::SetLocal => {
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => {
                        Some(Instruction::new(OpCode::SetLocal, Operand::U8(*operand), 2))
                    }
                    None => {
                        println!(
                            "Failed to read OpSetLocal: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            self.code.len()
                        );
                        None
                    }
                }
            }
            OpCode::JumpIfFalse => match self.read_u16_operand(offset) {
                Some(operand) => Some(Instruction::new(
                    OpCode::JumpIfFalse,
                    Operand::U16(operand),
                    3,
                )),
                None => {
                    println!(
                        "Failed to read OpJumpIfFalse: Missing operands at offset {offset} (needs access at offsets {} and {}, total length: {})",
                        offset + 1,
                        offset + 2,
                        self.code.len()
                    );
                    None
                }
            },
            OpCode::Jump => match self.read_u16_operand(offset) {
                Some(operand) => Some(Instruction::new(OpCode::Jump, Operand::U16(operand), 3)),
                None => {
                    println!(
                        "Failed to read OpJump: Missing operands at offset {offset} (needs access at offsets {} and {}, total length: {})",
                        offset + 1,
                        offset + 2,
                        self.code.len()
                    );
                    None
                }
            },
        }
    }

    /// 读取双字节操作数（high_byte + low_byte）
    fn read_u16_operand(&self, offset: usize) -> Option<u16> {
        if let (Some(&high_byte), Some(&low_byte)) =
            (self.code.get(offset + 1), self.code.get(offset + 2))
        {
            Some((high_byte, low_byte).to())
        } else {
            None
        }
    }

    /// 向常量池添加常量
    pub fn add_constant<T: Into<Value>>(&mut self, value: T) -> usize {
        self.constants.write_value_array(value);
        self.constants.len() - 1
    }

    pub fn read_constant(&self, operand: &Operand) -> Option<&Value> {
        match operand {
            Operand::None => None,
            Operand::U8(index) => self.constants.get_value(*index as usize),
            Operand::U16(index) => self.constants.get_value(*index as usize),
        }
    }

    pub fn constants(&self) -> &ValueArray {
        &self.constants
    }
}
