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
    False=0x0A,
    // !
    Not=0x0B,
    // =
    Equal=0x0C,
    // >
    Gerater=0x0D,
    // <
    Less=0x0E,
    // 弹出栈顶值并将其遗弃
    Pop=0x0F,
    // 弹出n个栈顶值并将其遗弃
    Popn=0x10,
    // 定义全局变量
    DefineGlobal=0x11,
    // 获取全局变量
    GetGlobal=0x12,
    // 赋值全局变量
    SetGlobal=0x13,
    // 获取局部变量
    GetLocal=0x14,
    // 赋值局部变量
    SetLocal=0x15,
    // 如果为false则跳转(16位操作数)
    JumpIfFalse=0x16,
    // 跳转(16位操作数)
    Jump=0x17,
    // loop向前跳转
    Loop=0x18,
    // 调用
    Call=0x19,
    // 闭包
    Closure=0x1A,
    // 获取上值
    GetUpvalue=0x1B,
    // 设置上值
    SetUpvalue=0x1C,
    Print=0xFF
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
#[derive(Debug, Default, Clone)]
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
        // 辅助宏：处理无操作数指令
        macro_rules! no_operand {
            ($op:ident) => {
                Some(Instruction::new(OpCode::$op, Operand::None, 1))
            };
        }

        // 辅助宏：处理 U8 操作数指令
        macro_rules! u8_operand {
            ($op:ident, $op_name:expr) => {{
                let operand_offset = offset + 1;
                match self.code.get(operand_offset) {
                    Some(operand) => Some(Instruction::new(OpCode::$op, Operand::U8(*operand), 2)),
                    None => {
                        println!(
                            "Failed to read {}: Missing operand at offset {offset} (needs access at offset {operand_offset}, total length: {})",
                            $op_name,
                            self.code.len()
                        );
                        None
                    }
                }
            }};
        }

        // 辅助宏：处理 U16 操作数指令
        macro_rules! u16_operand {
            ($op:ident, $op_name:expr) => {{
                match self.read_u16_operand(offset) {
                    Some(operand) => Some(Instruction::new(OpCode::$op, Operand::U16(operand), 3)),
                    None => {
                        println!(
                            "Failed to read {}: Missing operands at offset {offset} (needs access at offsets {} and {}, total length: {})",
                            $op_name,
                            offset + 1,
                            offset + 2,
                            self.code.len()
                        );
                        None
                    }
                }
            }};
        }

        let byte = self.code.get(offset);
        let op_code = byte.and_then(|b| OpCode::try_from(*b).ok())?;

        match op_code {
            // 无操作数指令
            OpCode::Return => no_operand!(Return),
            OpCode::Negate => no_operand!(Negate),
            OpCode::Add => no_operand!(Add),
            OpCode::Subtract => no_operand!(Subtract),
            OpCode::Multiply => no_operand!(Multiply),
            OpCode::Divide => no_operand!(Divide),
            OpCode::True => no_operand!(True),
            OpCode::False => no_operand!(False),
            OpCode::Nil => no_operand!(Nil),
            OpCode::Not => no_operand!(Not),
            OpCode::Equal => no_operand!(Equal),
            OpCode::Gerater => no_operand!(Gerater),
            OpCode::Less => no_operand!(Less),
            OpCode::Print => no_operand!(Print),
            OpCode::Pop => no_operand!(Pop),

            // U8 操作数指令
            OpCode::Constant => u8_operand!(Constant, "OpConstant"),
            OpCode::Popn => u8_operand!(Popn, "Popn"),
            OpCode::DefineGlobal => u8_operand!(DefineGlobal, "OpDefineGlobal"),
            OpCode::GetGlobal => u8_operand!(GetGlobal, "OpGetGlobal"),
            OpCode::SetGlobal => u8_operand!(SetGlobal, "OpSetGlobal"),
            OpCode::GetLocal => u8_operand!(GetLocal, "OpGetLocal"),
            OpCode::SetLocal => u8_operand!(SetLocal, "OpSetLocal"),
            OpCode::Call => u8_operand!(Call, "Call"),
            OpCode::Closure => u8_operand!(Closure, "Closure"),

            // U16 操作数指令
            OpCode::JumpIfFalse => u16_operand!(JumpIfFalse, "OpJumpIfFalse"),
            OpCode::Jump => u16_operand!(Jump, "OpJump"),
            OpCode::Loop => u16_operand!(Loop, "OpLoop"),
            OpCode::GetUpvalue => todo!(),
            OpCode::SetUpvalue => todo!(),
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
