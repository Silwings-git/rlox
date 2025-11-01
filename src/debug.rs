use crate::{
    chunk::{Chunk, Instruction, OpCode, Operand},
    value::print_value,
};

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");

    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("{:>4} ", "|")
    } else {
        print!("{:<4} ", chunk.get_line(offset).unwrap_or(0))
    }

    let instruction = match chunk.read_opcode(offset) {
        Some(op) => op,
        None => {
            println!("Unknown opcode, offset: {offset}");
            return offset + 1;
        }
    };

    match instruction.op {
        OpCode::Return => simple_instruction("OP_RETURN"),
        OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, &instruction),
        OpCode::Negate => simple_instruction("OP_NEGATE"),
        OpCode::Add => simple_instruction("OP_ADD"),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT"),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY"),
        OpCode::Divide => simple_instruction("OP_DIVIDE"),
        OpCode::True => simple_instruction("OP_TRUE"),
        OpCode::False => simple_instruction("OP_FALSE"),
        OpCode::Nil => simple_instruction("OP_NIL"),
        OpCode::Not => simple_instruction("OP_NOT"),
        OpCode::Equal => simple_instruction("OP_EQUAL"),
        OpCode::Gerater => simple_instruction("OP_GERATER"),
        OpCode::Less => simple_instruction("OP_LESS"),
        OpCode::Print => simple_instruction("OP_PRINT"),
        OpCode::Pop => simple_instruction("OP_POP"),
        OpCode::Popn => popn_instruction("OP_POPN", &instruction),
        OpCode::DefineGlobal => simple_instruction("OP_DEFINE_GLOBAL"),
        OpCode::GetGlobal => simple_instruction("OP_GET_GLOBAL"),
        OpCode::SetGlobal => simple_instruction("OP_SET_GLOBAL"),
        OpCode::GetLocal => byte_instruction("OP_GET_LOCAL", &instruction),
        OpCode::SetLocal => byte_instruction("OP_SET_LOCAL", &instruction),
    }

    offset + instruction.len
}

fn byte_instruction(name: &str, instruction: &Instruction) {
    let slot = match instruction.operand {
        Operand::U8(slot) => slot as usize,
        Operand::None => 0,
    };
    println!("{name:<16} {slot:4}");
}

fn simple_instruction(name: &str) {
    println!("{name}");
}

fn constant_instruction(name: &str, chunk: &Chunk, instruction: &Instruction) {
    print!("{:<16} {:>4} ", name, instruction.operand);
    let constant = chunk.read_constant(&instruction.operand);
    if let Some(v) = constant {
        print_value(v);
        println!();
    } else {
        println!("常量池中未找到目标常量, 指令信息: {instruction:?}");
    }
}

fn popn_instruction(name: &str, instruction: &Instruction) {
    println!("{:<16} {:>4} ", name, instruction.operand);
}
