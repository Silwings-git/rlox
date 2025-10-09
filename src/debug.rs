use crate::{
    chunk::{Chunk, Instruction, OpCode},
    value::print_value,
};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");

    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("  | ")
    } else {
        print!("{:<4}", chunk.get_line(offset).unwrap_or(0))
    }

    let instruction = match chunk.read_opcode(offset) {
        Some(op) => op,
        None => {
            println!("Unknown opcode, offset: {offset}");
            return offset + 1;
        }
    };

    match instruction.op {
        OpCode::OpReturn => simple_instruction("OP_RETURN", offset),
        OpCode::OpConstant => constant_instruction("OP_CONSTANT", chunk, instruction, offset),
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn constant_instruction(
    name: &str,
    chunk: &Chunk,
    instruction: Instruction,
    offset: usize,
) -> usize {
    print!("{:<16} {:>4} ", name, instruction.operand);
    let constant = chunk.read_constant(&instruction.operand);
    if let Some(v) = constant {
        print_value(v);
        println!();
    } else {
        println!("常量池中未找到目标常量, 指令信息: {instruction:?}");
    }
    offset + instruction.len
}
