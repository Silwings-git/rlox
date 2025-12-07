use crate::{
    chunk::{Chunk, Instruction, OpCode, Operand},
    object::{Function, Upvalue},
    value::{Value, print_value},
};

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");
    println!("{:4} {:4} {:16} Index Constvalue", "IP", "Line", "OPCode",);
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset = disassemble_instruction(chunk, offset);
    }
    println!()
}

pub fn disassemble_instruction(chunk: &Chunk, mut offset: usize) -> usize {
    print!("{offset:04} ");
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("{:>4} ", "|")
    } else {
        print!("{:>4} ", chunk.get_line(offset).unwrap_or(0))
    }

    let instruction = match chunk.read_opcode(offset) {
        Some(op) => op,
        None => {
            println!("Unknown opcode, offset: {offset}");
            return offset + 1;
        }
    };

    offset += instruction.len;

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
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", &instruction, offset, true),
        OpCode::Jump => jump_instruction("OP_JUMP", &instruction, offset, true),
        OpCode::Loop => jump_instruction("OP_LOOP", &instruction, offset, false),
        OpCode::Call => byte_instruction("OP_CALL", &instruction),
        OpCode::Closure => closure_instruction("OP_CLOSURE", chunk, &instruction, offset),
        OpCode::GetUpvalue => byte_instruction("OP_GET_UPVALUE", &instruction),
        OpCode::SetUpvalue => byte_instruction("OP_SET_UPVALUE", &instruction),
    }

    offset
}

fn closure_instruction(name: &str, chunk: &Chunk, instruction: &Instruction, offset: usize) {
    let operand = match instruction.operand {
        Operand::None => 0,
        Operand::U8(u) => u as usize,
        Operand::U16(u) => u as usize,
    };
    print!("{name:16} {operand:4}");
    let constant = chunk.read_constant(&instruction.operand).unwrap();
    print_value(constant);
    println!();
    if let Value::Function(f) = constant {
        for &Upvalue { index, is_local } in &f.upvalues {
            println!(
                "{:04}      |                     {} {}\n",
                offset - 2,
                if is_local { "local" } else { "upvalue" },
                index
            );
        }
    }
}

fn jump_instruction(name: &str, instruction: &Instruction, offset: usize, add: bool) {
    let operand = match instruction.operand {
        Operand::None => 0,
        Operand::U8(u) => u as usize,
        Operand::U16(u) => u as usize,
    };

    let target = if add {
        offset + operand
    } else {
        offset - operand
    };

    // 输出时起跳位置使用当前操作码的位置,所以需要`-instruction.len`
    println!("{:<16} {:4} -> {}", name, offset - instruction.len, target)
}

fn byte_instruction(name: &str, instruction: &Instruction) {
    let slot = match instruction.operand {
        Operand::U8(slot) => slot as usize,
        Operand::U16(slot) => slot as usize,
        Operand::None => 0,
    };
    // println!("{name} {slot:4}");
    println!("{name:<16} {slot} ");
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
