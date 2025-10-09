use crate::{
    chunk::{Chunk, OpCode, Operand},
    debug::disassemble_chunk,
};

mod chunk;
mod debug;
mod value;

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_chunk_op_code(OpCode::OpConstant, 123);
    chunk.write_chunk_operand(Operand::U8(constant as u8), 123);

    chunk.write_chunk_op_code(OpCode::OpReturn, 123);
    disassemble_chunk(&chunk, "test chunk");
}
