use crate::{
    chunk::{Chunk, OpCode, Operand},
    vm::VM,
};

mod chunk;
mod debug;
mod value;
mod vm;

fn main() {
    let mut vm = VM::new();

    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_chunk_op_code(OpCode::Constant, 123);
    chunk.write_chunk_operand(Operand::U8(constant as u8), 123);

    chunk.write_chunk_op_code(OpCode::Negate, 123);

    chunk.write_chunk_op_code(OpCode::Return, 123);

    let res = vm.interpret(chunk);
    match res {
        Ok(_) => println!("run success"),
        Err(err) => match err {
            vm::InterpretError::CompileError => println!("编译错误"),
            vm::InterpretError::RuntimeError(msg) => println!("运行时错误: {msg}"),
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        chunk::{Chunk, OpCode, Operand},
        vm::VM,
    };

    #[test]
    fn test_op() {
        let mut vm = VM::new();

        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(1.2);
        chunk.write_chunk_op_code(OpCode::Constant, 123);
        chunk.write_chunk_operand(Operand::U8(constant as u8), 123);

        let constant = chunk.add_constant(3.4);
        chunk.write_chunk_op_code(OpCode::Constant, 123);
        chunk.write_chunk_operand(Operand::U8(constant as u8), 123);

        chunk.write_chunk_op_code(OpCode::Add, 123);

        let constant = chunk.add_constant(5.6);
        chunk.write_chunk_op_code(OpCode::Constant, 123);
        chunk.write_chunk_operand(Operand::U8(constant as u8), 123);

        chunk.write_chunk_op_code(OpCode::Divide, 123);

        chunk.write_chunk_op_code(OpCode::Negate, 123);

        chunk.write_chunk_op_code(OpCode::Return, 123);

        vm.interpret(chunk).unwrap();
    }
}
