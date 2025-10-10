use std::{
    env, fs,
    io::{self, Write, stdin},
    path::Path,
    process,
};

use crate::vm::{InterpretError, VM};

mod chunk;
mod compiler;
mod debug;
mod value;
mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(Path::new(&args[1]));
    } else {
        eprintln!("Usage: rlox [path]");
        process::exit(64);
    }

    process::exit(0);
}

fn repl() {
    let stdin = stdin();
    let mut line = String::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        line.clear();
        let _ = stdin.read_line(&mut line);
        println!();
        // 尙未可执行
        VM::new().interpret(&line).unwrap();
    }
}

fn run_file(path: &Path) {
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            println!("Could not open file {}. {}", path.to_str().unwrap(), e);
            process::exit(74);
        }
    };
    let result = VM::new().interpret(&source);
    match result {
        Err(InterpretError::CompileError) => process::exit(65),
        Err(InterpretError::RuntimeError(msg)) => {
            eprintln!("{msg}");
            process::exit(7065);
        }
        _ => {}
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
