use std::collections::HashMap;
use std::rc::Rc;

use crate::chunk::{Chunk, Instruction};
use crate::chunk::{OpCode, Operand};
use crate::compiler::Parser;
use crate::config::VMConfig;
use crate::object::Function;
use crate::string_pool::{InternedString, StringPool};
use crate::value::{Value, print_value};

macro_rules! binary_op {
    ($vm:expr, $value_type:path, $op:tt) => {{
        let b = $vm.peek(0).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;
        let a = $vm.peek(1).and_then(|v|v.as_number().ok()).ok_or_else(||$vm.runtime_error("Operand must be a number."))?;

        $vm.pop().unwrap();
        $vm.pop().unwrap();

        $vm.push($value_type(a $op b))?;
    }};
}

type Table = HashMap<InternedString, Value>;

pub struct VM {
    // 调用栈
    frames: CallStack,
    stack: Stack,
    globals: Table,
    strings: StringPool,
}

struct CallStack {
    inner: Vec<CallFrame>,
    max_depth: usize,
    // 当前CallFrame栈的高度(正在进行函数调用的数量)
    frame_count: usize,
}

impl CallStack {
    fn new(max_depth: usize) -> Self {
        CallStack {
            inner: Vec::with_capacity(max_depth),
            max_depth,
            frame_count: 0,
        }
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        self.inner.last_mut().unwrap()
    }

    fn reset(&mut self) {
        // todo 是否需要把call frame也重置?
        self.frame_count = 0;
    }

    fn push(&mut self, function: CallFrame) {
        self.inner.push(function)
    }
}

struct Stack {
    inner: Vec<Value>,
    max_depth: usize,
}

impl Stack {
    fn new(max_depth: usize) -> Self {
        Self {
            inner: vec![],
            max_depth,
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        if self.inner.len() > distance {
            Some(&self.inner[self.inner.len() - 1 - distance])
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<Value> {
        self.inner.pop()
    }

    fn popn(&mut self, n: usize) -> Result<Vec<Value>, InterpretError> {
        if n == 0 {
            return Ok(vec![]);
        }
        if n > self.inner.len() {
            return Err(InterpretError::RuntimeError(format!(
                "insufficient stack elements: need {}, got {}",
                n,
                self.inner.len()
            )));
        }
        let split_idx = self.inner.len().saturating_sub(n);

        let popped = self.inner.split_off(split_idx);

        Ok(popped)
    }

    fn push<T: Into<Value>>(&mut self, v: T) -> Result<(), InterpretError> {
        if self.inner.len() > self.max_depth {
            return Err(InterpretError::RuntimeError(format!(
                "StackOverflow: max depth {} exceeded",
                self.max_depth
            )));
        }
        self.inner.push(v.into());
        Ok(())
    }

    fn reset_stack(&mut self) {
        self.inner.clear();
    }

    fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    fn get_by_index(&self, index: usize) -> Option<&Value> {
        self.inner.get(index)
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn set_by_index(&mut self, index: usize, v: Value) {
        self.inner[index] = v;
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError(String),
}

/// 代表正在进行的函数调用
struct CallFrame {
    // 被调用函数
    function: Rc<Function>,
    // 当前函数执行到的ip
    ip: usize,
    // 指向虚拟机的值栈中该函数使用的第一个槽
    slot: usize,
}
impl CallFrame {
    pub fn new(function: Rc<Function>) -> Self {
        Self {
            function,
            ip: 0,
            slot: 0,
        }
    }

    fn read_byte(&mut self) -> Result<Instruction, InterpretError> {
        let instruction = self
            .function
            .chunk
            .read_opcode(self.ip)
            .ok_or_else(|| InterpretError::RuntimeError("No next opcode".to_string()))?;
        self.ip += instruction.len;
        Ok(instruction)
    }

    fn read_string(&self, operand: &Operand) -> Option<&Value> {
        self.function.chunk.read_constant(operand)
    }

    fn read_constant(&self, operand: &Operand) -> Result<&Value, InterpretError> {
        self.function
            .chunk
            .read_constant(operand)
            .ok_or_else(|| InterpretError::RuntimeError("Cannot find the constant".to_string()))
    }
}

impl VM {
    pub fn new(config: VMConfig) -> Self {
        VM {
            frames: CallStack::new(config.max_frames_depth),
            stack: Stack::new(config.max_stack_depth),
            globals: HashMap::new(),
            strings: Default::default(),
        }
    }

    fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.peek(distance)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
    fn popn(&mut self, n: usize) -> Result<Vec<Value>, InterpretError> {
        self.stack.popn(n)
    }

    fn push<T: Into<Value>>(&mut self, v: T) -> Result<(), InterpretError> {
        self.stack.push(v)
    }

    fn reset_stack(&mut self) {
        self.stack.reset_stack();
        self.frames.reset();
    }

    fn intern_interned(&mut self, s: &InternedString) -> InternedString {
        self.strings.intern_interned(s)
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let parser = Parser::new(source);

        let function = match parser.compile() {
            Some(func) => func,
            None => return Err(InterpretError::CompileError),
        };

        self.push(Value::Function(function.clone()))?;

        let frame = CallFrame::new(function.clone());
        self.frames.push(frame);

        self.intern_chunk_constants(&function.chunk);

        self.run()
    }

    fn print_stack(&self) {
        {
            print!("          ");
            if !self.stack.is_empty() {
                for v in self.stack.inner.iter().rev() {
                    print!("[ ");
                    print_value(v);
                    print!(" ]");
                }
            }
            println!();
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            #[cfg(feature = "debug")]
            {
                self.print_stack();
                let frame = self.current_frame();
                use crate::debug::disassemble_instruction;
                disassemble_instruction(&frame.function.chunk, frame.ip);
            }
            let instruction = self.current_frame().read_byte()?;
            match instruction.op {
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.current_frame().read_constant(&instruction.operand)?.clone();
                    self.push(constant)?
                }
                OpCode::Negate => {
                    let number = self
                        .peek(0)
                        .and_then(|v| v.as_number().ok())
                        .ok_or_else(|| self.runtime_error("Operand must be a number."))?;
                    self.pop().unwrap();
                    self.push(-number)?;
                }
                OpCode::Add => {
                    let b = self.peek(0).cloned();
                    let a = self.peek(1).cloned();
                    match (b, a) {
                        (Some(Value::String(s2)), Some(Value::String(s1))) => {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            // 动态字符串不驻留
                            self.push(s1 + s2)?
                        }
                         (Some(Value::String(n2)), Some(v1)) => {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            self.push(InternedString::new(v1.to_string().into()) + n2)?
                        }
                        (Some(v2), Some(Value::String(s1)) )=> {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            self.push(s1 + InternedString::new(v2.to_string().into()) )?
                        }
                        (Some(Value::Number(n2)), Some(Value::Number(n1))) => {
                            self.pop().unwrap();
                            self.pop().unwrap();
                            self.push(n1 + n2)?
                        }
                        _ => {
                            return Err(
                                        self.runtime_error("Operands must be two numbers, or at least one string.")
                                    );
                            }
                    }
                }
                OpCode::Subtract => binary_op!(self,Value::Number,-),
                OpCode::Multiply => binary_op!(self,Value::Number,*),
                OpCode::Divide => binary_op!(self,Value::Number,/),
                OpCode::True => self.push(true)?,
                OpCode::False => self.push(false)?,
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::Not => {
                    // `false` and `nil` are false, and all other values are true.
                    let pop = match self.pop().unwrap() {
                        Value::Bool(b) => b,
                        Value::Nil => false,
                        _=>true,
                    };
                    self.push(!pop)?;
                }
                OpCode::Equal => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a.eq(&b))?;
                }
                OpCode::Gerater => binary_op!(self,Value::Bool,>),
                OpCode::Less => binary_op!(self,Value::Bool,<),
                OpCode::Print => {
                    print_value(
                        &self
                            .pop()
                            .ok_or_else(|| InterpretError::RuntimeError("print: stack is empty".into()))?,
                    );
                    println!();
                    Ok(())?
                }
                OpCode::Pop => {
                    self.pop()
                        .ok_or_else(|| InterpretError::RuntimeError("pop: stack is empty".into()))?;
                }
                OpCode::Popn => {
                    if let Operand::U8(n) = &instruction.operand {
                        self.popn(*n as usize)?;
                    } else {
                        return Err(self.runtime_error("popn: operand is empty."));
                    }
                }
                OpCode::DefineGlobal => {
                    let name = self.current_frame().read_string(&instruction.operand).ok_or_else(
                       || InterpretError::RuntimeError(
                            "Bytecode error: The operand of the DefineGlobal directive does not correspond to the string in the constant pool".into(),
                        ),
                    )?
                    .as_string()?
                    .to_owned();
                    self.globals.insert(
                        name,
                        // 允许全局变量声明为nil
                        self.peek(0).cloned().unwrap_or(Value::Nil),
                    );
                    self.pop().ok_or_else(||InterpretError::RuntimeError(
                        "define global: stack is empty".into(),
                    ))?;
                }
                OpCode::GetGlobal => {
                    let name = self.current_frame()
                        .read_string(&instruction.operand)
                        .ok_or_else(||InterpretError::RuntimeError(
                            "Failed to read global variable name from constants.".into(),
                        ))?
                        .as_string()?
                        .to_owned();

                    let value = self.globals.get(&name);
                    match value {
                        Some(v) => {
                            self.push(v.clone())?;
                        }
                        None => {
                            return Err(self
                                .runtime_error(&format!("Undefined variable {}", &name.as_str())));
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = self.current_frame().read_string(&instruction.operand).ok_or_else(
                      ||  InterpretError::RuntimeError(
                            "Failed to read global variable name from constants.".into(),
                        ),
                    )?;
                    let name = name.as_string()?.to_owned();
                    if self.globals.contains_key(&name) {
                        self.globals
                            .insert(name.to_owned(), self.peek(0).cloned().unwrap_or(Value::Nil));
                    } else {
                        // 如果变量未声明,返回错误
                        return Err(
                            self.runtime_error(&format!("Undefined variable {}", name.as_str()))
                        );
                    }
                }
                OpCode::GetLocal => match instruction.operand {
                    Operand::U8(index) => {
                        let slot = self.current_frame().slot;
                        let v = self
                            .stack
                            .get_by_index(slot + index as usize)
                            .ok_or_else(||
                                 InterpretError::RuntimeError(format!("GetLocal instruction: index {} is out of bounds for stack (length: {})", index, self.stack.len())))?;
                        self.push(v.clone())?;
                    }
                    Operand::U16(_) => {
                        return Err(self.runtime_error("Invalid operand."));
                    }
                    Operand::None => {
                        return Err(self.runtime_error("The instruction requires an index operand, but no operand was provided."));
                    }
                },
                OpCode::SetLocal => match instruction.operand {
                    Operand::U8(index) => {
                        let slot = self.current_frame().slot;
                        let v = self.peek(0).cloned().unwrap_or(Value::Nil);
                        self.stack.set_by_index(slot + index as usize, v);
                    }
                    Operand::U16(_) => {
                        return Err(self.runtime_error("Invalid operand."));
                    }
                    Operand::None => {
                        return Err(self.runtime_error("The instruction requires an index operand, but no operand was provided."));
                    }
                },
                OpCode::JumpIfFalse => match instruction.operand {
                    Operand::None => return Err(self.runtime_error("Illegal operand for JumpIfFalse: Operand::None is not supported (expected U16 offset)")),
                    Operand::U8(u) => return Err(self.runtime_error(&format!("Illegal operand type for JumpIfFalse: U8 is not allowed (requires U16 offset, current value: {u:?})"))),
                    Operand::U16(offset) => {
                        if self
                            .peek(0)
                            .filter(|&v| matches!(v,Value::Bool(b) if *b))
                            .is_none()
                        {
                            self.current_frame().ip += offset as usize;
                        }
                    }
                },
                OpCode::Jump=>{
                    match instruction.operand {
                        Operand::None => return Err(self.runtime_error("Illegal operand for Jump: Operand::None is not supported (expected U16 offset)")),
                        Operand::U8(u) => return Err(self.runtime_error(&format!("Illegal operand type for Jump: U8 is not allowed (requires U16 offset, current value: {u:?})"))),
                        Operand::U16(offset) => self.current_frame().ip += offset as usize,
                    }
                },
                OpCode::Loop=>{
                    match instruction.operand {
                        Operand::None => return Err(self.runtime_error("Illegal operand for Loop: Operand::None is not supported (expected U16 offset)")),
                        Operand::U8(u) => return Err(self.runtime_error(&format!("Illegal operand type for Loop: U8 is not allowed (requires U16 offset, current value: {u:?})"))),
                        Operand::U16(offset) => self.current_frame().ip -= offset as usize,
                    }
                },
                OpCode::Call=>{
                    match instruction.operand {
                        Operand::U8(arg_count) => {
                            let callee = self.peek(arg_count as usize).cloned();
                            if !self.call_value(callee,arg_count) {
                                return Err(self.runtime_error("Invalid parameter list"));
                            }    
                        },
                        Operand::U16(_)=>return Err(self.runtime_error("Illegal operand for Call: Operand::U16 is not supported (expected U8 offset)")),
                        Operand::None=>return Err(self.runtime_error("Illegal operand for Call: Operand::None is not supported (expected U8 offset)"))
                    }
                }
            }
        }
    }

    fn call_value(&mut self,callee:Option<Value>,arg_count: u8)->bool{
        if let Some(callee) = callee {
            match callee {
                Value::Function(function) => self.call(function.clone(),arg_count),
                _=>false
            }
        }else {
            false
        }
    }

    fn call(&mut self,function: Rc<Function>,arg_count: u8)->bool{
        let code_len =function.chunk.code_len();
            // 我们让窗口提前一个槽开始，以使它们与实参对齐
            // 例如sum(5,6,7):
            /*
                    rame->slot       stack_top(stack.len)
                         ^                 ^
                         | -1  | -arg_count|
                         |-----|-----------|          
            0        1   2     3   4   5   6     7     
            +--------+---+-----+---+---+---+-----+-----
            | script | 4 | sum | 5 | 6 | 7 | ... | ... 
            +--------+---+-----+---+---+---+-----+-----
                         ^                 ^
                         |-----------------| 
                           sum() CallFrame
             */
        let frame = CallFrame{
            function,
            ip:code_len,
            slot:self.stack.len()-arg_count as usize-1,
        };

        self.frames.push(frame);
        true
    }

    fn runtime_error(&mut self, arg: &str) -> InterpretError {
        let frame = self.current_frame();
        let line = frame.function.chunk.get_line(frame.ip - 1).unwrap_or(1);
        self.reset_stack();
        InterpretError::RuntimeError(format!("[line {line}] in script: {arg}"))
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        self.frames.current_frame()
    }

    fn intern_chunk_constants(&mut self, chunk: &Chunk) {
        for ele in chunk.constants().into_iter() {
            if let Value::String(s) = ele {
                self.intern_interned(s);
            }
        }
    }
}
