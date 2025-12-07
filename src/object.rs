use std::{fmt::Display, rc::Rc};

use crate::{chunk::Chunk, string_pool::InternedString, value::Value};

#[derive(Debug, Clone)]
pub struct Function {
    // 函数形参数量
    pub arity: u8,
    pub chunk: Chunk,
    pub name: InternedString,
    pub upvalues: Vec<Upvalue>,
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    // 封闭局部变量的栈槽索引
    pub index: usize,
    //
    pub is_local: bool,
}

#[derive(Debug)]
pub struct UpvalueObj {
    location: usize,
}

impl Display for UpvalueObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

impl Default for Function {
    fn default() -> Self {
        Self {
            arity: Default::default(),
            chunk: Default::default(),
            name: InternedString::new("".into()),
            upvalues: vec![],
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.as_str().is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", self.name)
        }
    }
}

pub enum FunctionType {
    Function,
    Script,
}

impl Function {
    pub fn new(funtion_name: &str) -> Self {
        Function {
            arity: Default::default(),
            chunk: Default::default(),
            name: InternedString::new(funtion_name.into()),
            upvalues: vec![],
        }
    }
}

pub type NativeFn = fn(Vec<Value>) -> Value;

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Rc<Function>,
}

impl Closure {
    pub fn new(function: Rc<Function>) -> Self {
        Closure { function }
    }
}
