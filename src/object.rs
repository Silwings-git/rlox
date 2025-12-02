use std::{fmt::Display, rc::Rc};

use crate::{chunk::Chunk, string_pool::InternedString, value::Value};

#[derive(Debug, Clone)]
pub struct Function {
    // 函数形参数量
    pub arity: u8,
    pub chunk: Chunk,
    pub name: InternedString,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            arity: Default::default(),
            chunk: Default::default(),
            name: InternedString::new("".into()),
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
