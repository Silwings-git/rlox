use std::fmt::Display;

use crate::vm::InterpretError;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(r1), Self::Bool(r2)) => r1 == r2,
            (Self::Number(r1), Self::Number(r2)) => r1 == r2,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{n}"),
        }
    }
}

impl Value {
    pub fn as_number(&self) -> Result<f64, InterpretError> {
        match self {
            Value::Number(v) => Ok(*v),
            v => Err(InterpretError::RuntimeError(format!(
                "cannot convert to number: {v:?}",
            ))),
        }
    }
}

/// 常量池
#[derive(Debug, Default)]
pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray { values: vec![] }
    }

    pub fn write_value_array(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn get_value(&self, index: usize) -> Option<&Value> {
        self.values.get(index)
    }
}

pub fn print_value(value: &Value) {
    print!("{value}");
}
