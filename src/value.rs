use std::{fmt::Display, slice::Iter};

use crate::{string_pool::InternedString, vm::InterpretError};

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(InternedString),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(r1), Self::Bool(r2)) => r1 == r2,
            (Self::Number(r1), Self::Number(r2)) => r1 == r2,
            (Self::Nil, Self::Nil) => true,
            (Self::String(str1), Self::String(str2)) => str1 == str2,
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

impl From<InternedString> for Value {
    fn from(value: InternedString) -> Self {
        Self::String(value)
    }
}

impl From<&InternedString> for Value {
    fn from(value: &InternedString) -> Self {
        Self::String(value.to_owned())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(str) => write!(f, "{str}"),
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

    pub fn as_string(&self) -> Result<&InternedString, InterpretError> {
        match self {
            Value::String(v) => Ok(v),
            v => Err(InterpretError::RuntimeError(format!(
                "cannot convert to string: {v:?}",
            ))),
        }
    }
}

/// 常量池
#[derive(Debug, Default)]
pub struct ValueArray {
    values: Vec<Value>,
}

impl<'a> IntoIterator for &'a ValueArray {
    type Item = &'a Value;

    type IntoIter = Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray { values: vec![] }
    }

    pub fn write_value_array<T: Into<Value>>(&mut self, value: T) {
        self.values.push(value.into());
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
