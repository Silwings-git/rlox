use std::{
    fmt::Display,
    hash::{DefaultHasher, Hash, Hasher},
    ops::Add,
    rc::Rc,
};

use crate::vm::InterpretError;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(InternedString),
}

#[derive(Debug, Clone)]
pub struct InternedString {
    str: Rc<str>,
    hash: u64,
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.str, &other.str) || self.str == other.str
    }
}

impl Eq for InternedString {}

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl Add for &InternedString {
    type Output = InternedString;

    fn add(self, rhs: Self) -> Self::Output {
        let combined = format!("{}{}", self.as_str(), rhs.as_str());
        InternedString::new(&combined)
    }
}

impl Add for InternedString {
    type Output = InternedString;

    fn add(self, rhs: Self) -> Self::Output {
        let combined = format!("{}{}", self.as_str(), rhs.as_str());
        InternedString::new(&combined)
    }
}

impl InternedString {
    pub fn new(s: &str) -> Self {
        let hash = {
            let mut hasher = DefaultHasher::new();
            s.hash(&mut hasher);
            hasher.finish()
        };
        Self {
            str: Rc::from(s),
            hash,
        }
    }

    /// 暴露内部字符串的不可变引用
    pub fn as_str(&self) -> &str {
        &self.str
    }
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
