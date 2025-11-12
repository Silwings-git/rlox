use crate::{chunk::Chunk, string_pool::InternedString};

#[derive(Debug, Clone)]
pub struct Function {
    arity: usize,
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

pub enum FunctionType {
    Function,
    Script
}


