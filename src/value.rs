pub type Value = f64;

/// 常量池
#[derive(Debug)]
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
