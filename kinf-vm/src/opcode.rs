use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Rational(i64, i64),
    K1(Box<Value>),
    K2(Box<Value>),
    Kω(Vec<Value>),
}

impl Value {
    pub fn k0(num: i64, denom: i64) -> Self {
        Value::Rational(num, denom)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OpCode {
    Push(Value),
    Add,
    Mul,
    LevelUp,
}
