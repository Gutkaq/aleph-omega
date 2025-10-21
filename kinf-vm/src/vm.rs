use crate::opcode::{OpCode, Value};

pub struct VM {
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM { stack: Vec::new() }
    }

    pub fn run(&mut self, program: &[OpCode]) -> Result<Value, String> {
        for op in program {
            match op {
                OpCode::Push(val) => self.stack.push(val.clone()),
                OpCode::Add => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(self.add(a, b)?);
                }
                OpCode::Mul => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(self.mul(a, b)?);
                }
                OpCode::LevelUp => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::K1(Box::new(val)));
                }
            }
        }
        self.stack.pop().ok_or("Empty stack".to_string())
    }

    fn add(&self, a: Value, b: Value) -> Result<Value, String> {
        match (a, b) {
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Ok(Value::Rational(n1 * d2 + n2 * d1, d1 * d2))
            }
            _ => Err("Type mismatch in Add".to_string()),
        }
    }

    fn mul(&self, a: Value, b: Value) -> Result<Value, String> {
        match (a, b) {
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Ok(Value::Rational(n1 * n2, d1 * d2))
            }
            _ => Err("Type mismatch in Mul".to_string()),
        }
    }
}
