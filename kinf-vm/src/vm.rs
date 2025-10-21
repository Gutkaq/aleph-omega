use crate::opcode::{OpCode, Value};

/// ℵω Virtual Machine - Stack-based executor
pub struct VM {
    stack: Vec<Value>,
    pc: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::new(),
            pc: 0,
        }
    }
    
    /// Execute bytecode program
    pub fn run(&mut self, code: &[OpCode]) -> Result<Value, VMError> {
        self.pc = 0;
        
        while self.pc < code.len() {
            match &code[self.pc] {
                OpCode::Push(val) => {
                    self.stack.push(val.clone());
                }
                
                OpCode::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }
                
                OpCode::Dup => {
                    let val = self.stack.last().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(val.clone());
                }
                
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(self.add(a, b)?);
                }
                
                OpCode::Mul => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(self.mul(a, b)?);
                }
                
                OpCode::LevelUp => {
                    let val = self.pop()?;
                    self.stack.push(self.level_up(val)?);
                }
                
                OpCode::Node(level) => {
                    // For now, just push a placeholder node
                    self.stack.push(Value::NodeId(self.pc as u64));
                }
                
                OpCode::Edge => {
                    let _b = self.pop()?;
                    let _a = self.pop()?;
                    // For now, edges are no-ops
                }
                
                OpCode::Jump(addr) => {
                    self.pc = *addr;
                    continue;
                }
                
                OpCode::Return => {
                    break;
                }
            }
            
            self.pc += 1;
        }
        
        self.stack.pop().ok_or(VMError::EmptyStack)
    }
    
    fn pop(&mut self) -> Result<Value, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
    
    /// Add two K₀ elements
    fn add(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (a, b) {
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                // a/b + c/d = (ad + bc) / bd
                let num = n1 * d2 + n2 * d1;
                let den = d1 * d2;
                Ok(Value::Rational(num, den))
            }
            _ => Err(VMError::TypeError),
        }
    }
    
    /// Multiply two K₀ elements
    fn mul(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (a, b) {
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
                Ok(Value::Rational(n1 * n2, d1 * d2))
            }
            _ => Err(VMError::TypeError),
        }
    }
    
    /// Lift Kₙ to Kₙ₊₁
    fn level_up(&self, val: Value) -> Result<Value, VMError> {
        let level = val.level();
        Ok(Value::Kernel {
            level: level + 1,
            data: vec![val],
        })
    }
}

#[derive(Debug)]
pub enum VMError {
    StackUnderflow,
    EmptyStack,
    TypeError,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

