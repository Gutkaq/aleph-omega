use serde::{Deserialize, Serialize};

/// Bytecode instructions for the ℵω VM
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum OpCode {
    // Stack operations
    Push(Value),
    Pop,
    Dup,
    
    // KInf arithmetic (K₀ level)
    Add,
    Mul,
    
    // Level operations
    LevelUp,      // ι: Kₙ → Kₙ₊₁
    
    // Graph operations
    Node(u32),    // Create node at level n
    Edge,         // Connect top two nodes
    
    // Control flow
    Jump(usize),
    Return,
}

/// Values on the VM stack
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Value {
    /// Rational number (numerator, denominator)
    Rational(i64, i64),
    
    /// KInf element at level n
    Kernel { level: u32, data: Vec<Value> },
    
    /// Node ID in computation graph
    NodeId(u64),
}

impl Value {
    /// Create K₀ from rational
    pub fn k0(num: i64, den: i64) -> Self {
        Value::Rational(num, den)
    }
    
    /// Get level of KInf element
    pub fn level(&self) -> u32 {
        match self {
            Value::Rational(_, _) => 0,
            Value::Kernel { level, .. } => *level,
            Value::NodeId(_) => 0,
        }
    }
}

