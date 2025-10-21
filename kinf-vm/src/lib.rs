//! ℵω Virtual Machine - Stack-based execution for KInf operations

pub mod opcode;
pub mod vm;

pub use opcode::{OpCode, Value};
pub use vm::{VM, VMError};

