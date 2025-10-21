use kinf_vm::opcode::{OpCode, Value};
use kinf_vm::vm::VM;

fn main() {
    let mut vm = VM::new();

    // Example 1: 2 + 3
    let program1 = vec![
        OpCode::Push(Value::k0(2, 1)),
        OpCode::Push(Value::k0(3, 1)),
        OpCode::Add,
    ];

    match vm.run(&program1) {
        Ok(result) => println!("2 + 3 = {:?}", result),
        Err(e) => println!("Error: {}", e),
    }

    // Example 2: Level up 5
    let mut vm2 = VM::new();
    let program2 = vec![
        OpCode::Push(Value::k0(5, 1)),
        OpCode::LevelUp,
    ];

    match vm2.run(&program2) {
        Ok(result) => println!("LevelUp(5) = {:?}", result),
        Err(e) => println!("Error: {}", e),
    }
}
