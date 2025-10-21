use kinf_vm::{VM, OpCode, Value};

fn main() {
    let mut vm = VM::new();
    
    // Program: (2/1) + (3/1) = 5/1
    let program = vec![
        OpCode::Push(Value::k0(2, 1)),  // Push 2
        OpCode::Push(Value::k0(3, 1)),  // Push 3
        OpCode::Add,                     // Add them
    ];
    
    println!("=== ℵω VM Test ===");
    println!("Program: 2 + 3");
    
    match vm.run(&program) {
        Ok(result) => {
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("Error: {:?}", e);
        }
    }
    
    // Test level up
    let mut vm2 = VM::new();
    let program2 = vec![
        OpCode::Push(Value::k0(1, 1)),   // Push 1
        OpCode::LevelUp,                  // ι(1) → K₁
        OpCode::LevelUp,                  // ι(ι(1)) → K₂
    ];
    
    println!("\n=== Level Up Test ===");
    println!("Program: ι(ι(1))");
    
    match vm2.run(&program2) {
        Ok(result) => {
            println!("Result: {:?}", result);
            println!("Level: {}", result.level());
        }
        Err(e) => {
            eprintln!("Error: {:?}", e);
        }
    }
}

