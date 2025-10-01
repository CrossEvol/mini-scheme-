use mini_scheme::{VM, Chunk, OpCode, Value};

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Enable tracing to see execution
    vm.enable_trace();

    println!("=== VM Demo: Basic Arithmetic ===");
    
    // Create a program that computes: (5 + 3) * 2 - 1 = 15
    chunk.write_constant(Value::Number(5.0), 1).unwrap();
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_ADD, 1);
    chunk.write_constant(Value::Number(2.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_MULTIPLY, 1);
    chunk.write_constant(Value::Number(1.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_SUBTRACT, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    match vm.interpret(&chunk) {
        Ok(result) => {
            println!("Result: {:?}", result);
            if let Value::Number(n) = result {
                println!("Final value: {}", n);
            }
        }
        Err(e) => {
            println!("Runtime error: {}", e);
        }
    }

    println!("\n=== VM Demo: Global Variables ===");
    
    // Reset VM and create a new program
    vm.reset();
    vm.disable_trace(); // Disable tracing for cleaner output
    let mut chunk2 = Chunk::new();

    // Define a global variable x = 42
    chunk2.write_constant(Value::Number(42.0), 1).unwrap();
    chunk2.add_constant(Value::string("x".to_string()));
    chunk2.write_instruction_with_byte(OpCode::OP_DEFINE_GLOBAL, 1, 1);

    // Get x and add 8 to it: x + 8
    chunk2.write_instruction_with_byte(OpCode::OP_GET_GLOBAL, 1, 1);
    chunk2.write_constant(Value::Number(8.0), 1).unwrap();
    chunk2.write_instruction(OpCode::OP_ADD, 1);
    chunk2.write_instruction(OpCode::OP_RETURN, 1);

    match vm.interpret(&chunk2) {
        Ok(result) => {
            println!("Result of x + 8: {:?}", result);
            if let Value::Number(n) = result {
                println!("Final value: {}", n);
            }
        }
        Err(e) => {
            println!("Runtime error: {}", e);
        }
    }

    println!("\n=== VM Demo: Boolean Logic ===");
    
    // Reset VM and create a boolean logic program
    vm.reset();
    let mut chunk3 = Chunk::new();

    // Compute: !(5 > 10) = true
    chunk3.write_constant(Value::Number(5.0), 1).unwrap();
    chunk3.write_constant(Value::Number(10.0), 1).unwrap();
    chunk3.write_instruction(OpCode::OP_GREATER, 1);
    chunk3.write_instruction(OpCode::OP_NOT, 1);
    chunk3.write_instruction(OpCode::OP_RETURN, 1);

    match vm.interpret(&chunk3) {
        Ok(result) => {
            println!("Result of !(5 > 10): {:?}", result);
            if let Value::Boolean(b) = result {
                println!("Final value: {}", b);
            }
        }
        Err(e) => {
            println!("Runtime error: {}", e);
        }
    }
}