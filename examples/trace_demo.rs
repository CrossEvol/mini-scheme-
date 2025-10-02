use mini_scheme::{Compiler, VM, TraceConfig, Tracer, Expr};

fn main() {
    println!("=== MiniScheme Tracing System Demo ===\n");

    // Create a simple expression to compile and execute
    let expr = Expr::Number(42.0);

    // Test compilation tracing
    println!("1. Testing Compilation Tracing:");
    let mut compiler = Compiler::new_script();
    let tracer = Tracer::new(TraceConfig::compilation_only());
    compiler.set_tracer(tracer);
    
    match compiler.compile_expr(&expr) {
        Ok(_) => println!("   Compilation successful!"),
        Err(e) => println!("   Compilation error: {}", e),
    }

    let function = compiler.end_compiler();
    println!("   Generated {} bytes of bytecode\n", function.chunk.code.len());

    // Test execution tracing
    println!("2. Testing Execution Tracing:");
    let mut vm = VM::new();
    let exec_tracer = Tracer::new(TraceConfig::execution_only());
    vm.set_tracer(exec_tracer);

    match vm.interpret(&function.chunk) {
        Ok(result) => println!("   Execution result: {}", format_value(&result)),
        Err(e) => println!("   Execution error: {}", e),
    }

    // Test combined tracing with filtering
    println!("\n3. Testing Combined Tracing with Compact Format:");
    let mut combined_config = TraceConfig::all_enabled();
    combined_config.compact_format = true;
    combined_config.max_stack_depth = Some(5);
    
    let combined_tracer = Tracer::new(combined_config);
    
    let mut compiler2 = Compiler::new_script();
    compiler2.set_tracer(combined_tracer.clone());
    
    let expr2 = Expr::Call(
        Box::new(Expr::Variable("add".to_string())),
        vec![Expr::Number(1.0), Expr::Number(2.0)]
    );
    
    match compiler2.compile_expr(&expr2) {
        Ok(_) => println!("   Complex expression compiled successfully!"),
        Err(e) => println!("   Compilation error: {}", e),
    }

    // Show trace statistics
    if let Some(tracer) = compiler2.tracer() {
        let stats = tracer.get_stats();
        println!("\n4. Trace Statistics:");
        println!("   Compilation traces: {}", stats.compilation_trace_count);
        println!("   Execution traces: {}", stats.execution_trace_count);
        println!("   Total traces: {}", stats.total_traces);
    }

    println!("\n=== Demo Complete ===");
}

fn format_value(value: &mini_scheme::Value) -> String {
    match value {
        mini_scheme::Value::Number(n) => n.to_string(),
        mini_scheme::Value::Boolean(b) => if *b { "#t".to_string() } else { "#f".to_string() },
        mini_scheme::Value::Nil => "nil".to_string(),
        _ => "<object>".to_string(),
    }
}