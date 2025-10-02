# MiniScheme Tracing and Debugging System

The MiniScheme bytecode compiler includes a comprehensive tracing and debugging system that allows you to monitor both compilation and execution phases.

## Features

- **Compilation Tracing**: Track AST to bytecode transformation
- **Execution Tracing**: Monitor VM instruction execution step-by-step
- **Configurable Filtering**: Filter by functions, instructions, or compilation phases
- **Multiple Output Formats**: Compact or verbose trace output
- **Memory Management**: Configurable trace limits to prevent memory issues

## Basic Usage

### Compilation Tracing

```rust
use mini_scheme::{Compiler, TraceConfig, Tracer};

let mut compiler = Compiler::new_script();
let tracer = Tracer::new(TraceConfig::compilation_only());
compiler.set_tracer(tracer);

// Compile your expressions - tracing output will be printed
compiler.compile_expr(&expr)?;
```

### Execution Tracing

```rust
use mini_scheme::{VM, TraceConfig, Tracer};

let mut vm = VM::new();
let tracer = Tracer::new(TraceConfig::execution_only());
vm.set_tracer(tracer);

// Execute bytecode - tracing output will be printed
vm.interpret(&chunk)?;
```

### Combined Tracing

```rust
use mini_scheme::{TraceConfig, Tracer};

let config = TraceConfig::all_enabled();
let tracer = Tracer::new(config);

// Use the same tracer for both compiler and VM
compiler.set_tracer(tracer.clone());
vm.set_tracer(tracer);
```

## Configuration Options

### TraceConfig

The `TraceConfig` struct provides fine-grained control over tracing behavior:

```rust
let mut config = TraceConfig::default();

// Enable/disable different trace types
config.compilation = true;
config.execution = true;
config.stack_operations = true;
config.upvalue_operations = true;
config.function_calls = true;
config.variable_access = true;

// Configure output format
config.compact_format = true;  // Single-line output
config.show_line_numbers = true;
config.show_addresses = true;

// Set limits
config.max_stack_depth = Some(10);  // Limit stack display
config.max_trace_count = Some(1000); // Limit memory usage
```

### Filtering

#### Function Filtering

```rust
let mut tracer = Tracer::new(TraceConfig::all_enabled());

// Only trace specific functions
tracer.add_function_filter("my_function".to_string());
tracer.set_function_filter(vec!["func1".to_string(), "func2".to_string()]);

// Exclude specific functions
tracer.exclude_function("internal_helper".to_string());
```

#### Instruction Filtering

```rust
// Only trace specific instructions
tracer.add_instruction_filter("OP_CALL".to_string());
tracer.add_instruction_filter("OP_RETURN".to_string());

// Exclude noisy instructions
tracer.exclude_instruction("OP_POP".to_string());
```

#### Compilation Phase Filtering

```rust
use mini_scheme::CompilationPhase;

// Only trace specific compilation phases
tracer.add_phase_filter(CompilationPhase::VariableResolution);
tracer.add_phase_filter(CompilationPhase::UpvalueCapture);

// Exclude phases
tracer.exclude_phase(CompilationPhase::InstructionGeneration);
```

## Predefined Configurations

### Compact Tracing

```rust
let config = TraceConfig::compact();
// Single-line output, minimal information
```

### Debug Tracing

```rust
let config = TraceConfig::debug();
// Maximum verbosity, all information included
```

### Compilation Only

```rust
let config = TraceConfig::compilation_only();
// Only trace compilation phases
```

### Execution Only

```rust
let config = TraceConfig::execution_only();
// Only trace VM execution
```

## Output Formats

### Verbose Format (Default)

```
[COMPILE] Phase: VariableResolution
  Function: main
  AST: Variable("x")
  Scope Depth: 0
  Generated:
    OP_GET_GLOBAL 0

[EXEC] OP_GET_GLOBAL 0
  Frame: main@5
  Stack Before: []
  Stack After:  [42]
  Locals:
    arg_0: 42
```

### Compact Format

```
[COMPILE] VariableResolution main Variable("x") -> OP_GET_GLOBAL(0)
[EXEC] OP_GET_GLOBAL(0) main [] -> [42]
```

## Trace Statistics

```rust
let stats = tracer.get_stats();
println!("Compilation traces: {}", stats.compilation_trace_count);
println!("Execution traces: {}", stats.execution_trace_count);
println!("Total traces: {}", stats.total_traces);
```

## Exporting Traces

```rust
// Export all traces to a string
let trace_output = tracer.export_traces();
std::fs::write("traces.txt", trace_output)?;
```

## Performance Considerations

- Tracing adds overhead to compilation and execution
- Use filtering to reduce noise and improve performance
- Set `max_trace_count` to prevent excessive memory usage
- Use compact format for high-frequency tracing
- Disable tracing in production builds

## Example: Debugging Closure Compilation

```rust
use mini_scheme::{TraceConfig, Tracer, CompilationPhase};

let mut config = TraceConfig::compilation_only();
config.filter_phases = vec![
    CompilationPhase::UpvalueCapture,
    CompilationPhase::FunctionCompilation,
];

let tracer = Tracer::new(config);
compiler.set_tracer(tracer);

// Compile a lambda with closures - only upvalue and function traces will show
```

This tracing system provides powerful debugging capabilities for understanding how MiniScheme programs are compiled and executed.