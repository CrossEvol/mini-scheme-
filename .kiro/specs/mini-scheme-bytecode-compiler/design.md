# Design Document

## Overview

This document outlines the design for a bytecode compiler and virtual machine for the MiniScheme language. The system transforms AST nodes into bytecode instructions and executes them using a stack-based virtual machine with support for closures, lexical scoping, and all Scheme data types.

The design consists of three main components:
1. **Object System**: Runtime representation of Scheme values
2. **Bytecode Compiler**: Transforms AST to bytecode instructions
3. **Virtual Machine**: Stack-based interpreter that executes bytecode

## Architecture

### High-Level Architecture

```
AST → Bytecode Compiler → Bytecode → Virtual Machine → Result
                ↓
        Object System (Runtime Values)
```

### Component Interaction

- **AST to Bytecode**: The compiler walks the AST and emits bytecode instructions
- **Object System**: Provides runtime representation for all Scheme values
- **Virtual Machine**: Executes bytecode using a stack-based approach with call frames
- **Closure Support**: Implements lexical scoping through upvalue capture mechanism

## Components and Interfaces

### 1. Object System

The object system provides runtime representation for all Scheme data types with a unified `Value` type and object hierarchy.

#### Core Value Type

```rust
#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(Rc<RefCell<Object>>),
}

#[derive(Debug)]
pub enum Object {
    String(String),
    Character(char),
    Cons(Cons),                 // Fundamental cons cell for lists
    Vector(Vec<Value>),
    Hashtable(HashMap<String, Value>),
    Function(Function),
    Closure(Closure),
    Upvalue(Upvalue),
}

#[derive(Debug)]
pub struct Cons {
    pub car: Value,             // First element
    pub cdr: Value,             // Rest of the list
}
```

#### Function and Closure Objects

```rust
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,           // Contains bytecode instructions
    pub upvalue_count: usize,   // Number of upvalues this function captures
}

#[derive(Debug)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Debug)]
pub struct Upvalue {
    pub location: UpvalueLocation,
}

#[derive(Debug)]
pub enum UpvalueLocation {
    Stack(usize),              // Points to stack slot (open upvalue)
    Closed(Value),             // Contains the closed value (closed upvalue)
}
```

### 2. Bytecode Instructions

Based on the provided instruction set, extended for MiniScheme:

```rust
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    // Constants and literals
    OP_CONSTANT,     // Load constant from constant pool
    OP_NIL,          // Push nil value
    OP_TRUE,         // Push true value
    OP_FALSE,        // Push false value
    
    // Stack operations
    OP_POP,          // Pop top value from stack
    
    // Variable operations
    OP_GET_LOCAL,    // Get local variable by slot index
    OP_SET_LOCAL,    // Set local variable by slot index
    OP_GET_GLOBAL,   // Get global variable by name
    OP_DEFINE_GLOBAL,// Define global variable
    OP_SET_GLOBAL,   // Set global variable by name
    OP_GET_UPVALUE,  // Get upvalue by index
    OP_SET_UPVALUE,  // Set upvalue by index
    
    // Comparison operations
    OP_EQUAL,        // Equality comparison
    OP_GREATER,      // Greater than comparison
    OP_LESS,         // Less than comparison
    
    // Arithmetic operations
    OP_ADD,          // Addition
    OP_SUBTRACT,     // Subtraction
    OP_MULTIPLY,     // Multiplication
    OP_DIVIDE,       // Division
    
    // Unary operations
    OP_NOT,          // Logical not
    OP_NEGATE,       // Arithmetic negation
    
    // I/O operations
    OP_PRINT,        // Print value to stdout
    
    // Control flow
    OP_JUMP,         // Unconditional jump
    OP_JUMP_IF_FALSE,// Conditional jump
    OP_LOOP,         // Loop back jump
    
    // Function operations
    OP_CALL,         // Call function/closure
    OP_CLOSURE,      // Create closure with upvalue capture
    OP_CLOSE_UPVALUE,// Close upvalues when leaving scope
    OP_RETURN,       // Return from function
    
    // Additional MiniScheme operations
    OP_CONS,         // Create cons cell from two stack values
    OP_CAR,          // Get car of cons cell
    OP_CDR,          // Get cdr of cons cell
    OP_VECTOR,       // Create vector from stack values
    OP_MAKE_HASHTABLE, // Create hashtable
}
```

### 3. Bytecode Compiler

The compiler transforms AST nodes into bytecode instructions with proper variable resolution and closure capture.

#### Compiler Structure

```rust
pub struct Compiler {
    pub function: Function,
    pub function_type: FunctionType,
    pub locals: Vec<Local>,
    pub upvalues: Vec<Upvalue>,
    pub scope_depth: usize,
    pub enclosing: Option<Box<Compiler>>,
    pub trace_enabled: bool,     // Enable compilation tracing
}

#[derive(Debug)]
pub struct Local {
    pub name: String,
    pub depth: isize,
    pub is_captured: bool,  // True if captured by a closure
}

#[derive(Debug)]
pub struct CompilerUpvalue {
    pub index: u8,
    pub is_local: bool,     // True if capturing local, false if capturing upvalue
}
```

#### Variable Resolution Strategy

1. **Local Variables**: Resolved by stack slot index within current function
2. **Upvalues**: Resolved through recursive lookup in enclosing scopes
3. **Global Variables**: Resolved by name lookup in global environment

#### Closure Compilation

When compiling a lambda expression:
1. Create new compiler context for the function body
2. Analyze variable references to identify upvalues
3. Mark captured locals in enclosing scopes
4. Generate `OP_CLOSURE` instruction with upvalue metadata
5. Emit upvalue capture information as instruction operands

### 4. Virtual Machine

Stack-based virtual machine with call frame management and upvalue handling.

#### VM Structure

```rust
pub struct VM {
    pub stack: Vec<Value>,
    pub stack_top: usize,
    pub frames: Vec<CallFrame>,
    pub frame_count: usize,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: Vec<Rc<RefCell<Upvalue>>>,  // Sorted by stack address
    pub trace_execution: bool,   // Enable/disable execution tracing
    pub trace_compilation: bool, // Enable/disable compilation tracing
}

#[derive(Debug)]
pub struct CallFrame {
    pub closure: Rc<Closure>,
    pub ip: usize,              // Instruction pointer
    pub slots: usize,           // Base of stack frame
}
```

#### Execution Model

1. **Stack Management**: Values pushed/popped during expression evaluation
2. **Call Frames**: Each function call creates a new frame with local variable slots
3. **Upvalue Management**: Open upvalues tracked in sorted list, closed when leaving scope
4. **Instruction Dispatch**: Main execution loop dispatches based on opcode

## Data Models

### Bytecode Chunk

```rust
#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,           // Bytecode instructions and operands
    pub constants: Vec<Value>,   // Constant pool
    pub lines: Vec<usize>,       // Line number information for debugging
}
```

### Variable Scoping

- **Global Scope**: Variables accessible from anywhere, stored in VM globals table
- **Local Scope**: Variables in current function, accessed by stack slot index
- **Upvalue Scope**: Variables captured from enclosing functions, managed through upvalue objects

### Closure Capture Mechanism

1. **Open Upvalues**: Point directly to stack locations while variables are in scope
2. **Closed Upvalues**: Copy values to heap when variables leave scope
3. **Shared Upvalues**: Multiple closures can share the same upvalue object

## Error Handling

### Compile-Time Errors

- Undefined variable references
- Invalid syntax constructs
- Scope resolution failures
- Type mismatches in special forms

### Runtime Errors

- Stack overflow/underflow
- Undefined variable access
- Type errors in operations
- Invalid function calls
- Memory allocation failures

### Error Reporting

```rust
#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError { expected: String, got: String },
    ArityMismatch { expected: usize, got: usize },
    StackOverflow,
    InvalidOperation(String),
}
```

## Testing Strategy

### Unit Tests

1. **Object System Tests**: Value creation, type checking, memory management
2. **Compiler Tests**: AST to bytecode transformation, variable resolution
3. **VM Tests**: Instruction execution, stack management, call frame handling
4. **Closure Tests**: Upvalue capture, closure creation, variable access

### Integration Tests

1. **End-to-End Compilation**: Complete AST to bytecode to execution pipeline
2. **Scheme Program Tests**: Execute example Scheme programs and verify results
3. **Error Handling Tests**: Verify proper error reporting and recovery
4. **Performance Tests**: Measure execution speed and memory usage

### Test Data

- Use existing example Scheme files as test cases
- Create specific test cases for closure behavior
- Test edge cases like deeply nested closures and recursive functions

## Performance Considerations

### Optimization Strategies

1. **Constant Folding**: Evaluate constant expressions at compile time
2. **Local Variable Optimization**: Use stack slots for local variables
3. **Tail Call Optimization**: Reuse stack frames for tail calls
4. **Upvalue Sharing**: Share upvalue objects between closures when possible

### Memory Management

1. **Reference Counting**: Use `Rc<RefCell<>>` for shared objects
2. **Upvalue Lifecycle**: Automatic transition from open to closed upvalues
3. **Garbage Collection**: Consider implementing mark-and-sweep GC for complex programs

## Implementation Phases

### Phase 1: Basic Object System and VM
- Implement core Value and Object types
- Create basic VM with stack operations
- Support simple arithmetic and comparison operations

### Phase 2: Function Calls and Local Variables
- Add function objects and call frames
- Implement local variable access
- Support basic function calls without closures

### Phase 3: Closure Support
- Implement upvalue objects and capture mechanism
- Add closure creation and upvalue access instructions
- Support lexical scoping and variable capture

### Phase 4: Complete Scheme Features
- Add support for all Scheme data types (lists, vectors, hashtables)
- Implement built-in functions (car, cdr, cons, etc.)
- Add support for special forms (let, cond, etc.)

### Phase 5: Optimization and Error Handling
- Improve error reporting with line numbers
- Add performance optimizations
- Comprehensive testing and debugging

## Debugging and Tracing System

### Compilation Tracing

The compiler provides detailed tracing of the compilation process to help understand how AST nodes are transformed into bytecode.

#### Trace Information

```rust
#[derive(Debug)]
pub struct CompilationTrace {
    pub phase: CompilationPhase,
    pub ast_node: String,           // String representation of AST node
    pub generated_instructions: Vec<(OpCode, Vec<u8>)>,
    pub local_variables: Vec<String>,
    pub upvalues: Vec<String>,
    pub scope_depth: usize,
}

#[derive(Debug)]
pub enum CompilationPhase {
    VariableResolution,
    InstructionGeneration,
    ScopeManagement,
    UpvalueCapture,
    FunctionCompilation,
}
```

#### Trace Output Format

```
[COMPILE] Phase: VariableResolution
  AST: Variable("x")
  Scope Depth: 2
  Resolution: Local(slot=1)
  Generated: OP_GET_LOCAL 1

[COMPILE] Phase: FunctionCompilation  
  AST: Lambda(params=["x"], body=[...])
  Upvalues: ["y" (local=true, index=0)]
  Generated: OP_CLOSURE 0
             0 1  // upvalue 0: local=true, index=1
```

### Runtime Execution Tracing

The VM provides detailed tracing of bytecode execution to help understand program behavior.

#### Execution Trace Information

```rust
#[derive(Debug)]
pub struct ExecutionTrace {
    pub instruction: OpCode,
    pub operands: Vec<u8>,
    pub stack_before: Vec<Value>,
    pub stack_after: Vec<Value>,
    pub current_frame: FrameInfo,
    pub upvalue_states: Vec<UpvalueState>,
}

#[derive(Debug)]
pub struct FrameInfo {
    pub function_name: String,
    pub instruction_pointer: usize,
    pub local_variables: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct UpvalueState {
    pub index: usize,
    pub is_closed: bool,
    pub value: Value,
    pub stack_location: Option<usize>,
}
```

#### Trace Output Format

```
[EXEC] OP_GET_LOCAL 1
  Frame: main@12
  Stack Before: [42, "hello"]
  Stack After:  [42, "hello", 100]
  Local[1]: 100

[EXEC] OP_CLOSURE 0
  Frame: main@15
  Stack Before: [42]
  Stack After:  [42, <closure:lambda>]
  Upvalues: [0: open@stack[2]=100]

[EXEC] OP_CLOSE_UPVALUE
  Frame: main@20
  Stack Before: [42, <closure:lambda>, 100]
  Stack After:  [42, <closure:lambda>]
  Upvalues: [0: closed=100]
```

### Trace Control Interface

```rust
impl VM {
    pub fn enable_execution_trace(&mut self) {
        self.trace_execution = true;
    }
    
    pub fn disable_execution_trace(&mut self) {
        self.trace_execution = false;
    }
    
    pub fn set_trace_filter(&mut self, filter: TraceFilter) {
        // Allow filtering specific instruction types or functions
    }
}

impl Compiler {
    pub fn enable_compilation_trace(&mut self) {
        self.trace_enabled = true;
    }
    
    pub fn trace_compilation_step(&self, phase: CompilationPhase, info: &str) {
        if self.trace_enabled {
            println!("[COMPILE] Phase: {:?}", phase);
            println!("  {}", info);
        }
    }
}
```

### Disassembler

A bytecode disassembler to help understand generated code:

```rust
pub struct Disassembler;

impl Disassembler {
    pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < chunk.code.len() {
            offset = self.disassemble_instruction(chunk, offset);
        }
    }
    
    pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
        print!("{:04} ", offset);
        
        // Print line number
        if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", chunk.lines[offset]);
        }
        
        let instruction = OpCode::from(chunk.code[offset]);
        match instruction {
            OpCode::OP_CONSTANT => self.constant_instruction("OP_CONSTANT", chunk, offset),
            OpCode::OP_GET_LOCAL => self.byte_instruction("OP_GET_LOCAL", chunk, offset),
            OpCode::OP_CLOSURE => self.closure_instruction("OP_CLOSURE", chunk, offset),
            // ... other instructions
        }
    }
}
```

### Integration with Development Workflow

1. **Compilation Debugging**: Enable compilation tracing to see how AST transforms to bytecode
2. **Execution Debugging**: Enable execution tracing to see step-by-step program execution  
3. **Selective Tracing**: Filter traces by function name, instruction type, or scope level
4. **Interactive Debugging**: REPL integration with trace commands
5. **Performance Analysis**: Trace timing information for optimization

### Trace Configuration

```rust
#[derive(Debug)]
pub struct TraceConfig {
    pub compilation: bool,
    pub execution: bool,
    pub stack_operations: bool,
    pub upvalue_operations: bool,
    pub function_calls: bool,
    pub variable_access: bool,
    pub filter_functions: Vec<String>,  // Only trace these functions
    pub max_stack_depth: Option<usize>, // Limit trace output
}
```