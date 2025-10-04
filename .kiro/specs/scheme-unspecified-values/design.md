# Design Document

## Overview

This design implements proper handling of "unspecified values" in the MiniScheme interpreter. Unspecified values are returned by expressions that are executed primarily for side effects (like `define`, `set!`, `display`) rather than to produce meaningful data. The key insight is that these values should not be printed by the REPL, distinguishing them from actual data values like `'()` which should be displayed.

The implementation extends the existing object system with a new `Unspecified` variant, modifies the VM to handle unspecified values correctly, updates the compiler to generate unspecified values for appropriate expressions, and adjusts the REPL behavior to suppress printing of unspecified values.

## Architecture

### Core Components

1. **Object System Extension** (`src/object.rs`)
   - Add `Unspecified` variant to the `Value` enum
   - Implement type checking and conversion methods
   - Update display formatting and equality comparison

2. **Virtual Machine Updates** (`src/vm.rs`)
   - Modify REPL result handling to detect unspecified values
   - Update built-in function implementations to return unspecified values
   - Ensure proper error handling when unspecified values are misused

3. **Compiler Integration** (`src/compiler.rs`)
   - Generate unspecified values for side-effect expressions
   - Handle conditional expressions without else clauses
   - Maintain existing bytecode instruction set (no new opcodes needed)

4. **REPL Behavior** (`src/main.rs`)
   - Suppress output for unspecified values
   - Maintain normal printing for all other values including `'()`
   - Preserve existing debugging and tracing functionality

### Data Flow

```
Source Code → Lexer → Parser → AST → Compiler → Bytecode → VM → Result
                                         ↓
                              Side-effect expressions generate
                              unspecified values instead of nil
                                         ↓
                                    REPL checks result type
                                         ↓
                              Unspecified: no output
                              Other values: normal display
```

## Components and Interfaces

### 1. Value Type Extension

**Location**: `src/object.rs`

```rust
#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Unspecified,  // New variant
    Object(Rc<RefCell<Object>>),
    MultipleValues,
}
```

**Key Methods**:
- `Value::unspecified()` - Constructor for unspecified values
- `Value::is_unspecified()` - Type checking predicate
- Updated `Display` implementation to show `#<unspecified>` for debugging
- Updated `PartialEq` implementation for proper equality semantics

### 2. Compiler Modifications

**Location**: `src/compiler.rs`

**Key Changes**:
- `compile_define()` - Return unspecified value instead of nil
- `compile_set()` - Return unspecified value for assignments
- `compile_if()` - Return unspecified value when no else clause and condition is false
- `compile_import()` - Return unspecified value for import statements

**Implementation Strategy**:
```rust
// Instead of emitting OP_NIL for side-effect operations
self.emit_byte(OpCode::OP_NIL, 1);

// Emit a constant unspecified value
let unspecified_constant = self.add_constant(Value::Unspecified)?;
self.emit_bytes(OpCode::OP_CONSTANT, unspecified_constant as u8, 1);
```

### 3. Built-in Function Updates

**Location**: `src/vm.rs`

**Functions to Update**:
- `display` - Output value and return unspecified
- `write` - Output value and return unspecified  
- `newline` - Output newline and return unspecified
- Future I/O functions

**Implementation Pattern**:
```rust
// In builtin function handling
"display" => {
    let value = self.pop()?;
    print!("{}", value);
    self.output_produced = true;
    self.push(Value::Unspecified)?;  // Return unspecified instead of nil
    Ok(())
}
```

### 4. REPL Integration

**Location**: `src/main.rs`

**Key Changes**:
- Update `process_input_with_repl_vm_result()` to check for unspecified values
- Suppress output only for unspecified values
- Maintain existing behavior for all other values including `nil`

**Implementation**:
```rust
.map(|result| {
    if result.is_unspecified() {
        // Don't print anything for unspecified values
        return;
    }
    
    if result.is_multiple_values() {
        // Handle multiple values as before
        for value in vm.get_multiple_values() {
            println!("{}", value);
        }
        vm.clear_multiple_values_buffer();
    } else {
        // Print all other values, including nil and empty list
        if show_tokens || show_ast || show_bytecode {
            println!("Result: {}", result);
        } else {
            println!("{}", result);
        }
    }
})?;
```

## Data Models

### Unspecified Value Representation

The unspecified value is represented as a unit variant in the `Value` enum:

```rust
Value::Unspecified
```

**Properties**:
- **Size**: Zero additional memory overhead (unit variant)
- **Equality**: All unspecified values are equal to each other
- **Display**: Shows `#<unspecified>` in debug contexts, nothing in REPL
- **Type Safety**: Cannot be used in arithmetic or other inappropriate operations

### Integration with Existing Types

**Relationship to Nil**:
- `Value::Nil` represents the empty list `()` - a legitimate data value
- `Value::Unspecified` represents "no meaningful return value" - a meta-value
- These are distinct and serve different purposes

**Relationship to MultipleValues**:
- `Value::MultipleValues` is used for the multiple values mechanism
- `Value::Unspecified` is orthogonal and can coexist
- An expression could theoretically return multiple unspecified values (though this is not standard)

## Error Handling

### Type Errors for Misuse

When unspecified values are used inappropriately, the system generates clear error messages:

```rust
// In arithmetic operations
if value.is_unspecified() {
    return Err(RuntimeError::TypeError {
        expected: "number".to_string(),
        got: "unspecified value".to_string(),
    });
}

// In function calls
if function.is_unspecified() {
    return Err(RuntimeError::TypeError {
        expected: "callable".to_string(),
        got: "unspecified value".to_string(),
    });
}
```

### Error Message Consistency

All error messages involving unspecified values use the term "unspecified value" for clarity:

```rust
impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Unspecified => "unspecified value",
            // ... other cases
        }
    }
}
```

## Testing Strategy

### Unit Tests

**Object System Tests** (`src/object.rs`):
```rust
#[test]
fn test_unspecified_value_creation() {
    let unspecified = Value::Unspecified;
    assert!(unspecified.is_unspecified());
    assert!(!unspecified.is_nil());
}

#[test]
fn test_unspecified_equality() {
    let unspecified1 = Value::Unspecified;
    let unspecified2 = Value::Unspecified;
    assert_eq!(unspecified1, unspecified2);
    assert_ne!(unspecified1, Value::Nil);
}

#[test]
fn test_unspecified_display() {
    let unspecified = Value::Unspecified;
    assert_eq!(format!("{}", unspecified), "#<unspecified>");
}
```

**Compiler Tests**:
```rust
#[test]
fn test_define_returns_unspecified() {
    let ast = vec![Expr::Define(DefineExpr::Variable {
        name: "x".to_string(),
        value: Box::new(Expr::Number(42.0)),
    })];
    
    let function = Compiler::compile_ast(&ast).unwrap();
    let mut vm = VM::new();
    let result = vm.interpret(&function.chunk).unwrap();
    
    assert!(result.is_unspecified());
}
```

**REPL Integration Tests**:
```rust
#[test]
fn test_repl_suppresses_unspecified_output() {
    let mut vm = VM::new();
    
    // Test that define produces no output
    let result = execute_in_repl("(define x 42)", &mut vm);
    assert_eq!(result.output, ""); // No output for unspecified
    
    // Test that accessing the variable works
    let result = execute_in_repl("x", &mut vm);
    assert_eq!(result.output, "42\n"); // Normal output for value
}
```

### Integration Tests

**Side-Effect Expression Tests**:
- `(define x 42)` - No output, binding created
- `(set! x 43)` - No output, binding updated  
- `(display "hello")` - Outputs "hello", no return value printed
- `(if #f 1)` - No output (unspecified value)

**Data Value Tests**:
- `'()` - Outputs "()"
- `42` - Outputs "42"
- `#t` - Outputs "#t"

**Error Handling Tests**:
- `(+ (define x 1) 2)` - Type error: cannot use unspecified value in arithmetic
- `((define x 1))` - Type error: unspecified value is not callable

### Performance Tests

**Memory Usage**:
- Verify unspecified values don't increase memory footprint
- Test garbage collection handles unspecified values correctly

**Execution Speed**:
- Ensure unspecified value handling doesn't impact performance
- Benchmark REPL responsiveness with mixed expressions

## Implementation Phases

### Phase 1: Core Object System
1. Add `Unspecified` variant to `Value` enum
2. Implement type checking methods (`is_unspecified()`)
3. Update `Display` and `PartialEq` implementations
4. Add unit tests for basic functionality

### Phase 2: Compiler Integration  
1. Update `compile_define()` to return unspecified values
2. Update `compile_set()` for assignments
3. Handle `if` expressions without else clauses
4. Add compiler tests

### Phase 3: VM and Built-ins
1. Update built-in I/O functions (`display`, `newline`)
2. Add proper error handling for type mismatches
3. Add VM-level tests

### Phase 4: REPL Integration
1. Modify REPL output logic to suppress unspecified values
2. Ensure all other values still print correctly
3. Add integration tests

### Phase 5: Comprehensive Testing
1. Test all side-effect expressions
2. Verify error messages are clear
3. Performance and memory testing
4. Documentation updates

## Backward Compatibility

This implementation maintains full backward compatibility:

- **Existing Code**: All current Scheme programs continue to work unchanged
- **API Stability**: No changes to public interfaces or existing bytecode
- **REPL Behavior**: Only suppresses output that shouldn't have been printed anyway
- **Error Messages**: Enhanced but not breaking existing error handling

The change is purely additive and corrects behavior to match Scheme standards rather than breaking existing functionality.