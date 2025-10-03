# Design Document

## Overview

This design implements Scheme's multiple values mechanism (`values` and `call-with-values`) using a stack-based approach without composite types. The key insight is that multiple values should remain as separate entities on the VM stack, coordinated through specialized bytecode instructions and VM state tracking.

The implementation extends the existing MiniScheme bytecode compiler and virtual machine with two new instructions: `OP_RETURN_VALUES` and `OP_CALL_WITH_VALUES`, along with VM state to track multiple return values.

## Architecture

### Core Design Principles

1. **No Composite Types**: Multiple values are never packaged into a `MultipleValues` type
2. **Stack-Based Coordination**: Values remain as individual stack entries
3. **Instruction-Level Protocol**: Producer and consumer coordination through bytecode instructions
4. **Metadata Tracking**: VM tracks return value count without modifying the values themselves

### Component Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Compiler      │    │   Bytecode      │    │   Virtual       │
│                 │    │   Instructions  │    │   Machine       │
│ - values expr   │───▶│ OP_RETURN_VALUES│───▶│ - Stack mgmt    │
│ - call-with-    │    │ OP_CALL_WITH_   │    │ - Return count  │
│   values expr   │───▶│   VALUES        │───▶│ - Call coord    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Components and Interfaces

### 1. Bytecode Instructions

#### New OpCode Variants

```rust
pub enum OpCode {
    // ... existing opcodes ...
    
    /// Return multiple values from current function
    /// Operand: u8 - number of values on stack to return
    OP_RETURN_VALUES = 46,
    
    /// Execute call-with-values coordination
    /// No operands - expects producer and consumer on stack
    OP_CALL_WITH_VALUES = 47,
}
```

#### Instruction Semantics

- **`OP_RETURN_VALUES n`**: Marks the top `n` stack values as return values and returns from current function
- **`OP_CALL_WITH_VALUES`**: Pops consumer and producer, calls producer, then calls consumer with producer's results

### 2. Virtual Machine Extensions

#### VM State Extensions

```rust
pub struct VM {
    // ... existing fields ...
    
    /// Number of values returned by the last function call
    /// None = single value, Some(n) = n multiple values
    pub last_return_count: Option<usize>,
    
    /// Flag indicating if we're in a multiple-value context
    pub in_multiple_value_context: bool,
}
```

#### Key VM Methods

```rust
impl VM {
    /// Execute OP_RETURN_VALUES instruction
    fn op_return_values(&mut self, value_count: u8) -> Result<(), RuntimeError>;
    
    /// Execute OP_CALL_WITH_VALUES instruction  
    fn op_call_with_values(&mut self) -> Result<(), RuntimeError>;
    
    /// Check if multiple values are valid in current context
    fn validate_value_context(&self, value_count: usize) -> Result<(), RuntimeError>;
    
    /// Call a function and handle multiple return values
    fn call_function_with_values(&mut self, closure: Rc<Closure>, arg_count: usize) -> Result<(), RuntimeError>;
}
```

### 3. Compiler Extensions

#### AST Compilation

The compiler already has `CallWithValuesExpr` and `LetValuesExpr` in the AST. We need to add compilation support:

```rust
impl Compiler {
    /// Compile call-with-values expression
    fn compile_call_with_values(&mut self, expr: &CallWithValuesExpr) -> Result<(), CompileError>;
    
    /// Compile let-values expression by transforming to call-with-values
    fn compile_let_values(&mut self, expr: &LetValuesExpr) -> Result<(), CompileError>;
    
    /// Compile values expression (as function call)
    fn compile_values_call(&mut self, args: &[Expr]) -> Result<(), CompileError>;
}
```

#### Let-values to Call-with-values Transformation

`let-values` expressions are compiled by transforming them into equivalent `call-with-values` forms:

```scheme
;; Original let-values
(let-values (((a b c) (values 10 20 30)))
  (+ a b c))

;; Transformed to call-with-values
(call-with-values
  (lambda () (values 10 20 30))  ; producer
  (lambda (a b c) (+ a b c)))    ; consumer
```

This transformation allows us to implement `let-values` without additional bytecode instructions.

## Data Models

### Multiple Values Representation

Multiple values are represented as:
1. **Individual stack values**: Each value remains as a separate `Value` on the stack
2. **Return count metadata**: VM tracks how many values were returned
3. **Context flags**: VM knows when it's in a multiple-value context

### Stack Layout Examples

#### Example 1: `(values 1 2 3)`
```
Stack before OP_RETURN_VALUES 3:
[... | Value::Number(1) | Value::Number(2) | Value::Number(3)]
                                                              ^sp

After OP_RETURN_VALUES 3:
- Function returns
- VM sets last_return_count = Some(3)
- Stack contains the 3 values for caller
```

#### Example 2: `(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))`
```
1. Stack before OP_CALL_WITH_VALUES:
   [... | consumer_closure | producer_closure]

2. After calling producer:
   [... | Value::Number(1) | Value::Number(2)]
   last_return_count = Some(2)

3. After calling consumer with args:
   [... | Value::Number(3)]  ; result of (+ 1 2)
   last_return_count = None
```

## Error Handling

### Error Types

```rust
pub enum RuntimeError {
    // ... existing variants ...
    
    /// Multiple values in single value context
    MultipleValuesInSingleContext { count: usize },
    
    /// Zero values in single value context  
    ZeroValuesInSingleContext,
    
    /// Producer function arity mismatch (should take 0 args)
    ProducerArityMismatch { expected: usize, got: usize },
    
    /// Consumer function arity mismatch
    ConsumerArityMismatch { expected: usize, got: usize },
}
```

### Context Validation

The VM validates value contexts:
- **Single-value context**: Only 1 value allowed, error if 0 or >1
- **Multiple-value context**: Any number of values allowed
- **Call-with-values context**: Special handling for producer/consumer coordination

## Testing Strategy

### Unit Tests

1. **Bytecode Generation Tests**
   - Verify `OP_RETURN_VALUES` generation for `values` expressions
   - Verify `OP_CALL_WITH_VALUES` generation for `call-with-values` expressions

2. **VM Execution Tests**
   - Test `OP_RETURN_VALUES` with different value counts (0, 1, multiple)
   - Test `OP_CALL_WITH_VALUES` coordination
   - Test error cases (wrong contexts, arity mismatches)

3. **Integration Tests**
   - Test complete `call-with-values` expressions
   - Test multiple values in different contexts
   - Test error propagation

### Test Cases

```scheme
;; Basic multiple values
(values 1 2 3)  ; Should work in multiple-value context

;; Call-with-values coordination
(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))  ; => 3

;; Let-values equivalence
(let-values (((a b c) (values 10 20 30)))
  (+ a b c))  ; => 60, equivalent to call-with-values form

;; Multiple let-values bindings (if supported)
(let-values (((x y) (values 1 2))
             ((z) (values 3)))
  (+ x y z))  ; => 6

;; Error cases
(+ (values 1 2) 3)  ; Should error: multiple values in single context
(+ (values) 3)      ; Should error: zero values in single context
(+ (values 1) 3)    ; Should work: => 4
```

## Implementation Details

### Compilation Flow

1. **`values` expressions**: Compiled as function calls to built-in `values` function
2. **`call-with-values` expressions**: Generate `OP_CALL_WITH_VALUES` instruction
3. **`let-values` expressions**: Transform to equivalent `call-with-values` form, then compile
4. **Context detection**: Compiler tracks when expressions are in multiple-value contexts

#### Let-values Compilation Strategy

```rust
fn compile_let_values(&mut self, expr: &LetValuesExpr) -> Result<(), CompileError> {
    // Transform: (let-values (((vars...) producer-expr)) body...)
    // Into: (call-with-values (lambda () producer-expr) (lambda (vars...) body...))
    
    for (vars, producer_expr) in &expr.bindings {
        // Create producer lambda: (lambda () producer-expr)
        let producer = LambdaExpr {
            params: vec![],
            body: vec![producer_expr.clone()],
        };
        
        // Create consumer lambda: (lambda (vars...) body...)
        let consumer = LambdaExpr {
            params: vars.clone(),
            body: expr.body.clone(),
        };
        
        // Compile as call-with-values
        let call_with_values = CallWithValuesExpr {
            producer: Expr::Lambda(Box::new(producer)),
            consumer: Expr::Lambda(Box::new(consumer)),
        };
        
        self.compile_call_with_values(&call_with_values)?;
    }
    Ok(())
}
```

### VM Execution Flow

#### OP_RETURN_VALUES Execution
1. Read value count from instruction operand
2. Validate stack has enough values
3. Set `last_return_count` metadata
4. Return from current function (values remain on stack)

#### OP_CALL_WITH_VALUES Execution
1. Pop consumer and producer closures from stack
2. Validate both are callable with correct arities
3. Call producer with 0 arguments
4. Check `last_return_count` to determine how many values were produced
5. Call consumer with the produced values as arguments
6. Reset `last_return_count` to None

### Context Management

The VM maintains context awareness:
- **Function calls**: Reset to single-value context by default
- **`call-with-values`**: Creates multiple-value context for producer
- **Arithmetic operations**: Require single-value context
- **Assignment**: Requires single-value context

## Integration with Existing Code

### Minimal Changes Required

1. **Add new OpCodes** to `bytecode.rs`
2. **Extend VM struct** with return count tracking
3. **Add VM instruction handlers** for new opcodes
4. **Update compiler** to generate new instructions
5. **Remove `MultipleValues` type** from `Value` enum
6. **Update built-in `values` function** to use `OP_RETURN_VALUES`

### Backward Compatibility

- Existing single-value functions continue to work unchanged
- No changes to existing bytecode instructions
- Existing `Value` types remain the same (except removing `MultipleValues`)

## Performance Considerations

### Advantages
- No heap allocation for multiple values
- Direct stack manipulation (faster than composite types)
- Minimal VM state overhead (just a counter)

### Trade-offs
- Slightly more complex VM logic for context tracking
- Need to validate contexts at runtime

## Security Considerations

- Stack overflow protection remains unchanged
- No new attack vectors introduced
- Error handling prevents invalid state transitions