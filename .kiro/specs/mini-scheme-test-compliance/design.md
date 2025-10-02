# Design Document

## Overview

The mini-scheme interpreter currently has a solid foundation with a lexer, parser, AST, compiler, and VM architecture. However, it lacks implementations for many core Scheme language constructs and built-in operations. This design outlines the systematic implementation of missing features to achieve full test compliance.

The interpreter follows a traditional compilation pipeline: Source → Tokens → AST → Bytecode → VM Execution. The missing functionality needs to be added at multiple levels of this pipeline.

## Architecture

### Current Architecture Analysis

The interpreter has a well-structured modular design:

- **Lexer**: Tokenizes source code, already supports most required tokens
- **Parser**: Converts tokens to AST, has basic structure for special forms
- **AST**: Defines expression types, includes most required constructs
- **Compiler**: Transforms AST to bytecode, needs significant expansion
- **VM**: Executes bytecode, needs built-in function implementations
- **Object System**: Handles values and closures, needs extension for new types

### Key Design Decisions

1. **Incremental Implementation**: Build upon existing infrastructure rather than rewriting
2. **Bytecode-Based Execution**: Continue using the VM approach for consistency
3. **Built-in Function Integration**: Implement missing operations as VM built-ins
4. **Error Handling**: Maintain existing error reporting patterns
5. **Output Formatting**: Standardize display formats to match Scheme conventions

## Components and Interfaces

### 1. Compiler Extensions

**Missing Special Form Compilation**:
- `if` expressions: Implement conditional jumps with proper branch handling
- `let`, `let*`, `let-loop`: Implement variable binding with scope management
- `cond`: Implement multi-branch conditionals with sequential evaluation
- `define`: Implement global variable and function definitions
- `lambda`: Fix closure creation and display formatting
- `import`: Implement module loading (minimal implementation)

**Implementation Strategy**:
```rust
impl Compiler {
    fn compile_if(&mut self, if_expr: &IfExpr) -> Result<(), CompileError>
    fn compile_let(&mut self, let_expr: &LetExpr) -> Result<(), CompileError>
    fn compile_cond(&mut self, cond_expr: &CondExpr) -> Result<(), CompileError>
    // ... other special forms
}
```

### 2. VM Built-in Functions

**Core Operations**:
- List operations: `car`, `cdr`, `cons`, `list`
- Type predicates: `null?`, `pair?`, `number?`, `string?`, etc.
- Comparison operations: `eq?`, `string=?`, `char=?`
- I/O operations: `display`
- Type conversions: `string->number`, `list->vector`, etc.

**Implementation Strategy**:
```rust
impl VM {
    fn call_builtin(&mut self, name: &str, arg_count: usize) -> Result<(), RuntimeError>
    fn builtin_car(&mut self) -> Result<(), RuntimeError>
    fn builtin_display(&mut self) -> Result<(), RuntimeError>
    // ... other built-ins
}
```

### 3. Object System Extensions

**New Value Types**:
- Vectors: Implement as `Vec<Value>` with proper display
- Hashtables: Implement as `HashMap<Value, Value>` with hash functions
- Characters: Already supported, ensure proper operations

**Enhanced Display Formatting**:
- Procedures: Display as `#<procedure>` instead of `#<closure:lambda@0>`
- Lists: Ensure proper parenthesized notation
- Vectors: Display with `#(...)` notation

### 4. Parser Enhancements

**Quote Handling**:
- Implement proper quote expansion for `'(1 2 3)` → `(quote (1 2 3))`
- Handle nested quotes and quasiquotes

**Vector Parsing**:
- Ensure `#(...)` syntax creates proper vector expressions

## Data Models

### Value System

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    Null,
    Cons(Rc<Cons>),
    Vector(Rc<Vec<Value>>),
    Hashtable(Rc<RefCell<HashMap<Value, Value>>>),
    Closure(Rc<Closure>),
    Builtin(String), // For built-in functions
}
```

### Built-in Function Registry

```rust
pub struct BuiltinRegistry {
    functions: HashMap<String, fn(&mut VM, usize) -> Result<(), RuntimeError>>,
}
```

### Scope Management

Enhanced local variable tracking for proper `let` and `let*` semantics:

```rust
pub struct ScopeManager {
    scopes: Vec<HashMap<String, usize>>, // Variable name to stack slot
    depth: usize,
}
```

## Error Handling

### Compilation Errors

- **Missing Implementation Errors**: Clear messages for unimplemented features
- **Type Checking**: Basic type validation during compilation where possible
- **Scope Errors**: Proper handling of undefined variables and redefinitions

### Runtime Errors

- **Arity Checking**: Validate argument counts for built-in functions
- **Type Errors**: Runtime type checking for operations like `car` on non-lists
- **Stack Management**: Prevent overflow and underflow conditions

## Testing Strategy

### Unit Testing Approach

1. **Lexer Tests**: Verify all required tokens are properly recognized
2. **Parser Tests**: Ensure AST generation for all special forms
3. **Compiler Tests**: Validate bytecode generation for each construct
4. **VM Tests**: Test built-in function implementations
5. **Integration Tests**: End-to-end testing with example files

### Test-Driven Implementation

1. Start with failing test cases
2. Implement minimal functionality to pass specific tests
3. Refactor and optimize while maintaining test compliance
4. Add comprehensive error handling

### Regression Testing

- Maintain existing functionality while adding new features
- Ensure output format consistency across all implementations
- Validate performance doesn't degrade significantly

## Implementation Phases

### Phase 1: Core Language Constructs
- Implement `if`, `let`, `let*`, `cond` compilation
- Add basic variable scoping and binding
- Fix `lambda` display formatting

### Phase 2: List Operations and Data Types
- Implement `car`, `cdr`, `cons`, `list` built-ins
- Add proper quote handling for list literals
- Implement vector operations

### Phase 3: Predicates and Type Checking
- Add all type predicate functions (`null?`, `pair?`, etc.)
- Implement comparison operations (`eq?`, `string=?`)
- Add character and string predicates

### Phase 4: I/O and Utility Functions
- Implement `display` for output
- Add `error` function creation
- Implement `for-each` and `values`

### Phase 5: Type Conversions and Advanced Features
- Add conversion functions (`string->number`, etc.)
- Implement hashtable operations
- Add `import` system (minimal)

### Phase 6: Output Format Standardization
- Ensure consistent procedure display
- Fix `define` and `import` to produce no output
- Standardize list and vector formatting