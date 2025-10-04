# Design Document

## Overview

This design document outlines the implementation approach for adding docstring support to the MiniScheme compiler. The feature will extend the existing AST structures and parser logic to recognize and handle documentation strings in function definitions while maintaining backward compatibility and ensuring docstrings don't affect program execution.

The implementation will modify the `DefineExpr` and `LambdaExpr` AST nodes to include optional docstring fields, update the parser to recognize docstring syntax, and ensure the compiler properly skips docstrings during code generation.

## Architecture

### AST Modifications

The core change involves extending the existing AST structures to accommodate docstrings:

```rust
// Modified DefineExpr structure
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
    pub docstring: Option<String>, // New field for docstring
}

// Modified LambdaExpr structure  
pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
    pub docstring: Option<String>, // New field for docstring
}
```

### Parser Integration

The parser will be enhanced to detect docstrings in two contexts:

1. **Function definitions**: `(define (name params...) "docstring" body...)`
2. **Lambda expressions**: `(lambda (params...) "docstring" body...)`

The parsing logic will:
- Check if the first expression after parameters is a string literal
- If so, extract it as a docstring and continue parsing the remaining body
- If not, treat all expressions as the function body

### Compiler Behavior

The compiler will be updated to handle the new AST structure:
- When compiling `DefineExpr` or `LambdaExpr` nodes with docstrings, ignore the docstring field
- Continue compiling the function body as before
- Maintain all existing compilation behavior for functions without docstrings

## Components and Interfaces

### 1. AST Module (`src/ast.rs`)

**Changes Required:**
- Add `docstring: Option<String>` field to `DefineExpr`
- Add `docstring: Option<String>` field to `LambdaExpr`
- Update constructors and implementations to handle the new field

**Interface:**
```rust
impl DefineExpr {
    pub fn new(name: String, value: Expr) -> Self {
        Self { name, value, docstring: None }
    }
    
    pub fn new_with_docstring(name: String, value: Expr, docstring: String) -> Self {
        Self { name, value, docstring: Some(docstring) }
    }
}

impl LambdaExpr {
    pub fn new(params: Vec<String>, body: Vec<Expr>) -> Self {
        Self { params, body, docstring: None }
    }
    
    pub fn new_with_docstring(params: Vec<String>, body: Vec<Expr>, docstring: String) -> Self {
        Self { params, body, docstring: Some(docstring) }
    }
}
```

### 2. Parser Module (`src/parser.rs`)

**Changes Required:**
- Modify `parse_define()` to detect and extract docstrings in function definitions
- Modify `parse_lambda()` to detect and extract docstrings in lambda expressions
- Add helper method `parse_docstring_and_body()` for common docstring parsing logic

**Key Methods:**
```rust
impl Parser {
    fn parse_docstring_and_body(&mut self) -> Result<(Option<String>, Vec<Expr>), ParseError> {
        // Check if first expression is a string (potential docstring)
        // If so, extract it and parse remaining expressions as body
        // If not, parse all expressions as body
    }
    
    fn is_string_literal(&self) -> bool {
        // Helper to check if current token is a string literal
    }
}
```

### 3. Compiler Module (`src/compiler.rs`)

**Changes Required:**
- Update `compile_define()` to handle the new `docstring` field (ignore it)
- Update `compile_lambda()` to handle the new `docstring` field (ignore it)
- Ensure all existing compilation logic remains unchanged

**No Interface Changes:** The compiler will simply ignore the docstring field and compile the function body as before.

## Data Models

### AST Node Extensions

```rust
// Before (current)
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
}

pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
}

// After (with docstring support)
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
    pub docstring: Option<String>,
}

pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
    pub docstring: Option<String>,
}
```

### Parsing State Machine

The parser will follow this logic for function definitions:

```
1. Parse function name and parameters
2. Check next token:
   - If String literal → Extract as docstring, continue to step 3
   - If not String → Set docstring to None, treat as body expression
3. Parse remaining expressions as function body
4. Create AST node with docstring (if any) and body
```

## Error Handling

### Syntax Error Cases

1. **Empty function body after docstring:**
   ```scheme
   (define (fn) "docstring")  ; Error: missing function body
   ```

2. **Invalid docstring format:**
   ```scheme
   (define (fn) 123 body)  ; 123 is not a valid docstring
   ```

3. **Malformed string literals:**
   ```scheme
   (define (fn) "unterminated string body)  ; String parsing error
   ```

### Error Messages

The parser will provide clear, contextual error messages:

```rust
ParseError::InvalidDocstring {
    reason: String,
    line: usize,
    column: usize,
    suggestion: Option<String>,
}

ParseError::MissingFunctionBody {
    function_name: String,
    line: usize,
    column: usize,
}
```

### Recovery Strategy

- If docstring parsing fails, fall back to treating the string as part of the function body
- Provide suggestions for correct docstring syntax
- Continue parsing to catch additional errors when possible

## Testing Strategy

### Unit Tests

1. **AST Construction Tests:**
   - Test creating `DefineExpr` and `LambdaExpr` with and without docstrings
   - Verify field access and serialization

2. **Parser Tests:**
   - Test parsing functions with docstrings
   - Test parsing functions without docstrings (backward compatibility)
   - Test error cases (malformed docstrings, missing bodies)
   - Test edge cases (empty docstrings, multiple strings)

3. **Compiler Tests:**
   - Test that docstrings are ignored during compilation
   - Test that function behavior is unchanged
   - Test bytecode generation remains identical for function bodies

### Integration Tests

1. **End-to-End Tests:**
   - Parse and compile complete programs with documented functions
   - Verify execution results are identical with and without docstrings
   - Test mixing documented and undocumented functions

2. **Compatibility Tests:**
   - Ensure existing test files continue to pass
   - Test that old AST serialization/deserialization still works
   - Verify no performance regression

### Test Cases

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_function_with_docstring() {
        // (define (add a b) "Adds two numbers" (+ a b))
    }
    
    #[test]
    fn test_parse_function_without_docstring() {
        // (define (add a b) (+ a b))
    }
    
    #[test]
    fn test_parse_lambda_with_docstring() {
        // (lambda (x) "Identity function" x)
    }
    
    #[test]
    fn test_docstring_compilation_ignored() {
        // Verify docstrings don't affect bytecode generation
    }
    
    #[test]
    fn test_error_empty_body_after_docstring() {
        // (define (fn) "doc") should error
    }
}
```

## Implementation Phases

### Phase 1: AST Extensions
- Add docstring fields to `DefineExpr` and `LambdaExpr`
- Update constructors and ensure backward compatibility
- Add unit tests for AST modifications

### Phase 2: Parser Integration
- Implement docstring detection logic
- Update `parse_define()` and `parse_lambda()` methods
- Add comprehensive parser tests

### Phase 3: Compiler Updates
- Ensure compiler ignores docstring fields
- Verify no changes to bytecode generation
- Add compiler tests

### Phase 4: Integration and Testing
- Run full test suite to ensure backward compatibility
- Add integration tests with example files
- Performance testing and optimization if needed

## Backward Compatibility

### Existing Code Support
- All existing Scheme code without docstrings will continue to work unchanged
- AST nodes without docstrings will have `docstring: None`
- Parser will handle both old and new syntax seamlessly

### Migration Path
- No migration required for existing code
- Developers can gradually add docstrings to their functions
- Tools can be built later to extract and display docstrings

### API Stability
- New AST constructors will be added alongside existing ones
- Existing constructors will set `docstring: None` by default
- No breaking changes to public interfaces