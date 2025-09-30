# MiniScheme Compiler Plan

This document outlines the preparation for the lexer and parser components of the MiniScheme compiler, based on the Scheme syntax examples provided in the `example/` directory.

## 1. Keywords and Special Forms

### Core Keywords
- `define` - Function and variable definition
- `lambda` - Anonymous function creation
- `if` - Conditional expression
- `cond` - Multi-way conditional expression
- `let` - Local binding
- `let*` - Sequential local binding
- `let loop` - Named let for iteration
- `let-values` - Binding values from multiple-value returns
- `call-with-values` - Calling procedures with multiple values
- `import` - Importing libraries
- `begin` - Grouping expressions
- `set!` - Variable assignment
- `quote` - Quotation (alternative to ')
- `quasiquote` - Quasi-quotation (alternative to `)
- `unquote` - Unquotation (alternative to ,)
- `unquote-splicing` - Unquote splicing (alternative to ,@)

### Control Flow Keywords
- `else` - Default case in conditional expressions

### Data Type Literals
- `#t`, `#f` - Boolean literals
- `'()` - Empty list
- `'null` - Null value

### Built-in Procedures
- `car`, `cdr` - List access
- `cons` - List construction
- `list` - List creation
- `vector` - Vector creation
- `hashtable` - Hashtable operations
- Predicates: `hashtable?`, `string?`, `number?`, `boolean?`, `char?`, `null?`, `pair?`, `eq?`, `char=?`, `string=?`
- Type conversions: `string->number`, `list->string`, `list->vector`, `vector->list`
- Control: `for-each`, `error`, `values`, `display`

## 2. Special Forms

Special forms are syntactic constructs that do not follow the normal evaluation rules.

### Core Special Forms
- `(define name value)` - Variable definition
- `(define (name args...) body...)` - Function definition
- `(lambda (args...) body...)` - Function creation
- `(if condition then else)` - Conditional expression
- `(cond (test body...) ... (else body...))` - Multi-way conditional
- `(let ((var val) ...) body...)` - Local binding
- `(let* ((var val) ...) body...)` - Sequential local binding
- `(let loop ((var val) ...) body...)` - Named let for iteration
- `(let-values (((var ...) expr) ...) body...)` - Binding multiple values
- `(call-with-values producer consumer)` - Multiple value handling

## 3. Token Types

### Lexical Tokens
- **Identifiers**: Variable names, function names (e.g., `x`, `hello`, `car`, `string->number`)
- **Keywords**: Reserved words with special meaning (e.g., `define`, `lambda`, `if`)
- **Numbers**: Integer and floating-point literals (e.g., `42`, `3.14`)
- **Strings**: Character sequences in double quotes (e.g., `"hello"`)
- **Characters**: Single characters preceded by `#\` (e.g., `#\a`, `#\space`)
- **Booleans**: Truth values `#t` and `#f`
- **Punctuation**:
  - Parentheses: `(` and `)`
  - Quote: `'`
  - Vector prefix: `#`
  - Comment start: `;`

### Token Structure
```rust
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Literals
    Identifier(String),
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    
    // Keywords
    Define,
    Lambda,
    If,
    Cond,
    Let,
    LetStar,
    LetLoop,
    LetValues,
    CallWithValues,
    Import,
    Else,
    
    // Punctuation
    LeftParen,
    RightParen,
    Quote,
    BackQuote,
    Comma,
    CommaAt,
    
    // Operators and built-in procedures
    Operator(String), // For things like +, -, *, etc.
    SetBang,         // set!
    QuoteSym,        // quote
    QuasiQuoteSym,   // quasiquote
    UnQuoteSym,      // unquote
    UnQuoteSplicingSym, // unquote-splicing
    Begin,
    Cons,
    List,
    Vector,
    Car,
    Cdr,
    Display,
    Error,
    Values,
    ForEach,
    
    // Predicate operators
    HashtableView,     // hashtable?
    StringView,        // string?
    NumberView,        // number?
    BooleanView,       // boolean?
    CharView,          // char?
    NullView,          // null?
    PairView,          // pair?
    EqView,            // eq?
    CharEqView,        // char=?
    StringEqView,      // string=?
    
    // Conversion operators
    StringToNumber,    // string->number
    ListToString,      // list->string
    ListToVector,      // list->vector
    VectorToList,      // vector->list
    
    // End of input
    Eof,
}
```

## 4. Abstract Syntax Tree (AST)

### AST Node Types

```rust
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Literal values
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    
    // Variables
    Variable(String),
    
    // Special forms
    Define(DefineExpr),
    Lambda(LambdaExpr),
    If(IfExpr),
    Cond(CondExpr),
    Let(LetExpr),
    LetStar(LetStarExpr),
    LetLoop(LetLoopExpr),
    LetValues(LetValuesExpr),
    CallWithValues(CallWithValuesExpr),
    Import(ImportExpr),
    Set(SetExpr),         // set!
    
    // Function application
    Call(Box<Expr>, Vec<Expr>), // (function arg1 arg2 ...)
    
    // Quoted expressions
    Quote(Box<Expr>),          // 'expr
    QuasiQuote(Box<Expr>),     // `expr
    UnQuote(Box<Expr>),        // ,expr
    UnQuoteSplicing(Box<Expr>), // ,@expr
    
    // Begin for multiple expressions
    Begin(Vec<Expr>),
    
    // Data structures
    List(Vec<Expr>),      // (list item1 item2 ...) or '(item1 item2 ...)
    Vector(Vec<Expr>),    // #(item1 item2 ...)
    Hashtable(Box<Expr>, Box<Expr>, Vec<(Expr, Expr)>), // (make-hashtable hash-fn equal-fn (key val) ...)
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefineExpr {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetExpr {
    pub var: String,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LambdaExpr {
    pub params: Vec<String>, // or could be more complex for variadic functions
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondExpr {
    pub clauses: Vec<CondClause>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondClause {
    pub test: Box<Expr>,
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub bindings: Vec<(String, Expr)>, // (var expr)
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStarExpr {
    pub bindings: Vec<(String, Expr)>, // (var expr) evaluated sequentially
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetLoopExpr {
    pub name: String,
    pub bindings: Vec<(String, Expr)>, // (var expr)
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetValuesExpr {
    pub bindings: Vec<(Vec<String>, Expr)>, // ((var ...) expr)
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallWithValuesExpr {
    pub producer: Box<Expr>,
    pub consumer: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportExpr {
    pub library: String, // or more complex library spec
}
```

## 5. Lexer Design

The lexer will transform input text into a sequence of tokens. It needs to handle:

- **Whitespace and comments**: Ignore whitespace and comments starting with `;`
- **Identifiers**: Names starting with letters or special characters like `+`, `-`, `*`, `/`, `!`, `?`, `=`, etc.
- **Numbers**: Integers and floating-point numbers (including scientific notation)
- **Strings**: Sequences of characters delimited by double quotes
- **Characters**: Single characters prefixed with `#\`
- **Boolean literals**: `#t` and `#f`
- **Special syntax**: Quote `'`, backquote `` ` ``, comma `,`, comma-at `,@`

### Lexer Implementation Plan
- Create a `Lexer` struct that holds the input text and current position
- Implement `next_token()` method to return the next token
- Handle multi-character operators and identifiers
- Handle special symbols like `!`, `?`, `=`, `->` in identifiers
- Track line and column numbers for error reporting
- Recognize special syntax for vectors (`#(`), characters (`#\`), booleans (`#t`, `#f`)

## 6. Parser Design

The parser will transform a sequence of tokens into an AST using recursive descent parsing.

### Top-Level Parser Functions
- `parse_program()`: Parse a complete program (sequence of expressions)
- `parse_expr()`: Parse a single expression
- `parse_special_form()`: Parse special forms like `define`, `lambda`, etc.
- `parse_list()`: Parse parenthesized expressions
- `parse_quote()`: Parse quoted expressions

### Error Handling
- Implement proper error reporting with line/column information
- Handle unmatched parentheses, unexpected tokens, etc.

## 7. Implementation Steps

### Phase 1: Basic Lexer
1. Implement basic token types
2. Handle identifiers and keywords
3. Handle numbers and strings
4. Handle punctuation and special characters
5. Add whitespace and comment handling

### Phase 2: Basic Parser
1. Implement expression parsing
2. Handle basic literal values
3. Implement function calls
4. Add error handling

### Phase 3: Special Forms
1. Add support for `define`
2. Add support for `lambda`
3. Add support for `if`
4. Add support for `cond`
5. Add support for `let` forms
6. Add support for `quote` and related forms

### Phase 4: Advanced Features
1. Add support for `import`
2. Add support for `call-with-values` and `let-values`
3. Improve error reporting
4. Add comprehensive tests

## 8. Testing Strategy

- Create unit tests for each component (lexer, parser)
- Use example files as test cases
- Test edge cases (empty lists, nested expressions, etc.)
- Test error conditions (unmatched parentheses, invalid syntax)

## 9. Integration with Existing Code

- The current `main.rs` is a placeholder; we'll replace it with actual lexer/parser logic
- Plan to add more modules as needed for lexer, parser, and AST
- Consider adding a REPL for interactive testing