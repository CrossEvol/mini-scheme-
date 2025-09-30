# Design Document

## Overview

The MiniScheme compiler frontend consists of three main components: a lexer for tokenization, a parser for syntax analysis, and an Abstract Syntax Tree (AST) representation. The design follows a traditional compiler frontend architecture with clear separation of concerns and modular components that can be tested independently.

The implementation will be built in Rust, leveraging the language's strong type system and pattern matching capabilities to create robust and efficient lexer and parser components. The design prioritizes correctness, maintainability, and extensibility to support future language features.

## Architecture

### Component Overview

```
Source Code → Lexer → Token Stream → Parser → AST
```

1. **Lexer Module** (`src/lexer.rs`): Transforms raw source code into a stream of tokens
2. **Token Module** (`src/token.rs`): Defines token types and their representations
3. **Parser Module** (`src/parser.rs`): Transforms token stream into an Abstract Syntax Tree
4. **AST Module** (`src/ast.rs`): Defines the structure of the Abstract Syntax Tree
5. **Error Module** (`src/error.rs`): Provides comprehensive error handling and reporting

### Module Dependencies

```
main.rs
├── lexer.rs (depends on token.rs, error.rs)
├── parser.rs (depends on token.rs, ast.rs, error.rs)
├── token.rs
├── ast.rs
└── error.rs
```

## Components and Interfaces

### Token Module (`src/token.rs`)

The token module defines all possible tokens in the MiniScheme language:

```rust
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Literals
    Identifier(String),
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    
    // Keywords and Special Forms
    Define,
    Lambda,
    If,
    Cond,
    Else,
    Let,
    LetStar,
    LetLoop,
    LetValues,
    CallWithValues,
    Import,
    Begin,
    SetBang,        // set!
    Quote,          // quote
    QuasiQuote,     // quasiquote
    UnQuote,        // unquote
    UnQuoteSplicing, // unquote-splicing
    
    // Built-in Procedures
    Car,
    Cdr,
    Cons,
    List,
    Vector,
    Display,
    Error,
    Values,
    ForEach,
    
    // Predicates
    HashtableQ,     // hashtable?
    StringQ,        // string?
    NumberQ,        // number?
    BooleanQ,       // boolean?
    CharQ,          // char?
    CharNumericQ,   // char-numeric?
    CharWhitespaceQ, // char-whitespace?
    NullQ,          // null?
    PairQ,          // pair?
    EqQ,            // eq?
    CharEqQ,        // char=?
    StringEqQ,      // string=?
    
    // Type Conversions
    StringToNumber, // string->number
    ListToString,   // list->string
    ListToVector,   // list->vector
    VectorToList,   // vector->list
    
    // Punctuation
    LeftParen,
    RightParen,
    Quote,          // '
    BackQuote,      // `
    Comma,          // ,
    CommaAt,        // ,@
    VectorPrefix,   // # (for vectors)
    
    // Special
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
    pub column: usize,
    pub span: (usize, usize), // start and end positions in source
}
```

### Lexer Module (`src/lexer.rs`)

The lexer provides a stateful tokenizer that processes source code character by character:

```rust
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self;
    pub fn next_token(&mut self) -> Result<TokenInfo, LexError>;
    pub fn tokenize(&mut self) -> Result<Vec<TokenInfo>, LexError>;
    
    // Private helper methods
    fn current_char(&self) -> Option<char>;
    fn peek_char(&self) -> Option<char>;
    fn advance(&mut self);
    fn skip_whitespace(&mut self);
    fn skip_comment(&mut self);
    fn read_identifier(&mut self) -> String;
    fn read_number(&mut self) -> Result<f64, LexError>;
    fn read_string(&mut self) -> Result<String, LexError>;
    fn read_character(&mut self) -> Result<char, LexError>;
    fn keyword_or_identifier(&self, text: &str) -> Token;
}
```

**Key Design Decisions:**
- Character-based processing for precise error location tracking
- Lookahead capability for multi-character tokens
- Keyword recognition through a lookup table
- Support for escape sequences in strings and characters
- Comprehensive comment and whitespace handling

### AST Module (`src/ast.rs`)

The AST module defines the structure for representing parsed Scheme programs:

```rust
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Literals
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    Variable(String),
    
    // Special Forms
    Define(Box<DefineExpr>),
    Lambda(Box<LambdaExpr>),
    If(Box<IfExpr>),
    Cond(Box<CondExpr>),
    Let(Box<LetExpr>),
    LetStar(Box<LetStarExpr>),
    LetLoop(Box<LetLoopExpr>),
    LetValues(Box<LetValuesExpr>),
    CallWithValues(Box<CallWithValuesExpr>),
    Import(Box<ImportExpr>),
    Set(Box<SetExpr>),
    
    // Function Application
    Call(Box<Expr>, Vec<Expr>),
    
    // Quotation
    Quote(Box<Expr>),
    QuasiQuote(Box<Expr>),
    UnQuote(Box<Expr>),
    UnQuoteSplicing(Box<Expr>),
    
    // Sequencing
    Begin(Vec<Expr>),
    
    // Data Structures
    List(Vec<Expr>),
    Vector(Vec<Expr>),
}

// Supporting structures for special forms
#[derive(Debug, PartialEq, Clone)]
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_expr: Expr,
    pub else_expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondExpr {
    pub clauses: Vec<CondClause>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondClause {
    pub test: Expr,
    pub body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
}

// Additional structures for other special forms...
```

### Parser Module (`src/parser.rs`)

The parser implements recursive descent parsing with comprehensive error recovery:

```rust
pub struct Parser {
    tokens: Vec<TokenInfo>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<TokenInfo>) -> Self;
    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError>;
    
    // Core parsing methods
    fn parse_expr(&mut self) -> Result<Expr, ParseError>;
    fn parse_atom(&mut self) -> Result<Expr, ParseError>;
    fn parse_list(&mut self) -> Result<Expr, ParseError>;
    fn parse_special_form(&mut self, keyword: &Token) -> Result<Expr, ParseError>;
    
    // Special form parsers
    fn parse_define(&mut self) -> Result<DefineExpr, ParseError>;
    fn parse_lambda(&mut self) -> Result<LambdaExpr, ParseError>;
    fn parse_if(&mut self) -> Result<IfExpr, ParseError>;
    fn parse_cond(&mut self) -> Result<CondExpr, ParseError>;
    fn parse_let(&mut self) -> Result<LetExpr, ParseError>;
    // ... other special form parsers
    
    // Utility methods
    fn current_token(&self) -> Option<&TokenInfo>;
    fn peek_token(&self) -> Option<&TokenInfo>;
    fn advance(&mut self) -> Option<&TokenInfo>;
    fn expect_token(&mut self, expected: Token) -> Result<&TokenInfo, ParseError>;
    fn match_token(&mut self, token: &Token) -> bool;
}
```

**Parser Design Principles:**
- Recursive descent for clear structure mapping
- Comprehensive error reporting with context
- Recovery strategies for continued parsing after errors
- Support for all Scheme special forms
- Proper precedence and associativity handling

### Error Module (`src/error.rs`)

Comprehensive error handling for both lexical and syntactic errors:

```rust
#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    UnexpectedCharacter { char: char, line: usize, column: usize },
    UnterminatedString { line: usize, column: usize },
    InvalidNumber { text: String, line: usize, column: usize },
    InvalidCharacter { text: String, line: usize, column: usize },
    UnexpectedEof { line: usize, column: usize },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken { expected: String, found: Token, line: usize, column: usize },
    UnexpectedEof { expected: String },
    InvalidSpecialForm { form: String, reason: String, line: usize, column: usize },
    UnmatchedParenthesis { line: usize, column: usize },
    InvalidBinding { reason: String, line: usize, column: usize },
}

impl std::fmt::Display for LexError { /* ... */ }
impl std::fmt::Display for ParseError { /* ... */ }
impl std::error::Error for LexError { /* ... */ }
impl std::error::Error for ParseError { /* ... */ }
```

## Data Models

### Token Stream Model

The lexer produces a stream of `TokenInfo` structures that include both the token type and location information. This enables precise error reporting and supports future features like source maps.

### AST Model

The AST uses an enum-based design that directly corresponds to Scheme's expression-based nature. Each variant represents a different type of expression, with boxed recursive structures to handle nested expressions efficiently.

**Key Design Decisions:**
- Box allocation for recursive structures to prevent infinite size
- Separate structs for complex special forms to maintain clarity
- Vector-based storage for variable-length constructs (parameters, arguments, etc.)
- Clone trait implementation for AST manipulation and transformation

## Error Handling

### Error Recovery Strategy

1. **Lexical Errors**: Report and attempt to continue tokenization when possible
2. **Syntax Errors**: Use panic mode recovery - skip tokens until a synchronization point
3. **Semantic Validation**: Validate special form structure during parsing

### Error Reporting

- Line and column information for all errors
- Context-aware error messages
- Suggestions for common mistakes
- Multiple error reporting when possible

## Testing Strategy

### Unit Testing Approach

1. **Lexer Tests**:
   - Token recognition for all token types
   - Error handling for invalid input
   - Position tracking accuracy
   - Comment and whitespace handling

2. **Parser Tests**:
   - Correct AST generation for valid input
   - Error reporting for invalid syntax
   - Special form parsing
   - Nested expression handling

3. **Integration Tests**:
   - End-to-end parsing of example files
   - Error recovery scenarios
   - Performance with large inputs

### Test Data Organization

```
tests/
├── lexer/
│   ├── valid_tokens.rs
│   ├── invalid_input.rs
│   └── edge_cases.rs
├── parser/
│   ├── expressions.rs
│   ├── special_forms.rs
│   ├── error_recovery.rs
│   └── integration.rs
└── fixtures/
    ├── valid_programs/
    └── invalid_programs/
```

## Performance Considerations

### Memory Management

- Use of `Box` for recursive AST structures to minimize stack usage
- String interning for frequently used identifiers (future optimization)
- Efficient token stream representation

### Parsing Performance

- Single-pass parsing with minimal backtracking
- Efficient keyword lookup using hash maps
- Lazy evaluation where appropriate

## Future Extensibility

### Language Feature Extensions

The design supports easy addition of:
- New special forms through enum extension
- Additional data types
- Macro system support
- Module system enhancements

### Tooling Integration

The modular design enables:
- Language server protocol implementation
- Syntax highlighting support
- Code formatting tools
- Static analysis tools

## Integration Points

### Compiler Pipeline Integration

The frontend provides clean interfaces for:
- Semantic analysis phase
- Code generation phase
- Error reporting system
- Debug information generation

### REPL Integration

The parser supports:
- Interactive expression evaluation
- Partial input handling
- Error recovery for continued interaction