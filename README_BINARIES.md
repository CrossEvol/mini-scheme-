# MiniScheme Frontend Binaries

This project provides three separate binaries for different use cases:

## 1. Full Frontend (`mini_scheme`)
The complete MiniScheme frontend with both lexer and parser.

```bash
# Process a file (shows both tokens and AST)
cargo run --bin mini_scheme -- example/core/lambda.scm

# Start interactive REPL
cargo run --bin mini_scheme

# REPL commands:
# :tokens  - Show tokens only
# :ast     - Show AST only (default)
# :both    - Show both tokens and AST
# :none    - Parse only, no output
# quit     - Exit
```

## 2. Lexer Only (`lexer`)
Tokenization-focused tool for analyzing lexical structure.

```bash
# Tokenize a file
cargo run --bin lexer -- example/core/lambda.scm

# Start lexer REPL
cargo run --bin lexer

# REPL commands:
# :verbose - Show detailed token info with line/column (default)
# :simple  - Show simple token list
# quit     - Exit
```

### Example Output:
```
Tokens (12 total):
  1: TokenInfo { token: LeftParen, line: 1, column: 1, span: (0, 1) }
  2: TokenInfo { token: Lambda, line: 1, column: 2, span: (1, 7) }
  3: TokenInfo { token: LeftParen, line: 1, column: 9, span: (8, 9) }
  ...
```

## 3. Parser Only (`parser`)
AST generation tool for analyzing syntactic structure.

```bash
# Parse a file
cargo run --bin parser -- example/core/lambda.scm

# Start parser REPL
cargo run --bin parser

# REPL commands:
# :pretty  - Use pretty-printed AST format
# :debug   - Use debug AST format (default)
# :tokens  - Toggle showing tokens before AST
# quit     - Exit
```

### Example Output (Pretty Mode):
```
AST (1 expressions):
  1. Lambda(
    params: ["x"],
    body: [
      Call(
        func: Variable("+"),
        args: [
          Variable("x"),
          Number(1)
        ]
      )
    ]
  )
```

## Use Cases

### Lexer Binary
- **Debugging tokenization issues**: See exactly how input is tokenized
- **Learning lexical analysis**: Understand how text becomes tokens
- **Performance testing**: Measure tokenization speed
- **Token stream analysis**: Examine token sequences without parsing

### Parser Binary  
- **AST visualization**: See the structure of parsed expressions
- **Debugging parse errors**: Focus on parsing without tokenization noise
- **Syntax tree analysis**: Study how expressions are structured
- **Pretty printing**: Generate readable AST representations

### Full Frontend Binary
- **Complete processing**: See the full pipeline from text to AST
- **Integration testing**: Test both components together
- **General usage**: Most common use case for processing Scheme files

## Building All Binaries

```bash
# Build all binaries
cargo build

# Build specific binary
cargo build --bin lexer
cargo build --bin parser
cargo build --bin mini_scheme

# Run with optimizations
cargo run --release --bin lexer -- file.scm
```

## Installation

After building, the binaries are located in `target/debug/` (or `target/release/` for optimized builds):

- `target/debug/mini_scheme` - Full frontend
- `target/debug/lexer` - Lexer only
- `target/debug/parser` - Parser only

You can copy these to your PATH for system-wide access.