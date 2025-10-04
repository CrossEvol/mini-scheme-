// Module declarations
pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod error;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
pub mod trace;
pub mod vm;

// Re-exports for convenience
pub use ast::Expr;
pub use bytecode::{Chunk, Disassembler, OpCode};
pub use compiler::{Compiler, CompileError, FunctionType};
pub use error::{LexError, ParseError};
pub use lexer::Lexer;
pub use object::{Closure, Cons, Function, Object, Upvalue, Value};
pub use parser::Parser;
pub use token::{TokenType, Token};
pub use trace::{Tracer, TraceConfig, CompilationTrace, ExecutionTrace, CompilationPhase, TraceStats};
pub use vm::{VM, RuntimeError, CallFrame};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_identifiers() {
        let mut lexer = Lexer::new("hello world + - * /");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 7); // 6 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Plus);
        assert_eq!(tokens[3].token_type, TokenType::Minus);
        assert_eq!(tokens[4].token_type, TokenType::Multiply);
        assert_eq!(tokens[5].token_type, TokenType::Divide);
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = Lexer::new("define lambda if cond else");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 6); // 5 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Define);
        assert_eq!(tokens[1].token_type, TokenType::Lambda);
        assert_eq!(tokens[2].token_type, TokenType::If);
        assert_eq!(tokens[3].token_type, TokenType::Cond);
        assert_eq!(tokens[4].token_type, TokenType::Else);
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_predicates() {
        let mut lexer = Lexer::new("null? pair? eq? string=?");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 5); // 4 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::NullQ);
        assert_eq!(tokens[1].token_type, TokenType::PairQ);
        assert_eq!(tokens[2].token_type, TokenType::EqQ);
        assert_eq!(tokens[3].token_type, TokenType::StringEqQ);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_booleans() {
        let mut lexer = Lexer::new("#t #f");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 3); // 2 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Boolean(true));
        assert_eq!(tokens[1].token_type, TokenType::Boolean(false));
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_whitespace_and_comments() {
        let mut lexer = Lexer::new("hello ; this is a comment\nworld");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 3); // 2 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_position_tracking() {
        let mut lexer = Lexer::new("hello\nworld");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[1].column, 1);
    }

    #[test]
    fn test_lexer_special_identifiers() {
        let mut lexer = Lexer::new("set! string->number list->vector");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::SetBang);
        assert_eq!(tokens[1].token_type, TokenType::StringToNumber);
        assert_eq!(tokens[2].token_type, TokenType::ListToVector);
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_hashtable_operations() {
        let mut lexer = Lexer::new("make-hashtable hashtable-set! hashtable-ref hashtable?");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 5); // 4 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::MakeHashtable);
        assert_eq!(tokens[1].token_type, TokenType::HashtableSet);
        assert_eq!(tokens[2].token_type, TokenType::HashtableRef);
        assert_eq!(tokens[3].token_type, TokenType::HashtableQ);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_comparison_operators() {
        let mut lexer = Lexer::new("= < <= > >=");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 6); // 5 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Equal);
        assert_eq!(tokens[1].token_type, TokenType::LessThan);
        assert_eq!(tokens[2].token_type, TokenType::LessThanEqual);
        assert_eq!(tokens[3].token_type, TokenType::GreaterThan);
        assert_eq!(tokens[4].token_type, TokenType::GreaterThanEqual);
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_mixed_content() {
        let mut lexer = Lexer::new("define my-func lambda ; comment\n+ - custom-id");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens.len(), 7); // 6 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Define);
        assert_eq!(tokens[1].token_type, TokenType::Identifier("my-func".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Lambda);
        assert_eq!(tokens[3].token_type, TokenType::Plus);
        assert_eq!(tokens[4].token_type, TokenType::Minus);
        assert_eq!(tokens[5].token_type, TokenType::Identifier("custom-id".to_string()));
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_numbers() {
        // Test integers
        let mut lexer = Lexer::new("42 -17 +123");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Number(42.0));
        assert_eq!(tokens[1].token_type, TokenType::Number(-17.0));
        assert_eq!(tokens[2].token_type, TokenType::Number(123.0));
        assert_eq!(tokens[3].token_type, TokenType::Eof);

        // Test floating point numbers
        let mut lexer = Lexer::new("3.14 -2.5 +0.001");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].token_type, TokenType::Number(3.14));
        assert_eq!(tokens[1].token_type, TokenType::Number(-2.5));
        assert_eq!(tokens[2].token_type, TokenType::Number(0.001));
        assert_eq!(tokens[3].token_type, TokenType::Eof);

        // Test edge cases
        let mut lexer = Lexer::new("0 0.0 123.456");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].token_type, TokenType::Number(0.0));
        assert_eq!(tokens[1].token_type, TokenType::Number(0.0));
        assert_eq!(tokens[2].token_type, TokenType::Number(123.456));
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_strings() {
        // Test basic strings
        let mut lexer = Lexer::new(r#""hello" "world" """#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String("".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Eof);

        // Test strings with escape sequences
        let mut lexer = Lexer::new(r#""hello\nworld" "tab\there" "quote\"here" "backslash\\""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::String("hello\nworld".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("tab\there".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String("quote\"here".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::String("backslash\\".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_characters() {
        // Test basic characters
        let mut lexer = Lexer::new(r"#\a #\Z #\1 #\!");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 5); // 4 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Character('a'));
        assert_eq!(tokens[1].token_type, TokenType::Character('Z'));
        assert_eq!(tokens[2].token_type, TokenType::Character('1'));
        assert_eq!(tokens[3].token_type, TokenType::Character('!'));
        assert_eq!(tokens[4].token_type, TokenType::Eof);

        // Test special character names
        let mut lexer = Lexer::new(r"#\space #\newline #\tab");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].token_type, TokenType::Character(' '));
        assert_eq!(tokens[1].token_type, TokenType::Character('\n'));
        assert_eq!(tokens[2].token_type, TokenType::Character('\t'));
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_mixed_literals() {
        let mut lexer = Lexer::new(r#"42 "hello" #\a #t 3.14 #f"#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 7); // 6 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Number(42.0));
        assert_eq!(tokens[1].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Character('a'));
        assert_eq!(tokens[3].token_type, TokenType::Boolean(true));
        assert_eq!(tokens[4].token_type, TokenType::Number(3.14));
        assert_eq!(tokens[5].token_type, TokenType::Boolean(false));
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_number_errors() {
        // Test invalid number format with multiple decimal points
        let mut lexer = Lexer::new("1.2.3");
        let result = lexer.tokenize();
        assert!(result.is_err());

        // Test number followed by invalid character (should be treated as separate tokens)
        let mut lexer = Lexer::new("123abc");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 3); // number + identifier + EOF
        assert_eq!(tokens[0].token_type, TokenType::Number(123.0));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("abc".to_string()));
    }

    #[test]
    fn test_lexer_string_errors() {
        // Test unterminated string
        let mut lexer = Lexer::new(r#""hello world"#);
        let result = lexer.tokenize();
        assert!(result.is_err());
        if let Err(LexError::UnterminatedString { .. }) = result {
            // Expected error
        } else {
            panic!("Expected UnterminatedString error");
        }
    }

    #[test]
    fn test_lexer_character_errors() {
        // Test invalid hash literal
        let mut lexer = Lexer::new("#x");
        let result = lexer.tokenize();
        assert!(result.is_err());
        if let Err(LexError::InvalidCharacter { text, .. }) = result {
            assert_eq!(text, "#x");
        }

        // Test incomplete character literal
        let mut lexer = Lexer::new("#\\");
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_lexer_edge_cases() {
        // Test empty input
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Eof);

        // Test only whitespace and comments
        let mut lexer = Lexer::new("   \n\t  ; just a comment\n  ");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::Eof);

        // Test single character identifiers
        let mut lexer = Lexer::new("+ - * /");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Multiply);
        assert_eq!(tokens[3].token_type, TokenType::Divide);
        assert_eq!(tokens[4].token_type, TokenType::Eof);

        // Test numbers vs identifiers
        let mut lexer = Lexer::new("+ +123 - -456");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Number(123.0));
        assert_eq!(tokens[2].token_type, TokenType::Minus);
        assert_eq!(tokens[3].token_type, TokenType::Number(-456.0));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }
}
