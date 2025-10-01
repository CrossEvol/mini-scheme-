use std::fmt;

/// Errors that can occur during lexical analysis
#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    UnexpectedCharacter { 
        char: char, 
        line: usize, 
        column: usize 
    },
    UnterminatedString { 
        line: usize, 
        column: usize 
    },
    InvalidNumber { 
        text: String, 
        line: usize, 
        column: usize 
    },
    InvalidCharacter { 
        text: String, 
        line: usize, 
        column: usize 
    },
    UnexpectedEof { 
        line: usize, 
        column: usize 
    },
}

/// Errors that can occur during parsing
#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken { 
        expected: String, 
        found: String, 
        line: usize, 
        column: usize 
    },
    UnexpectedEof { 
        expected: String 
    },
    InvalidSpecialForm { 
        form: String, 
        reason: String, 
        line: usize, 
        column: usize 
    },
    UnmatchedParenthesis { 
        line: usize, 
        column: usize 
    },
    InvalidBinding { 
        reason: String, 
        line: usize, 
        column: usize 
    },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedCharacter { char, line, column } => {
                write!(f, "Unexpected character '{}' at line {}, column {}", char, line, column)
            }
            LexError::UnterminatedString { line, column } => {
                write!(f, "Unterminated string at line {}, column {}", line, column)
            }
            LexError::InvalidNumber { text, line, column } => {
                write!(f, "Invalid number '{}' at line {}, column {}", text, line, column)
            }
            LexError::InvalidCharacter { text, line, column } => {
                write!(f, "Invalid character '{}' at line {}, column {}", text, line, column)
            }
            LexError::UnexpectedEof { line, column } => {
                write!(f, "Unexpected end of file at line {}, column {}", line, column)
            }
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found, line, column } => {
                write!(f, "Expected {} but found '{}' at line {}, column {}", expected, found, line, column)
            }
            ParseError::UnexpectedEof { expected } => {
                write!(f, "Unexpected end of file, expected {}", expected)
            }
            ParseError::InvalidSpecialForm { form, reason, line, column } => {
                write!(f, "Invalid {} form: {} at line {}, column {}", form, reason, line, column)
            }
            ParseError::UnmatchedParenthesis { line, column } => {
                write!(f, "Unmatched parenthesis at line {}, column {}", line, column)
            }
            ParseError::InvalidBinding { reason, line, column } => {
                write!(f, "Invalid binding: {} at line {}, column {}", reason, line, column)
            }
        }
    }
}

impl std::error::Error for LexError {}
impl std::error::Error for ParseError {}