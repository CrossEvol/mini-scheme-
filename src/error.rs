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
        column: usize,
        context: Option<String>,
    },
    UnexpectedEof { 
        expected: String,
        context: Option<String>,
    },
    InvalidSpecialForm { 
        form: String, 
        reason: String, 
        line: usize, 
        column: usize,
        suggestion: Option<String>,
    },
    UnmatchedParenthesis { 
        line: usize, 
        column: usize,
        paren_type: String,
    },
    InvalidBinding { 
        reason: String, 
        line: usize, 
        column: usize,
        suggestion: Option<String>,
    },
    TooManyArguments {
        form: String,
        expected: usize,
        found: usize,
        line: usize,
        column: usize,
    },
    TooFewArguments {
        form: String,
        expected: usize,
        found: usize,
        line: usize,
        column: usize,
    },
    InvalidSyntax {
        message: String,
        line: usize,
        column: usize,
        suggestion: Option<String>,
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
            ParseError::UnexpectedToken { expected, found, line, column, context } => {
                write!(f, "Expected {} but found '{}' at line {}, column {}", expected, found, line, column)?;
                if let Some(ctx) = context {
                    write!(f, " (in {})", ctx)?;
                }
                Ok(())
            }
            ParseError::UnexpectedEof { expected, context } => {
                write!(f, "Unexpected end of file, expected {}", expected)?;
                if let Some(ctx) = context {
                    write!(f, " (in {})", ctx)?;
                }
                Ok(())
            }
            ParseError::InvalidSpecialForm { form, reason, line, column, suggestion } => {
                write!(f, "Invalid {} form: {} at line {}, column {}", form, reason, line, column)?;
                if let Some(hint) = suggestion {
                    write!(f, "\n  Suggestion: {}", hint)?;
                }
                Ok(())
            }
            ParseError::UnmatchedParenthesis { line, column, paren_type } => {
                write!(f, "Unmatched {} at line {}, column {}", paren_type, line, column)
            }
            ParseError::InvalidBinding { reason, line, column, suggestion } => {
                write!(f, "Invalid binding: {} at line {}, column {}", reason, line, column)?;
                if let Some(hint) = suggestion {
                    write!(f, "\n  Suggestion: {}", hint)?;
                }
                Ok(())
            }
            ParseError::TooManyArguments { form, expected, found, line, column } => {
                write!(f, "Too many arguments for {}: expected {}, found {} at line {}, column {}", 
                       form, expected, found, line, column)
            }
            ParseError::TooFewArguments { form, expected, found, line, column } => {
                write!(f, "Too few arguments for {}: expected {}, found {} at line {}, column {}", 
                       form, expected, found, line, column)
            }
            ParseError::InvalidSyntax { message, line, column, suggestion } => {
                write!(f, "Syntax error: {} at line {}, column {}", message, line, column)?;
                if let Some(hint) = suggestion {
                    write!(f, "\n  Suggestion: {}", hint)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for LexError {}
impl std::error::Error for ParseError {}

impl ParseError {
    /// Create an UnexpectedToken error with optional context
    pub fn unexpected_token(expected: String, found: String, line: usize, column: usize, context: Option<String>) -> Self {
        ParseError::UnexpectedToken { expected, found, line, column, context }
    }

    /// Create an UnexpectedEof error with optional context
    pub fn unexpected_eof(expected: String, context: Option<String>) -> Self {
        ParseError::UnexpectedEof { expected, context }
    }

    /// Create an InvalidSpecialForm error with optional suggestion
    pub fn invalid_special_form(form: String, reason: String, line: usize, column: usize, suggestion: Option<String>) -> Self {
        ParseError::InvalidSpecialForm { form, reason, line, column, suggestion }
    }

    /// Create an UnmatchedParenthesis error
    pub fn unmatched_parenthesis(line: usize, column: usize, paren_type: String) -> Self {
        ParseError::UnmatchedParenthesis { line, column, paren_type }
    }

    /// Create an InvalidBinding error with optional suggestion
    pub fn invalid_binding(reason: String, line: usize, column: usize, suggestion: Option<String>) -> Self {
        ParseError::InvalidBinding { reason, line, column, suggestion }
    }
}