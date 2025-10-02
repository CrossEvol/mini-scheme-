use std::fmt;
use crate::compiler::CompileError;
use crate::vm::RuntimeError;

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

/// Comprehensive error type that encompasses all possible errors in the MiniScheme system
#[derive(Debug, Clone)]
pub enum MiniSchemeError {
    /// Lexical analysis error
    Lexical(LexError),
    /// Parsing error
    Parse(ParseError),
    /// Compilation error
    Compile(CompileError),
    /// Runtime error
    Runtime(RuntimeError),
    /// I/O error (file operations, etc.)
    Io(String),
    /// Internal error (should not happen in normal operation)
    Internal(String),
}

impl fmt::Display for MiniSchemeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MiniSchemeError::Lexical(err) => write!(f, "Lexical error: {}", err),
            MiniSchemeError::Parse(err) => write!(f, "Parse error: {}", err),
            MiniSchemeError::Compile(err) => write!(f, "Compile error: {}", err),
            MiniSchemeError::Runtime(err) => write!(f, "Runtime error: {}", err),
            MiniSchemeError::Io(msg) => write!(f, "I/O error: {}", msg),
            MiniSchemeError::Internal(msg) => write!(f, "Internal error: {}", msg),
        }
    }
}

impl std::error::Error for MiniSchemeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            MiniSchemeError::Lexical(err) => Some(err),
            MiniSchemeError::Parse(err) => Some(err),
            MiniSchemeError::Compile(err) => Some(err),
            MiniSchemeError::Runtime(err) => Some(err),
            _ => None,
        }
    }
}

impl From<LexError> for MiniSchemeError {
    fn from(err: LexError) -> Self {
        MiniSchemeError::Lexical(err)
    }
}

impl From<ParseError> for MiniSchemeError {
    fn from(err: ParseError) -> Self {
        MiniSchemeError::Parse(err)
    }
}

impl From<CompileError> for MiniSchemeError {
    fn from(err: CompileError) -> Self {
        MiniSchemeError::Compile(err)
    }
}

impl From<RuntimeError> for MiniSchemeError {
    fn from(err: RuntimeError) -> Self {
        MiniSchemeError::Runtime(err)
    }
}

impl From<std::io::Error> for MiniSchemeError {
    fn from(err: std::io::Error) -> Self {
        MiniSchemeError::Io(err.to_string())
    }
}

/// Error context for providing additional information about where errors occurred
#[derive(Debug, Clone)]
pub struct ErrorContext {
    pub filename: Option<String>,
    pub source_line: Option<String>,
    pub line: usize,
    pub column: usize,
    pub context_description: Option<String>,
}

impl ErrorContext {
    pub fn new(filename: Option<String>, line: usize, column: usize) -> Self {
        ErrorContext {
            filename,
            source_line: None,
            line,
            column,
            context_description: None,
        }
    }

    pub fn with_source_line(mut self, source_line: String) -> Self {
        self.source_line = Some(source_line);
        self
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context_description = Some(context);
        self
    }
}

/// Enhanced error reporting with context and suggestions
pub struct ErrorReporter {
    pub show_context: bool,
    pub show_suggestions: bool,
    pub max_context_lines: usize,
}

impl Default for ErrorReporter {
    fn default() -> Self {
        ErrorReporter {
            show_context: true,
            show_suggestions: true,
            max_context_lines: 3,
        }
    }
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self::default()
    }

    /// Report an error with full context and formatting
    pub fn report_error(&self, error: &MiniSchemeError, context: Option<&ErrorContext>) {
        eprintln!("{}", self.format_error(error, context));
    }

    /// Format an error with context for display
    pub fn format_error(&self, error: &MiniSchemeError, context: Option<&ErrorContext>) -> String {
        let mut output = String::new();

        // Add error header
        if let Some(ctx) = context {
            if let Some(filename) = &ctx.filename {
                output.push_str(&format!("Error in {}", filename));
            } else {
                output.push_str("Error");
            }
            output.push_str(&format!(" at line {}, column {}:\n", ctx.line, ctx.column));
        } else {
            output.push_str("Error:\n");
        }

        // Add main error message
        output.push_str(&format!("  {}\n", error));

        // Add source context if available
        if self.show_context {
            if let Some(ctx) = context {
                if let Some(source_line) = &ctx.source_line {
                    output.push_str("\n");
                    output.push_str(&format!("  {} | {}\n", ctx.line, source_line));
                    
                    // Add pointer to error location
                    let pointer_line = format!("  {} | {}{}", 
                        " ".repeat(ctx.line.to_string().len()),
                        " ".repeat(ctx.column.saturating_sub(1)),
                        "^"
                    );
                    output.push_str(&pointer_line);
                    output.push('\n');
                }

                if let Some(context_desc) = &ctx.context_description {
                    output.push_str(&format!("\n  Context: {}\n", context_desc));
                }
            }
        }

        // Add suggestions if available and enabled
        if self.show_suggestions {
            match error {
                MiniSchemeError::Parse(ParseError::InvalidSpecialForm { suggestion: Some(s), .. }) |
                MiniSchemeError::Parse(ParseError::InvalidBinding { suggestion: Some(s), .. }) |
                MiniSchemeError::Parse(ParseError::InvalidSyntax { suggestion: Some(s), .. }) => {
                    output.push_str(&format!("\n  Suggestion: {}\n", s));
                }
                _ => {}
            }
        }

        output
    }

    /// Report multiple errors (useful for batch processing)
    pub fn report_errors(&self, errors: &[(MiniSchemeError, Option<ErrorContext>)]) {
        for (i, (error, context)) in errors.iter().enumerate() {
            if i > 0 {
                eprintln!(); // Add blank line between errors
            }
            self.report_error(error, context.as_ref());
        }
    }
}

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

    /// Get the line number for this error
    pub fn line(&self) -> Option<usize> {
        match self {
            ParseError::UnexpectedToken { line, .. } |
            ParseError::InvalidSpecialForm { line, .. } |
            ParseError::UnmatchedParenthesis { line, .. } |
            ParseError::InvalidBinding { line, .. } |
            ParseError::TooManyArguments { line, .. } |
            ParseError::TooFewArguments { line, .. } |
            ParseError::InvalidSyntax { line, .. } => Some(*line),
            ParseError::UnexpectedEof { .. } => None,
        }
    }

    /// Get the column number for this error
    pub fn column(&self) -> Option<usize> {
        match self {
            ParseError::UnexpectedToken { column, .. } |
            ParseError::InvalidSpecialForm { column, .. } |
            ParseError::UnmatchedParenthesis { column, .. } |
            ParseError::InvalidBinding { column, .. } |
            ParseError::TooManyArguments { column, .. } |
            ParseError::TooFewArguments { column, .. } |
            ParseError::InvalidSyntax { column, .. } => Some(*column),
            ParseError::UnexpectedEof { .. } => None,
        }
    }
}

impl LexError {
    /// Get the line number for this error
    pub fn line(&self) -> usize {
        match self {
            LexError::UnexpectedCharacter { line, .. } |
            LexError::UnterminatedString { line, .. } |
            LexError::InvalidNumber { line, .. } |
            LexError::InvalidCharacter { line, .. } |
            LexError::UnexpectedEof { line, .. } => *line,
        }
    }

    /// Get the column number for this error
    pub fn column(&self) -> usize {
        match self {
            LexError::UnexpectedCharacter { column, .. } |
            LexError::UnterminatedString { column, .. } |
            LexError::InvalidNumber { column, .. } |
            LexError::InvalidCharacter { column, .. } |
            LexError::UnexpectedEof { column, .. } => *column,
        }
    }
}