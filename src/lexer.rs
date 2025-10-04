use crate::error::LexError;
use crate::token::{Token, TokenType};

/// Lexer for tokenizing MiniScheme source code
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    /// Create a new lexer for the given input string
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenize the entire input and return a vector of tokens
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);

            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    /// Get the next token from the input
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        // Skip whitespace and comments
        self.skip_whitespace();

        let start_pos = self.position;
        let line = self.line;
        let column = self.column;

        match self.current_char() {
            None => Ok(Token::new(
                TokenType::Eof,
                line,
                column,
                start_pos,
                self.position,
            )),
            Some(ch) => {
                let token = match ch {
                    // Numbers (including negative numbers)
                    '0'..='9' => self.read_number()?,
                    '+' | '-' if self.peek_char().map_or(false, |c| c.is_ascii_digit()) => {
                        self.read_number()?
                    }
                    // Strings
                    '"' => self.read_string()?,
                    // Characters and booleans (both start with #)
                    '#' => self.read_hash_literal()?,
                    // Punctuation (must come before identifier check)
                    '(' => {
                        self.advance();
                        TokenType::LeftParen
                    }
                    ')' => {
                        self.advance();
                        TokenType::RightParen
                    }
                    '\'' => {
                        self.advance();
                        TokenType::QuoteMark
                    }
                    '`' => {
                        self.advance();
                        TokenType::BackQuote
                    }
                    ',' => {
                        self.advance();
                        // Check for ,@ (comma-at)
                        if self.current_char() == Some('@') {
                            self.advance();
                            TokenType::CommaAt
                        } else {
                            TokenType::Comma
                        }
                    }
                    '.' => {
                        self.advance();
                        TokenType::Dot
                    }
                    // Identifiers and keywords (after punctuation)
                    _ if self.is_identifier_start(ch) => self.read_identifier(),
                    // Other characters
                    _ => {
                        self.advance();
                        return Err(LexError::UnexpectedCharacter {
                            char: ch,
                            line,
                            column,
                        });
                    }
                };

                Ok(Token::new(token, line, column, start_pos, self.position))
            }
        }
    }

    /// Get the current character without advancing
    fn current_char(&self) -> Option<char> {
        self.input.get(self.position).copied()
    }

    /// Peek at the next character without advancing
    fn peek_char(&self) -> Option<char> {
        self.input.get(self.position + 1).copied()
    }

    /// Advance to the next character
    fn advance(&mut self) {
        if let Some(ch) = self.current_char() {
            self.position += 1;
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == ';' {
                self.skip_comment();
            } else {
                break;
            }
        }
    }

    /// Skip a comment (from ; to end of line)
    fn skip_comment(&mut self) {
        while let Some(ch) = self.current_char() {
            self.advance();
            if ch == '\n' {
                break;
            }
        }
    }

    /// Check if a character can start an identifier
    fn is_identifier_start(&self, ch: char) -> bool {
        ch.is_alphabetic() || "+-*/!?=<>#".contains(ch)
    }

    /// Check if a character can continue an identifier
    fn is_identifier_continue(&self, ch: char) -> bool {
        ch.is_alphanumeric() || "+-*/!?=<>-#".contains(ch)
    }

    /// Read an identifier from the input
    fn read_identifier(&mut self) -> TokenType {
        let mut identifier = String::new();

        while let Some(ch) = self.current_char() {
            if self.is_identifier_continue(ch) {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        self.keyword_or_identifier(identifier)
    }

    /// Convert identifier string to keyword token or return as identifier
    fn keyword_or_identifier(&self, identifier: String) -> TokenType {
        match identifier.as_str() {
            // Keywords and Special Forms
            "define" => TokenType::Define,
            "lambda" => TokenType::Lambda,
            "if" => TokenType::If,
            "cond" => TokenType::Cond,
            "else" => TokenType::Else,
            "let" => TokenType::Let,
            "let*" => TokenType::LetStar,
            "let-values" => TokenType::LetValues,
            "call-with-values" => TokenType::CallWithValues,
            "import" => TokenType::Import,
            "begin" => TokenType::Begin,
            "set!" => TokenType::SetBang,
            "quote" => TokenType::QuoteKeyword,
            "quasiquote" => TokenType::QuasiQuote,
            "unquote" => TokenType::UnQuote,
            "unquote-splicing" => TokenType::UnQuoteSplicing,

            // Built-in Procedures
            "car" => TokenType::Car,
            "cdr" => TokenType::Cdr,
            "cons" => TokenType::Cons,
            "list" => TokenType::List,
            "vector" => TokenType::Vector,
            "display" => TokenType::Display,
            "newline" => TokenType::Newline,
            "error" => TokenType::Error,
            "values" => TokenType::Values,
            "for-each" => TokenType::ForEach,

            // Type Predicates
            "null?" => TokenType::NullQ,
            "pair?" => TokenType::PairQ,
            "number?" => TokenType::NumberQ,
            "string?" => TokenType::StringQ,
            "boolean?" => TokenType::BooleanQ,
            "char?" => TokenType::CharQ,

            // Hashtable Operations
            "make-hashtable" => TokenType::MakeHashtable,
            "hashtable-set!" => TokenType::HashtableSet,
            "hashtable-ref" => TokenType::HashtableRef,
            "hashtable-delete!" => TokenType::HashtableDelete,

            // Hash Functions
            "string-hash" => TokenType::StringHash,
            "equal-hash" => TokenType::EqualHash,

            // Predicates
            "equal?" => TokenType::EqualQ,
            "hashtable?" => TokenType::HashtableQ,
            "char-numeric?" => TokenType::CharNumericQ,
            "char-whitespace?" => TokenType::CharWhitespaceQ,
            "eq?" => TokenType::EqQ,
            "char=?" => TokenType::CharEqQ,
            "string=?" => TokenType::StringEqQ,

            // Type Conversions
            "string->number" => TokenType::StringToNumber,
            "list->string" => TokenType::ListToString,
            "list->vector" => TokenType::ListToVector,
            "vector->list" => TokenType::VectorToList,

            // Vector Operations
            "vector?" => TokenType::VectorQ,
            "vector-length" => TokenType::VectorLength,
            "vector-ref" => TokenType::VectorRef,
            "vector-set!" => TokenType::VectorSet,

            // Arithmetic Operations
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Multiply,
            "/" => TokenType::Divide,

            // Comparison Operations
            "=" => TokenType::Equal,
            "<" => TokenType::LessThan,
            "<=" => TokenType::LessThanEqual,
            ">" => TokenType::GreaterThan,
            ">=" => TokenType::GreaterThanEqual,

            // Special Values
            "null" => TokenType::Null,

            // Default case - return as identifier
            _ => TokenType::Identifier(identifier),
        }
    }

    /// Read a number from the input (integers and floating-point numbers)
    fn read_number(&mut self) -> Result<TokenType, LexError> {
        let mut number_str = String::new();
        let start_line = self.line;
        let start_column = self.column;

        // Handle optional sign
        if let Some(ch) = self.current_char() {
            if ch == '+' || ch == '-' {
                number_str.push(ch);
                self.advance();
            }
        }

        // Read integer part
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                number_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point
        let mut has_decimal = false;
        if self.current_char() == Some('.')
            && self.peek_char().map_or(false, |c| c.is_ascii_digit())
        {
            has_decimal = true;
            number_str.push('.');
            self.advance();

            // Read fractional part
            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    number_str.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Check for invalid number format (multiple decimal points)
        if has_decimal
            && self.current_char() == Some('.')
            && self.peek_char().map_or(false, |c| c.is_ascii_digit())
        {
            // This would be a second decimal point followed by digits - invalid
            return Err(LexError::InvalidNumber {
                text: format!("{}.", number_str),
                line: start_line,
                column: start_column,
            });
        }

        // Parse the number
        match number_str.parse::<f64>() {
            Ok(num) => Ok(TokenType::Number(num)),
            Err(_) => Err(LexError::InvalidNumber {
                text: number_str,
                line: start_line,
                column: start_column,
            }),
        }
    }

    /// Read a string literal from the input
    fn read_string(&mut self) -> Result<TokenType, LexError> {
        let start_line = self.line;
        let start_column = self.column;
        let mut string_value = String::new();

        // Skip opening quote
        self.advance();

        while let Some(ch) = self.current_char() {
            match ch {
                '"' => {
                    // End of string
                    self.advance();
                    return Ok(TokenType::String(string_value));
                }
                '\\' => {
                    // Escape sequence
                    self.advance();
                    match self.current_char() {
                        Some('n') => {
                            string_value.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            string_value.push('\t');
                            self.advance();
                        }
                        Some('r') => {
                            string_value.push('\r');
                            self.advance();
                        }
                        Some('\\') => {
                            string_value.push('\\');
                            self.advance();
                        }
                        Some('"') => {
                            string_value.push('"');
                            self.advance();
                        }
                        Some(other) => {
                            // Invalid escape sequence - just include the character
                            string_value.push(other);
                            self.advance();
                        }
                        None => {
                            return Err(LexError::UnterminatedString {
                                line: start_line,
                                column: start_column,
                            });
                        }
                    }
                }
                _ => {
                    string_value.push(ch);
                    self.advance();
                }
            }
        }

        // Reached EOF without closing quote
        Err(LexError::UnterminatedString {
            line: start_line,
            column: start_column,
        })
    }

    /// Read hash-prefixed literals (#t, #f, #\char, #( for vectors)
    fn read_hash_literal(&mut self) -> Result<TokenType, LexError> {
        let start_line = self.line;
        let start_column = self.column;

        // Skip the #
        self.advance();

        match self.current_char() {
            Some('t') => {
                self.advance();
                Ok(TokenType::Boolean(true))
            }
            Some('f') => {
                self.advance();
                Ok(TokenType::Boolean(false))
            }
            Some('\\') => {
                // Character literal
                self.advance();
                self.read_character(start_line, start_column)
            }
            Some('(') => {
                // Vector prefix - don't consume the '(' as it will be handled separately
                Ok(TokenType::VectorPrefix)
            }
            Some(other) => Err(LexError::InvalidCharacter {
                text: format!("#{}", other),
                line: start_line,
                column: start_column,
            }),
            None => Err(LexError::UnexpectedEof {
                line: start_line,
                column: start_column,
            }),
        }
    }

    /// Read a character literal after #\
    fn read_character(
        &mut self,
        start_line: usize,
        start_column: usize,
    ) -> Result<TokenType, LexError> {
        match self.current_char() {
            Some(ch) => {
                // Check for special character names
                let mut char_name = String::new();
                let mut temp_pos = self.position;

                // Read potential character name
                while temp_pos < self.input.len() {
                    let c = self.input[temp_pos];
                    if c.is_alphabetic() {
                        char_name.push(c);
                        temp_pos += 1;
                    } else {
                        break;
                    }
                }

                // Check for special character names
                let character = match char_name.as_str() {
                    "space" => {
                        // Advance past the word "space"
                        for _ in 0..5 {
                            self.advance();
                        }
                        ' '
                    }
                    "newline" => {
                        // Advance past the word "newline"
                        for _ in 0..7 {
                            self.advance();
                        }
                        '\n'
                    }
                    "tab" => {
                        // Advance past the word "tab"
                        for _ in 0..3 {
                            self.advance();
                        }
                        '\t'
                    }
                    _ => {
                        // Single character
                        self.advance();
                        ch
                    }
                };

                Ok(TokenType::Character(character))
            }
            None => Err(LexError::UnexpectedEof {
                line: start_line,
                column: start_column,
            }),
        }
    }
}
