use std::fmt;

/// Represents all possible tokens in the MiniScheme language
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
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
    SetBang,         // set!
    QuoteKeyword,    // quote (keyword form)
    QuasiQuote,      // quasiquote
    UnQuote,         // unquote
    UnQuoteSplicing, // unquote-splicing

    // Built-in Procedures
    Car,
    Cdr,
    Cons,
    List,
    Vector,
    Display,
    Newline,
    Error,
    Values,
    ForEach,

    // Type Predicates
    NullQ,    // null?
    PairQ,    // pair?
    NumberQ,  // number?
    StringQ,  // string?
    BooleanQ, // boolean?
    CharQ,    // char?

    // Hashtable Operations
    MakeHashtable,   // make-hashtable
    HashtableSet,    // hashtable-set!
    HashtableRef,    // hashtable-ref
    HashtableDelete, // hashtable-delete!

    // Hash Functions
    StringHash, // string-hash
    EqualHash,  // equal-hash

    // Additional Equality Predicates
    EqualQ, // equal?

    // Predicates
    HashtableQ,      // hashtable?
    CharNumericQ,    // char-numeric?
    CharWhitespaceQ, // char-whitespace?
    EqQ,             // eq?
    CharEqQ,         // char=?
    StringEqQ,       // string=?

    // Type Conversions
    StringToNumber, // string->number
    ListToString,   // list->string
    ListToVector,   // list->vector
    VectorToList,   // vector->list

    // Vector Operations
    VectorQ,      // vector?
    VectorLength, // vector-length
    VectorRef,    // vector-ref
    VectorSet,    // vector-set!

    // Arithmetic Operations
    Plus,     // +
    Minus,    // -
    Multiply, // *
    Divide,   // /

    // Comparison Operations
    Equal,            // =
    LessThan,         // <
    LessThanEqual,    // <=
    GreaterThan,      // >
    GreaterThanEqual, // >=

    // Punctuation
    LeftParen,
    RightParen,
    QuoteMark,    // '
    BackQuote,    // `
    Comma,        // ,
    CommaAt,      // ,@
    VectorPrefix, // # (for vectors)
    Dot,          // . (for dotted pairs)

    // Special Values
    Null, // null

    // Special
    Eof,
}

/// Token with position information for error reporting
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
    pub span: (usize, usize), // start and end positions in source
}

impl Token {
    pub fn new(token: TokenType, line: usize, column: usize, start: usize, end: usize) -> Self {
        Self {
            token_type: token,
            line,
            column,
            span: (start, end),
        }
    }

    /// Get the length of the token in the source
    pub fn len(&self) -> usize {
        self.span.1 - self.span.0
    }

    /// Check if this token is empty (shouldn't happen in practice)
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get the start position of the token
    pub fn start(&self) -> usize {
        self.span.0
    }

    /// Get the end position of the token
    pub fn end(&self) -> usize {
        self.span.1
    }

    /// Create a TokenInfo for EOF at the given position
    pub fn eof(line: usize, column: usize, position: usize) -> Self {
        Self::new(TokenType::Eof, line, column, position, position)
    }

    /// Check if this token is a literal value
    pub fn is_literal(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Number(_)
                | TokenType::String(_)
                | TokenType::Character(_)
                | TokenType::Boolean(_)
        )
    }

    /// Check if this token is an identifier
    pub fn is_identifier(&self) -> bool {
        matches!(self.token_type, TokenType::Identifier(_))
    }

    /// Check if this token is a keyword or special form
    pub fn is_keyword(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Define
                | TokenType::Lambda
                | TokenType::If
                | TokenType::Cond
                | TokenType::Else
                | TokenType::Let
                | TokenType::LetStar
                | TokenType::LetLoop
                | TokenType::LetValues
                | TokenType::CallWithValues
                | TokenType::Import
                | TokenType::Begin
                | TokenType::SetBang
                | TokenType::QuoteKeyword
                | TokenType::QuasiQuote
                | TokenType::UnQuote
                | TokenType::UnQuoteSplicing
        )
    }

    /// Check if this token is a built-in procedure
    pub fn is_builtin(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Car
                | TokenType::Cdr
                | TokenType::Cons
                | TokenType::List
                | TokenType::Vector
                | TokenType::Display
                | TokenType::Newline
                | TokenType::Error
                | TokenType::Values
                | TokenType::ForEach
                | TokenType::NullQ
                | TokenType::PairQ
                | TokenType::NumberQ
                | TokenType::StringQ
                | TokenType::BooleanQ
                | TokenType::CharQ
                | TokenType::MakeHashtable
                | TokenType::HashtableSet
                | TokenType::HashtableRef
                | TokenType::HashtableDelete
                | TokenType::StringHash
                | TokenType::EqualHash
                | TokenType::EqualQ
                | TokenType::Plus
                | TokenType::Minus
                | TokenType::Multiply
                | TokenType::Divide
                | TokenType::Equal
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual
        )
    }

    /// Check if this token is a predicate (ends with ?)
    pub fn is_predicate(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::HashtableQ
                | TokenType::StringQ
                | TokenType::NumberQ
                | TokenType::BooleanQ
                | TokenType::CharQ
                | TokenType::CharNumericQ
                | TokenType::CharWhitespaceQ
                | TokenType::NullQ
                | TokenType::PairQ
                | TokenType::EqQ
                | TokenType::CharEqQ
                | TokenType::StringEqQ
                | TokenType::EqualQ
        )
    }
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals
            TokenType::Identifier(s) => write!(f, "identifier '{}'", s),
            TokenType::Number(n) => write!(f, "number {}", n),
            TokenType::String(s) => write!(f, "string \"{}\"", s),
            TokenType::Character(c) => write!(f, "character #\\{}", c),
            TokenType::Boolean(b) => write!(f, "boolean #{}", if *b { "t" } else { "f" }),

            // Keywords and Special Forms
            TokenType::Define => write!(f, "define"),
            TokenType::Lambda => write!(f, "lambda"),
            TokenType::If => write!(f, "if"),
            TokenType::Cond => write!(f, "cond"),
            TokenType::Else => write!(f, "else"),
            TokenType::Let => write!(f, "let"),
            TokenType::LetStar => write!(f, "let*"),
            TokenType::LetLoop => write!(f, "let loop"),
            TokenType::LetValues => write!(f, "let-values"),
            TokenType::CallWithValues => write!(f, "call-with-values"),
            TokenType::Import => write!(f, "import"),
            TokenType::Begin => write!(f, "begin"),
            TokenType::SetBang => write!(f, "set!"),
            TokenType::QuoteKeyword => write!(f, "quote"),
            TokenType::QuasiQuote => write!(f, "quasiquote"),
            TokenType::UnQuote => write!(f, "unquote"),
            TokenType::UnQuoteSplicing => write!(f, "unquote-splicing"),

            // Built-in Procedures
            TokenType::Car => write!(f, "car"),
            TokenType::Cdr => write!(f, "cdr"),
            TokenType::Cons => write!(f, "cons"),
            TokenType::List => write!(f, "list"),
            TokenType::Vector => write!(f, "vector"),
            TokenType::Display => write!(f, "display"),
            TokenType::Newline => write!(f, "newline"),
            TokenType::Error => write!(f, "error"),
            TokenType::Values => write!(f, "values"),
            TokenType::ForEach => write!(f, "for-each"),

            // Type Predicates
            TokenType::NullQ => write!(f, "null?"),
            TokenType::PairQ => write!(f, "pair?"),
            TokenType::NumberQ => write!(f, "number?"),
            TokenType::StringQ => write!(f, "string?"),
            TokenType::BooleanQ => write!(f, "boolean?"),
            TokenType::CharQ => write!(f, "char?"),

            // Hashtable Operations
            TokenType::MakeHashtable => write!(f, "make-hashtable"),
            TokenType::HashtableSet => write!(f, "hashtable-set!"),
            TokenType::HashtableRef => write!(f, "hashtable-ref"),
            TokenType::HashtableDelete => write!(f, "hashtable-delete!"),

            // Hash Functions
            TokenType::StringHash => write!(f, "string-hash"),
            TokenType::EqualHash => write!(f, "equal-hash"),

            // Additional Equality Predicates
            TokenType::EqualQ => write!(f, "equal?"),

            // Predicates
            TokenType::HashtableQ => write!(f, "hashtable?"),
            TokenType::CharNumericQ => write!(f, "char-numeric?"),
            TokenType::CharWhitespaceQ => write!(f, "char-whitespace?"),
            TokenType::EqQ => write!(f, "eq?"),
            TokenType::CharEqQ => write!(f, "char=?"),
            TokenType::StringEqQ => write!(f, "string=?"),

            // Type Conversions
            TokenType::StringToNumber => write!(f, "string->number"),
            TokenType::ListToString => write!(f, "list->string"),
            TokenType::ListToVector => write!(f, "list->vector"),
            TokenType::VectorToList => write!(f, "vector->list"),

            // Vector Operations
            TokenType::VectorQ => write!(f, "vector?"),
            TokenType::VectorLength => write!(f, "vector-length"),
            TokenType::VectorRef => write!(f, "vector-ref"),
            TokenType::VectorSet => write!(f, "vector-set!"),

            // Arithmetic Operations
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Multiply => write!(f, "*"),
            TokenType::Divide => write!(f, "/"),

            // Comparison Operations
            TokenType::Equal => write!(f, "="),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LessThanEqual => write!(f, "<="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::GreaterThanEqual => write!(f, ">="),

            // Punctuation
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::QuoteMark => write!(f, "'"),
            TokenType::BackQuote => write!(f, "`"),
            TokenType::Comma => write!(f, ","),
            TokenType::CommaAt => write!(f, ",@"),
            TokenType::VectorPrefix => write!(f, "#"),
            TokenType::Dot => write!(f, "."),

            // Special Values
            TokenType::Null => write!(f, "null"),

            // Special
            TokenType::Eof => write!(f, "end of file"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.token_type, self.line, self.column
        )
    }
}
