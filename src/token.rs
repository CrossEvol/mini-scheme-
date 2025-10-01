use std::fmt;

/// Represents all possible tokens in the MiniScheme language
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
    QuoteKeyword,   // quote (keyword form)
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
    
    // Hashtable Operations
    MakeHashtable,   // make-hashtable
    HashtableSet,    // hashtable-set!
    HashtableRef,    // hashtable-ref
    HashtableDelete, // hashtable-delete!
    
    // Hash Functions
    StringHash,      // string-hash
    EqualHash,       // equal-hash
    
    // Additional Equality Predicates
    EqualQ,          // equal?
    
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
    
    // Arithmetic Operations
    Plus,           // +
    Minus,          // -
    Multiply,       // *
    Divide,         // /
    
    // Comparison Operations
    Equal,          // =
    LessThan,       // <
    LessThanEqual,  // <=
    GreaterThan,    // >
    GreaterThanEqual, // >=
    
    // Punctuation
    LeftParen,
    RightParen,
    QuoteMark,      // '
    BackQuote,      // `
    Comma,          // ,
    CommaAt,        // ,@
    VectorPrefix,   // # (for vectors)
    Dot,            // . (for dotted pairs)
    
    // Special Values
    Null,           // null
    
    // Special
    Eof,
}

/// Token with position information for error reporting
#[derive(Debug, PartialEq, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
    pub column: usize,
    pub span: (usize, usize), // start and end positions in source
}

impl TokenInfo {
    pub fn new(token: Token, line: usize, column: usize, start: usize, end: usize) -> Self {
        Self {
            token,
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
        Self::new(Token::Eof, line, column, position, position)
    }
    
    /// Check if this token is a literal value
    pub fn is_literal(&self) -> bool {
        matches!(
            self.token,
            Token::Number(_) | Token::String(_) | Token::Character(_) | Token::Boolean(_)
        )
    }
    
    /// Check if this token is an identifier
    pub fn is_identifier(&self) -> bool {
        matches!(self.token, Token::Identifier(_))
    }
    
    /// Check if this token is a keyword or special form
    pub fn is_keyword(&self) -> bool {
        matches!(
            self.token,
            Token::Define | Token::Lambda | Token::If | Token::Cond | Token::Else
            | Token::Let | Token::LetStar | Token::LetLoop | Token::LetValues
            | Token::CallWithValues | Token::Import | Token::Begin | Token::SetBang
            | Token::QuoteKeyword | Token::QuasiQuote | Token::UnQuote | Token::UnQuoteSplicing
        )
    }
    
    /// Check if this token is a built-in procedure
    pub fn is_builtin(&self) -> bool {
        matches!(
            self.token,
            Token::Car | Token::Cdr | Token::Cons | Token::List | Token::Vector
            | Token::Display | Token::Error | Token::Values | Token::ForEach
            | Token::MakeHashtable | Token::HashtableSet | Token::HashtableRef | Token::HashtableDelete
            | Token::StringHash | Token::EqualHash | Token::EqualQ
            | Token::Plus | Token::Minus | Token::Multiply | Token::Divide
            | Token::Equal | Token::LessThan | Token::LessThanEqual | Token::GreaterThan | Token::GreaterThanEqual
        )
    }
    
    /// Check if this token is a predicate (ends with ?)
    pub fn is_predicate(&self) -> bool {
        matches!(
            self.token,
            Token::HashtableQ | Token::StringQ | Token::NumberQ | Token::BooleanQ
            | Token::CharQ | Token::CharNumericQ | Token::CharWhitespaceQ | Token::NullQ
            | Token::PairQ | Token::EqQ | Token::CharEqQ | Token::StringEqQ | Token::EqualQ
        )
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals
            Token::Identifier(s) => write!(f, "identifier '{}'", s),
            Token::Number(n) => write!(f, "number {}", n),
            Token::String(s) => write!(f, "string \"{}\"", s),
            Token::Character(c) => write!(f, "character #\\{}", c),
            Token::Boolean(b) => write!(f, "boolean #{}", if *b { "t" } else { "f" }),
            
            // Keywords and Special Forms
            Token::Define => write!(f, "define"),
            Token::Lambda => write!(f, "lambda"),
            Token::If => write!(f, "if"),
            Token::Cond => write!(f, "cond"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::LetStar => write!(f, "let*"),
            Token::LetLoop => write!(f, "let loop"),
            Token::LetValues => write!(f, "let-values"),
            Token::CallWithValues => write!(f, "call-with-values"),
            Token::Import => write!(f, "import"),
            Token::Begin => write!(f, "begin"),
            Token::SetBang => write!(f, "set!"),
            Token::QuoteKeyword => write!(f, "quote"),
            Token::QuasiQuote => write!(f, "quasiquote"),
            Token::UnQuote => write!(f, "unquote"),
            Token::UnQuoteSplicing => write!(f, "unquote-splicing"),
            
            // Built-in Procedures
            Token::Car => write!(f, "car"),
            Token::Cdr => write!(f, "cdr"),
            Token::Cons => write!(f, "cons"),
            Token::List => write!(f, "list"),
            Token::Vector => write!(f, "vector"),
            Token::Display => write!(f, "display"),
            Token::Error => write!(f, "error"),
            Token::Values => write!(f, "values"),
            Token::ForEach => write!(f, "for-each"),
            
            // Hashtable Operations
            Token::MakeHashtable => write!(f, "make-hashtable"),
            Token::HashtableSet => write!(f, "hashtable-set!"),
            Token::HashtableRef => write!(f, "hashtable-ref"),
            Token::HashtableDelete => write!(f, "hashtable-delete!"),
            
            // Hash Functions
            Token::StringHash => write!(f, "string-hash"),
            Token::EqualHash => write!(f, "equal-hash"),
            
            // Additional Equality Predicates
            Token::EqualQ => write!(f, "equal?"),
            
            // Predicates
            Token::HashtableQ => write!(f, "hashtable?"),
            Token::StringQ => write!(f, "string?"),
            Token::NumberQ => write!(f, "number?"),
            Token::BooleanQ => write!(f, "boolean?"),
            Token::CharQ => write!(f, "char?"),
            Token::CharNumericQ => write!(f, "char-numeric?"),
            Token::CharWhitespaceQ => write!(f, "char-whitespace?"),
            Token::NullQ => write!(f, "null?"),
            Token::PairQ => write!(f, "pair?"),
            Token::EqQ => write!(f, "eq?"),
            Token::CharEqQ => write!(f, "char=?"),
            Token::StringEqQ => write!(f, "string=?"),
            
            // Type Conversions
            Token::StringToNumber => write!(f, "string->number"),
            Token::ListToString => write!(f, "list->string"),
            Token::ListToVector => write!(f, "list->vector"),
            Token::VectorToList => write!(f, "vector->list"),
            
            // Arithmetic Operations
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            
            // Comparison Operations
            Token::Equal => write!(f, "="),
            Token::LessThan => write!(f, "<"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterThanEqual => write!(f, ">="),
            
            // Punctuation
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::QuoteMark => write!(f, "'"),
            Token::BackQuote => write!(f, "`"),
            Token::Comma => write!(f, ","),
            Token::CommaAt => write!(f, ",@"),
            Token::VectorPrefix => write!(f, "#"),
            Token::Dot => write!(f, "."),
            
            // Special Values
            Token::Null => write!(f, "null"),
            
            // Special
            Token::Eof => write!(f, "end of file"),
        }
    }
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line {}, column {}", self.token, self.line, self.column)
    }
}