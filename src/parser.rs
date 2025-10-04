use crate::ast::Expr;
use crate::error::ParseError;
use crate::token::{Token, TokenType};

/// Parser for converting tokens into an Abstract Syntax Tree
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    /// Create a new parser with the given tokens
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    /// Parse the tokens into a vector of expressions with comprehensive error handling
    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut expressions = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            if let Some(token_info) = self.current_token() {
                if matches!(token_info.token_type, TokenType::Eof) {
                    break;
                }
            }

            match self.parse_expr() {
                Ok(expr) => expressions.push(expr),
                Err(error) => {
                    errors.push(error.clone());

                    // Attempt error recovery by synchronizing to the next top-level expression
                    if !self.synchronize() {
                        // If synchronization fails, return the first error
                        return Err(error);
                    }
                }
            }
        }

        // If we collected any errors during parsing, return the first one
        // In a more sophisticated implementation, we might collect all errors
        if let Some(first_error) = errors.into_iter().next() {
            return Err(first_error);
        }

        Ok(expressions)
    }

    /// Synchronize parser state after an error by advancing to a likely recovery point
    fn synchronize(&mut self) -> bool {
        // Skip tokens until we find a likely synchronization point
        while !self.is_at_end() {
            if let Some(token_info) = self.current_token() {
                match &token_info.token_type {
                    // Synchronize at top-level forms
                    TokenType::LeftParen => {
                        // Look ahead to see if this starts a top-level form
                        if let Some(next_token) = self.peek_token() {
                            match &next_token.token_type {
                                TokenType::Define
                                | TokenType::Lambda
                                | TokenType::If
                                | TokenType::Cond
                                | TokenType::Let
                                | TokenType::LetStar
                                | TokenType::LetValues
                                | TokenType::Begin
                                | TokenType::QuoteKeyword
                                | TokenType::QuasiQuote
                                | TokenType::CallWithValues
                                | TokenType::Import
                                | TokenType::SetBang => {
                                    return true;
                                }
                                _ => {}
                            }
                        }
                        return true; // Any left paren could start a new expression
                    }
                    TokenType::Eof => return false,
                    _ => {}
                }
            }
            self.advance();
        }
        false
    }

    /// Parse a single expression
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.current_token() {
            Some(token_info) => match &token_info.token_type {
                TokenType::LeftParen => self.parse_list(),
                TokenType::QuoteMark => self.parse_quote(),
                TokenType::BackQuote => self.parse_quasiquote(),
                TokenType::Comma => self.parse_unquote(),
                TokenType::CommaAt => self.parse_unquote_splicing(),
                TokenType::VectorPrefix => self.parse_vector(),
                _ => self.parse_atom(),
            },
            None => Err(ParseError::UnexpectedEof {
                expected: "expression".to_string(),
                context: None,
            }),
        }
    }

    /// Parse atomic expressions (literals and variables)
    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let token_info = match self.current_token() {
            Some(token_info) => token_info.clone(),
            None => {
                return Err(ParseError::UnexpectedEof {
                    expected: "atom".to_string(),
                    context: None,
                });
            }
        };

        self.advance();

        let token = Some(token_info.clone());
        match &token_info.token_type {
            TokenType::Number(n) => Ok(Expr::Number(*n, token)),
            TokenType::String(s) => Ok(Expr::String(s.clone(), token)),
            TokenType::Character(c) => Ok(Expr::Character(*c, token)),
            TokenType::Boolean(b) => Ok(Expr::Boolean(*b, token)),
            TokenType::Identifier(name) => Ok(Expr::Variable(name.clone(), token)),
            TokenType::Null => Ok(Expr::Variable("null".to_string(), token)), // null is a symbol

            // Handle built-in procedures and keywords as variables for now
            TokenType::Plus => Ok(Expr::Variable("+".to_string(), token)),
            TokenType::Minus => Ok(Expr::Variable("-".to_string(), token)),
            TokenType::Multiply => Ok(Expr::Variable("*".to_string(), token)),
            TokenType::Divide => Ok(Expr::Variable("/".to_string(), token)),
            TokenType::Equal => Ok(Expr::Variable("=".to_string(), token)),
            TokenType::LessThan => Ok(Expr::Variable("<".to_string(), token)),
            TokenType::LessThanEqual => Ok(Expr::Variable("<=".to_string(), token)),
            TokenType::GreaterThan => Ok(Expr::Variable(">".to_string(), token)),
            TokenType::GreaterThanEqual => Ok(Expr::Variable(">=".to_string(), token)),

            // Built-in procedures
            TokenType::Car => Ok(Expr::Variable("car".to_string(), token)),
            TokenType::Cdr => Ok(Expr::Variable("cdr".to_string(), token)),
            TokenType::Cons => Ok(Expr::Variable("cons".to_string(), token)),
            TokenType::List => Ok(Expr::Variable("list".to_string(), token)),
            TokenType::Vector => Ok(Expr::Variable("vector".to_string(), token)),
            TokenType::Display => Ok(Expr::Variable("display".to_string(), token)),
            TokenType::Newline => Ok(Expr::Variable("newline".to_string(), token)),
            TokenType::Error => Ok(Expr::Variable("error".to_string(), token)),
            TokenType::Values => Ok(Expr::Variable("values".to_string(), token)),
            TokenType::ForEach => Ok(Expr::Variable("for-each".to_string(), token)),

            // Predicates
            TokenType::HashtableQ => Ok(Expr::Variable("hashtable?".to_string(), token)),
            TokenType::StringQ => Ok(Expr::Variable("string?".to_string(), token)),
            TokenType::NumberQ => Ok(Expr::Variable("number?".to_string(), token)),
            TokenType::BooleanQ => Ok(Expr::Variable("boolean?".to_string(), token)),
            TokenType::CharQ => Ok(Expr::Variable("char?".to_string(), token)),
            TokenType::CharNumericQ => Ok(Expr::Variable("char-numeric?".to_string(), token)),
            TokenType::CharWhitespaceQ => Ok(Expr::Variable("char-whitespace?".to_string(), token)),
            TokenType::NullQ => Ok(Expr::Variable("null?".to_string(), token)),
            TokenType::PairQ => Ok(Expr::Variable("pair?".to_string(), token)),
            TokenType::EqQ => Ok(Expr::Variable("eq?".to_string(), token)),
            TokenType::CharEqQ => Ok(Expr::Variable("char=?".to_string(), token)),
            TokenType::StringEqQ => Ok(Expr::Variable("string=?".to_string(), token)),
            TokenType::EqualQ => Ok(Expr::Variable("equal?".to_string(), token)),

            // Type conversions
            TokenType::StringToNumber => Ok(Expr::Variable("string->number".to_string(), token)),
            TokenType::ListToString => Ok(Expr::Variable("list->string".to_string(), token)),
            TokenType::ListToVector => Ok(Expr::Variable("list->vector".to_string(), token)),
            TokenType::VectorToList => Ok(Expr::Variable("vector->list".to_string(), token)),

            // Vector operations
            TokenType::VectorQ => Ok(Expr::Variable("vector?".to_string(), token)),
            TokenType::VectorLength => Ok(Expr::Variable("vector-length".to_string(), token)),
            TokenType::VectorRef => Ok(Expr::Variable("vector-ref".to_string(), token)),
            TokenType::VectorSet => Ok(Expr::Variable("vector-set!".to_string(), token)),

            // Hashtable operations
            TokenType::MakeHashtable => Ok(Expr::Variable("make-hashtable".to_string(), token)),
            TokenType::HashtableSet => Ok(Expr::Variable("hashtable-set!".to_string(), token)),
            TokenType::HashtableRef => Ok(Expr::Variable("hashtable-ref".to_string(), token)),
            TokenType::HashtableDelete => {
                Ok(Expr::Variable("hashtable-delete!".to_string(), token))
            }
            TokenType::StringHash => Ok(Expr::Variable("string-hash".to_string(), token)),
            TokenType::EqualHash => Ok(Expr::Variable("equal-hash".to_string(), token)),

            _ => Err(ParseError::UnexpectedToken {
                expected: "literal value or identifier".to_string(),
                found: format!("{}", token_info.token_type),
                line: token_info.line,
                column: token_info.column,
                context: None,
            }),
        }
    }

    /// Parse parenthesized expressions (lists and function calls)
    fn parse_list(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        let left_paren = self.current_token().unwrap().clone();
        self.expect_token(TokenType::LeftParen)?;

        // Handle empty list
        if let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                self.advance();
                return Ok(Expr::List(vec![], token));
            }
        }

        // Check if this is a special form
        if let Some(token_info) = self.current_token() {
            let token = self.current_token_cloned();
            match &token_info.token_type {
                TokenType::Define => {
                    self.advance(); // consume 'define'
                    let mut define_expr = self.parse_define()?;
                    define_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Define(Box::new(define_expr)));
                }
                TokenType::Lambda => {
                    self.advance(); // consume 'lambda'
                    let mut lambda_expr = self.parse_lambda()?;
                    lambda_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Lambda(Box::new(lambda_expr)));
                }
                TokenType::If => {
                    self.advance(); // consume 'if'
                    let mut if_expr = self.parse_if()?;
                    if_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::If(Box::new(if_expr)));
                }
                TokenType::Cond => {
                    self.advance(); // consume 'cond'
                    let mut cond_expr = self.parse_cond()?;
                    cond_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Cond(Box::new(cond_expr)));
                }
                TokenType::Let => {
                    self.advance(); // consume 'let'
                    // Check if this is a named let (let loop)
                    if let Some(token_info) = self.current_token() {
                        if let TokenType::Identifier(_) = &token_info.token_type {
                            let mut let_loop_expr = self.parse_let_loop()?;
                            let_loop_expr.token = token;
                            self.expect_token(TokenType::RightParen)?;
                            return Ok(Expr::LetLoop(Box::new(let_loop_expr)));
                        }
                    }
                    let mut let_expr = self.parse_let()?;
                    let_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Let(Box::new(let_expr)));
                }
                TokenType::LetStar => {
                    self.advance(); // consume 'let*'
                    let mut let_star_expr = self.parse_let_star()?;
                    let_star_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::LetStar(Box::new(let_star_expr)));
                }
                TokenType::LetValues => {
                    self.advance(); // consume 'let-values'
                    let mut let_values_expr = self.parse_let_values()?;
                    let_values_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::LetValues(Box::new(let_values_expr)));
                }
                // Handle call-with-values special form
                TokenType::CallWithValues => {
                    self.advance(); // consume 'call-with-values'
                    let mut call_with_values_expr = self.parse_call_with_values()?;
                    call_with_values_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::CallWithValues(Box::new(call_with_values_expr)));
                }
                // Handle import special form
                TokenType::Import => {
                    self.advance(); // consume 'import'
                    let mut import_expr = self.parse_import()?;
                    import_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Import(Box::new(import_expr)));
                }
                // Handle set! special form
                TokenType::SetBang => {
                    self.advance(); // consume 'set!'
                    let mut set_expr = self.parse_set()?;
                    set_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Set(Box::new(set_expr)));
                }
                // Handle begin special form
                TokenType::Begin => {
                    self.advance(); // consume 'begin'
                    let mut begin_expr = self.parse_begin()?;
                    begin_expr.token = token;
                    self.expect_token(TokenType::RightParen)?;
                    return Ok(Expr::Begin(Box::new(begin_expr)));
                }
                _ => {}
            }
        }

        // Parse the first expression
        let first_expr = self.parse_expr()?;

        // Parse remaining expressions
        let mut args = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            args.push(self.parse_expr()?);
        }

        // Expect closing parenthesis
        match self.current_token() {
            Some(token_info) if matches!(token_info.token_type, TokenType::RightParen) => {
                self.advance();

                // Treat as function call
                Ok(Expr::Call(Box::new(first_expr), args))
            }
            Some(token_info) => Err(ParseError::UnexpectedToken {
                expected: ")".to_string(),
                found: format!("{}", token_info.token_type),
                line: token_info.line,
                column: token_info.column,
                context: Some("function call or list".to_string()),
            }),
            None => Err(ParseError::UnmatchedParenthesis {
                line: left_paren.line,
                column: left_paren.column,
                paren_type: "parenthesis".to_string(),
            }),
        }
    }

    /// Parse quoted expressions: 'expr
    fn parse_quote(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        self.expect_token(TokenType::QuoteMark)?;
        let expr = self.parse_expr()?;
        Ok(Expr::Quote(Box::new(expr), token))
    }

    /// Parse quasiquoted expressions: `expr
    fn parse_quasiquote(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        self.expect_token(TokenType::BackQuote)?;
        let expr = self.parse_expr()?;
        Ok(Expr::QuasiQuote(Box::new(expr), token))
    }

    /// Parse unquoted expressions: ,expr
    fn parse_unquote(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        self.expect_token(TokenType::Comma)?;
        let expr = self.parse_expr()?;
        Ok(Expr::UnQuote(Box::new(expr), token))
    }

    /// Parse unquote-splicing expressions: ,@expr
    fn parse_unquote_splicing(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        self.expect_token(TokenType::CommaAt)?;
        let expr = self.parse_expr()?;
        Ok(Expr::UnQuoteSplicing(Box::new(expr), token))
    }

    /// Parse vector expressions: #(expr ...)
    fn parse_vector(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token_cloned();
        self.expect_token(TokenType::VectorPrefix)?;
        self.expect_token(TokenType::LeftParen)?;

        let mut elements = Vec::new();

        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            elements.push(self.parse_expr()?);
        }

        self.expect_token(TokenType::RightParen)?;
        Ok(Expr::Vector(elements, token))
    }

    /// Parse begin expressions: (begin expr1 expr2 ...)
    fn parse_begin(&mut self) -> Result<crate::ast::BeginExpr, ParseError> {
        let mut expressions = Vec::new();

        // Parse all expressions until closing parenthesis
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            expressions.push(self.parse_expr()?);
        }

        // Begin requires at least one expression
        if expressions.is_empty() {
            return Err(ParseError::InvalidSpecialForm {
                form: "begin".to_string(),
                reason: "begin requires at least one expression".to_string(),
                line: 0,
                column: 0,
                suggestion: Some("Use (begin expr1 expr2 ...)".to_string()),
            });
        }

        Ok(crate::ast::BeginExpr {
            body: expressions,
            token: None,
        })
    }

    // Utility methods
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn current_token_cloned(&self) -> Option<Token> {
        Some(self.tokens[self.position].clone())
    }

    /// Parse docstring and body expressions
    /// Returns (docstring, body) where docstring is Some(string) if found, None otherwise
    /// A string literal is only treated as a docstring if there are additional expressions after it
    fn parse_docstring_and_body(
        &mut self,
    ) -> Result<(Option<Token>, Option<String>, Vec<Expr>), ParseError> {
        // First, collect all expressions
        let mut all_expressions = Vec::new();
        let first_token = self.current_token_cloned();
        all_expressions.push(self.parse_expr()?);
        let second_token = self.current_token_cloned();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            all_expressions.push(self.parse_expr()?);
        }

        // If we have more than one expression and the first is a string literal,
        // treat the first as a docstring and the rest as the body
        let (token, docstring, body) = if all_expressions.len() > 1 {
            if let Some(Expr::String(s, _)) = all_expressions.first() {
                let docstring = Some(s.clone());
                let body = all_expressions.into_iter().skip(1).collect();
                (second_token, docstring, body)
            } else {
                (second_token, None, all_expressions)
            }
        } else {
            // If we have only one expression (or none), treat it as the body
            (first_token, None, all_expressions)
        };

        Ok((token, docstring, body))
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.position + 1)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.tokens.get(self.position - 1)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
            || matches!(
                self.current_token().map(|t| &t.token_type),
                Some(TokenType::Eof)
            )
    }

    fn expect_token(&mut self, expected: TokenType) -> Result<&Token, ParseError> {
        match self.current_token() {
            Some(token_info)
                if std::mem::discriminant(&token_info.token_type)
                    == std::mem::discriminant(&expected) =>
            {
                Ok(self.advance().unwrap())
            }
            Some(token_info) => Err(ParseError::UnexpectedToken {
                expected: format!("{}", expected),
                found: format!("{}", token_info.token_type),
                line: token_info.line,
                column: token_info.column,
                context: None,
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: format!("{}", expected),
                context: None,
            }),
        }
    }

    // Special form parsers (simplified versions)
    fn parse_define(&mut self) -> Result<crate::ast::DefineExpr, ParseError> {
        // Handle both variable and function definitions
        match self.current_token() {
            Some(token_info) => {
                match &token_info.token_type {
                    TokenType::Identifier(name) => {
                        // Variable definition: (define name value)
                        let name = name.clone();
                        self.advance();
                        let value = self.parse_expr()?;
                        let define_expr = crate::ast::DefineExpr {
                            name,
                            value,
                            docstring: None,
                            token: None,
                        };
                        Ok(define_expr)
                    }
                    TokenType::LeftParen => {
                        // Function definition: (define (name args...) body...)
                        self.advance(); // consume '('

                        // Parse function name
                        let name = match self.current_token() {
                            Some(token_info) => match &token_info.token_type {
                                TokenType::Identifier(name) => {
                                    let name = name.clone();
                                    self.advance();
                                    name
                                }
                                _ => {
                                    return Err(ParseError::InvalidSpecialForm {
                                        form: "define".to_string(),
                                        reason: "function name must be an identifier".to_string(),
                                        line: token_info.line,
                                        column: token_info.column,
                                        suggestion: Some(
                                            "Use (define (function-name args...) body...)"
                                                .to_string(),
                                        ),
                                    });
                                }
                            },
                            None => {
                                return Err(ParseError::UnexpectedEof {
                                    expected: "function name".to_string(),
                                    context: Some("define function expression".to_string()),
                                });
                            }
                        };

                        // Parse parameters
                        let mut params = Vec::new();
                        while let Some(token_info) = self.current_token() {
                            if matches!(token_info.token_type, TokenType::RightParen) {
                                break;
                            }
                            match &token_info.token_type {
                                TokenType::Identifier(param) => {
                                    params.push(param.clone());
                                    self.advance();
                                }
                                _ => {
                                    return Err(ParseError::InvalidSpecialForm {
                                    form: "define".to_string(),
                                    reason: "parameter must be an identifier".to_string(),
                                    line: token_info.line,
                                    column: token_info.column,
                                    suggestion: Some("Use (define (function-name param1 param2 ...) body...)".to_string()),
                                });
                                }
                            }
                        }

                        self.expect_token(TokenType::RightParen)?; // consume closing ')'

                        // Parse docstring and function body
                        let (token, docstring, body) = self.parse_docstring_and_body()?;

                        // Ensure we have at least one body expression
                        if body.is_empty() {
                            return Err(ParseError::InvalidSpecialForm {
                                form: "define".to_string(),
                                reason: "function body cannot be empty".to_string(),
                                line: 0,
                                column: 0,
                                suggestion: Some(
                                    "Add at least one expression as the function body".to_string(),
                                ),
                            });
                        }

                        // Create a lambda expression as the value
                        let lambda_expr = crate::ast::LambdaExpr {
                            params,
                            body,
                            docstring: docstring.clone(),
                            token: token,
                        };

                        Ok(crate::ast::DefineExpr {
                            name,
                            value: Expr::Lambda(Box::new(lambda_expr)),
                            docstring,
                            token: None,
                        })
                    }
                    _ => {
                        return Err(ParseError::InvalidSpecialForm {
                            form: "define".to_string(),
                            reason: "expected variable name or function definition".to_string(),
                            line: token_info.line,
                            column: token_info.column,
                            suggestion: Some(
                                "Use (define name value) or (define (name args...) body...)"
                                    .to_string(),
                            ),
                        });
                    }
                }
            }
            None => {
                return Err(ParseError::UnexpectedEof {
                    expected: "variable name or function definition".to_string(),
                    context: Some("define expression".to_string()),
                });
            }
        }
    }

    fn parse_lambda(&mut self) -> Result<crate::ast::LambdaExpr, ParseError> {
        self.expect_token(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            match &token_info.token_type {
                TokenType::Identifier(param) => {
                    params.push(param.clone());
                    self.advance();
                }
                _ => {
                    return Err(ParseError::invalid_special_form(
                        "lambda".to_string(),
                        "parameter must be an identifier".to_string(),
                        token_info.line,
                        token_info.column,
                        Some("Use (lambda (param1 param2 ...) body...)".to_string()),
                    ));
                }
            }
        }

        self.expect_token(TokenType::RightParen)?;

        // Parse docstring and lambda body
        let (token, docstring, body) = self.parse_docstring_and_body()?;

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "lambda".to_string(),
                "lambda body cannot be empty".to_string(),
                0,
                0,
                Some("Lambda must have at least one body expression".to_string()),
            ));
        }

        Ok(crate::ast::LambdaExpr {
            params,
            body,
            docstring,
            token,
        })
    }

    fn parse_if(&mut self) -> Result<crate::ast::IfExpr, ParseError> {
        let condition = self.parse_expr()?;
        let then_expr = self.parse_expr()?;
        let else_expr = if let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                Expr::Boolean(false, None)
            } else {
                self.parse_expr()?
            }
        } else {
            return Err(ParseError::unexpected_eof(
                "else expression or closing parenthesis".to_string(),
                Some("if expression".to_string()),
            ));
        };

        Ok(crate::ast::IfExpr {
            condition,
            then_expr,
            else_expr,
            token: None,
        })
    }

    fn parse_cond(&mut self) -> Result<crate::ast::CondExpr, ParseError> {
        let mut clauses = Vec::new();

        while let Some(token_info) = self.current_token() {
            let token = self.current_token_cloned();
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }

            if !matches!(token_info.token_type, TokenType::LeftParen) {
                return Err(ParseError::invalid_special_form(
                    "cond".to_string(),
                    "each clause must be a list".to_string(),
                    token_info.line,
                    token_info.column,
                    Some("Use (cond (test1 body1...) (test2 body2...) ...)".to_string()),
                ));
            }

            self.advance(); // consume '('

            let test = if let Some(token_info) = self.current_token() {
                match &token_info.token_type {
                    TokenType::Else => {
                        self.advance();
                        Expr::Boolean(true, None)
                    }
                    _ => self.parse_expr()?,
                }
            } else {
                return Err(ParseError::unexpected_eof(
                    "test expression or 'else'".to_string(),
                    Some("cond clause".to_string()),
                ));
            };

            let mut body = Vec::new();
            while let Some(token_info) = self.current_token() {
                if matches!(token_info.token_type, TokenType::RightParen) {
                    break;
                }
                body.push(self.parse_expr()?);
            }

            if body.is_empty() {
                return Err(ParseError::invalid_special_form(
                    "cond".to_string(),
                    "clause body cannot be empty".to_string(),
                    0,
                    0,
                    Some("Each clause must have at least one body expression".to_string()),
                ));
            }

            self.expect_token(TokenType::RightParen)?;
            clauses.push(crate::ast::CondClause {
                test,
                body,
                token: token,
            });
        }

        if clauses.is_empty() {
            return Err(ParseError::invalid_special_form(
                "cond".to_string(),
                "cond must have at least one clause".to_string(),
                0,
                0,
                Some("Use (cond (test1 body1...) (test2 body2...) ...)".to_string()),
            ));
        }

        Ok(crate::ast::CondExpr {
            clauses,
            token: None,
        })
    }

    fn parse_let(&mut self) -> Result<crate::ast::LetExpr, ParseError> {
        self.expect_token(TokenType::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }

            if !matches!(token_info.token_type, TokenType::LeftParen) {
                return Err(ParseError::invalid_special_form(
                    "let".to_string(),
                    "each binding must be a list".to_string(),
                    token_info.line,
                    token_info.column,
                    Some("Use (let ((var1 val1) (var2 val2) ...) body...)".to_string()),
                ));
            }

            self.advance(); // consume '('

            let var_name = match self.current_token() {
                Some(token_info) => match &token_info.token_type {
                    TokenType::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        return Err(ParseError::invalid_binding(
                            "variable name must be an identifier".to_string(),
                            token_info.line,
                            token_info.column,
                            Some("Variable names must be valid identifiers".to_string()),
                        ));
                    }
                },
                None => {
                    return Err(ParseError::unexpected_eof(
                        "variable name".to_string(),
                        Some("let binding".to_string()),
                    ));
                }
            };

            let value = self.parse_expr()?;
            self.expect_token(TokenType::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(TokenType::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "let".to_string(),
                "let body cannot be empty".to_string(),
                0,
                0,
                Some("Let must have at least one body expression".to_string()),
            ));
        }

        Ok(crate::ast::LetExpr {
            bindings,
            body,
            token: None,
        })
    }

    fn parse_let_star(&mut self) -> Result<crate::ast::LetStarExpr, ParseError> {
        // Similar to parse_let but for let*
        self.expect_token(TokenType::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }

            self.expect_token(TokenType::LeftParen)?;

            let var_name = match self.current_token() {
                Some(token_info) => match &token_info.token_type {
                    TokenType::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        return Err(ParseError::invalid_binding(
                            "variable name must be an identifier".to_string(),
                            token_info.line,
                            token_info.column,
                            None,
                        ));
                    }
                },
                None => {
                    return Err(ParseError::unexpected_eof(
                        "variable name".to_string(),
                        Some("let* binding".to_string()),
                    ));
                }
            };

            let value = self.parse_expr()?;
            self.expect_token(TokenType::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(TokenType::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "let*".to_string(),
                "let* body cannot be empty".to_string(),
                0,
                0,
                None,
            ));
        }

        Ok(crate::ast::LetStarExpr {
            bindings,
            body,
            token: None,
        })
    }

    fn parse_let_loop(&mut self) -> Result<crate::ast::LetLoopExpr, ParseError> {
        let name = match self.current_token() {
            Some(token_info) => match &token_info.token_type {
                TokenType::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(ParseError::invalid_special_form(
                        "let loop".to_string(),
                        "loop name must be an identifier".to_string(),
                        token_info.line,
                        token_info.column,
                        None,
                    ));
                }
            },
            None => {
                return Err(ParseError::unexpected_eof(
                    "loop name".to_string(),
                    Some("let loop".to_string()),
                ));
            }
        };

        self.expect_token(TokenType::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }

            self.expect_token(TokenType::LeftParen)?;

            let var_name = match self.current_token() {
                Some(token_info) => match &token_info.token_type {
                    TokenType::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        return Err(ParseError::invalid_binding(
                            "variable name must be an identifier".to_string(),
                            token_info.line,
                            token_info.column,
                            None,
                        ));
                    }
                },
                None => {
                    return Err(ParseError::unexpected_eof(
                        "variable name".to_string(),
                        Some("let loop binding".to_string()),
                    ));
                }
            };

            let value = self.parse_expr()?;
            self.expect_token(TokenType::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(TokenType::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "let loop".to_string(),
                "let loop body cannot be empty".to_string(),
                0,
                0,
                None,
            ));
        }

        Ok(crate::ast::LetLoopExpr {
            name,
            bindings,
            body,
            token: None,
        })
    }

    fn parse_let_values(&mut self) -> Result<crate::ast::LetValuesExpr, ParseError> {
        self.expect_token(TokenType::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }

            self.expect_token(TokenType::LeftParen)?;
            self.expect_token(TokenType::LeftParen)?;

            let mut vars = Vec::new();
            while let Some(token_info) = self.current_token() {
                if matches!(token_info.token_type, TokenType::RightParen) {
                    break;
                }
                match &token_info.token_type {
                    TokenType::Identifier(name) => {
                        vars.push(name.clone());
                        self.advance();
                    }
                    _ => {
                        return Err(ParseError::invalid_binding(
                            "variable name must be an identifier".to_string(),
                            token_info.line,
                            token_info.column,
                            None,
                        ));
                    }
                }
            }

            self.expect_token(TokenType::RightParen)?;
            let value = self.parse_expr()?;
            self.expect_token(TokenType::RightParen)?;
            bindings.push((vars, value));
        }

        self.expect_token(TokenType::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token_type, TokenType::RightParen) {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "let-values".to_string(),
                "let-values body cannot be empty".to_string(),
                0,
                0,
                None,
            ));
        }

        Ok(crate::ast::LetValuesExpr {
            bindings,
            body,
            token: None,
        })
    }

    fn parse_call_with_values(&mut self) -> Result<crate::ast::CallWithValuesExpr, ParseError> {
        let producer = self.parse_expr()?;
        let consumer = self.parse_expr()?;
        Ok(crate::ast::CallWithValuesExpr {
            producer,
            consumer,
            token: None,
        })
    }

    fn parse_import(&mut self) -> Result<crate::ast::ImportExpr, ParseError> {
        let module_spec = self.parse_expr()?;
        Ok(crate::ast::ImportExpr {
            module_spec,
            token: None,
        })
    }

    fn parse_set(&mut self) -> Result<crate::ast::SetExpr, ParseError> {
        let variable = match self.current_token() {
            Some(token_info) => match &token_info.token_type {
                TokenType::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(ParseError::invalid_special_form(
                        "set!".to_string(),
                        "variable name must be an identifier".to_string(),
                        token_info.line,
                        token_info.column,
                        Some("Use (set! variable-name new-value)".to_string()),
                    ));
                }
            },
            None => {
                return Err(ParseError::unexpected_eof(
                    "variable name".to_string(),
                    Some("set! expression".to_string()),
                ));
            }
        };

        let value = self.parse_expr()?;
        Ok(crate::ast::SetExpr {
            variable,
            value,
            token: None,
        })
    }
}
