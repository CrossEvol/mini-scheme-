use crate::ast::Expr;
use crate::error::ParseError;
use crate::token::{Token, TokenInfo};

/// Parser for converting tokens into an Abstract Syntax Tree
pub struct Parser {
    tokens: Vec<TokenInfo>,
    position: usize,
}

impl Parser {
    /// Create a new parser with the given tokens
    pub fn new(tokens: Vec<TokenInfo>) -> Self {
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
                if matches!(token_info.token, Token::Eof) {
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
                match &token_info.token {
                    // Synchronize at top-level forms
                    Token::LeftParen => {
                        // Look ahead to see if this starts a top-level form
                        if let Some(next_token) = self.peek_token() {
                            match &next_token.token {
                                Token::Define
                                | Token::Lambda
                                | Token::If
                                | Token::Cond
                                | Token::Let
                                | Token::LetStar
                                | Token::LetValues
                                | Token::Begin
                                | Token::QuoteKeyword
                                | Token::QuasiQuote
                                | Token::CallWithValues
                                | Token::Import
                                | Token::SetBang => {
                                    return true;
                                }
                                _ => {}
                            }
                        }
                        return true; // Any left paren could start a new expression
                    }
                    Token::Eof => return false,
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
            Some(token_info) => match &token_info.token {
                Token::LeftParen => self.parse_list(),
                Token::QuoteMark => self.parse_quote(),
                Token::BackQuote => self.parse_quasiquote(),
                Token::Comma => self.parse_unquote(),
                Token::CommaAt => self.parse_unquote_splicing(),
                Token::VectorPrefix => self.parse_vector(),
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

        match &token_info.token {
            Token::Number(n) => Ok(Expr::Number(*n)),
            Token::String(s) => Ok(Expr::String(s.clone())),
            Token::Character(c) => Ok(Expr::Character(*c)),
            Token::Boolean(b) => Ok(Expr::Boolean(*b)),
            Token::Identifier(name) => Ok(Expr::Variable(name.clone())),
            Token::Null => Ok(Expr::List(vec![])), // null is represented as empty list

            // Handle built-in procedures and keywords as variables for now
            Token::Plus => Ok(Expr::Variable("+".to_string())),
            Token::Minus => Ok(Expr::Variable("-".to_string())),
            Token::Multiply => Ok(Expr::Variable("*".to_string())),
            Token::Divide => Ok(Expr::Variable("/".to_string())),
            Token::Equal => Ok(Expr::Variable("=".to_string())),
            Token::LessThan => Ok(Expr::Variable("<".to_string())),
            Token::LessThanEqual => Ok(Expr::Variable("<=".to_string())),
            Token::GreaterThan => Ok(Expr::Variable(">".to_string())),
            Token::GreaterThanEqual => Ok(Expr::Variable(">=".to_string())),

            // Built-in procedures
            Token::Car => Ok(Expr::Variable("car".to_string())),
            Token::Cdr => Ok(Expr::Variable("cdr".to_string())),
            Token::Cons => Ok(Expr::Variable("cons".to_string())),
            Token::List => Ok(Expr::Variable("list".to_string())),
            Token::Vector => Ok(Expr::Variable("vector".to_string())),
            Token::Display => Ok(Expr::Variable("display".to_string())),
            Token::Error => Ok(Expr::Variable("error".to_string())),
            Token::Values => Ok(Expr::Variable("values".to_string())),
            Token::ForEach => Ok(Expr::Variable("for-each".to_string())),

            // Predicates
            Token::HashtableQ => Ok(Expr::Variable("hashtable?".to_string())),
            Token::StringQ => Ok(Expr::Variable("string?".to_string())),
            Token::NumberQ => Ok(Expr::Variable("number?".to_string())),
            Token::BooleanQ => Ok(Expr::Variable("boolean?".to_string())),
            Token::CharQ => Ok(Expr::Variable("char?".to_string())),
            Token::CharNumericQ => Ok(Expr::Variable("char-numeric?".to_string())),
            Token::CharWhitespaceQ => Ok(Expr::Variable("char-whitespace?".to_string())),
            Token::NullQ => Ok(Expr::Variable("null?".to_string())),
            Token::PairQ => Ok(Expr::Variable("pair?".to_string())),
            Token::EqQ => Ok(Expr::Variable("eq?".to_string())),
            Token::CharEqQ => Ok(Expr::Variable("char=?".to_string())),
            Token::StringEqQ => Ok(Expr::Variable("string=?".to_string())),
            Token::EqualQ => Ok(Expr::Variable("equal?".to_string())),

            // Type conversions
            Token::StringToNumber => Ok(Expr::Variable("string->number".to_string())),
            Token::ListToString => Ok(Expr::Variable("list->string".to_string())),
            Token::ListToVector => Ok(Expr::Variable("list->vector".to_string())),
            Token::VectorToList => Ok(Expr::Variable("vector->list".to_string())),

            // Hashtable operations
            Token::MakeHashtable => Ok(Expr::Variable("make-hashtable".to_string())),
            Token::HashtableSet => Ok(Expr::Variable("hashtable-set!".to_string())),
            Token::HashtableRef => Ok(Expr::Variable("hashtable-ref".to_string())),
            Token::HashtableDelete => Ok(Expr::Variable("hashtable-delete!".to_string())),
            Token::StringHash => Ok(Expr::Variable("string-hash".to_string())),
            Token::EqualHash => Ok(Expr::Variable("equal-hash".to_string())),

            _ => Err(ParseError::UnexpectedToken {
                expected: "literal value or identifier".to_string(),
                found: format!("{}", token_info.token),
                line: token_info.line,
                column: token_info.column,
                context: None,
            }),
        }
    }

    /// Parse parenthesized expressions (lists and function calls)
    fn parse_list(&mut self) -> Result<Expr, ParseError> {
        let left_paren = self.current_token().unwrap().clone();
        self.expect_token(Token::LeftParen)?;

        // Handle empty list
        if let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                self.advance();
                return Ok(Expr::List(vec![]));
            }
        }

        // Check if this is a special form
        if let Some(token_info) = self.current_token() {
            match &token_info.token {
                Token::Define => {
                    self.advance(); // consume 'define'
                    let define_expr = self.parse_define()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Define(Box::new(define_expr)));
                }
                Token::Lambda => {
                    self.advance(); // consume 'lambda'
                    let lambda_expr = self.parse_lambda()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Lambda(Box::new(lambda_expr)));
                }
                Token::If => {
                    self.advance(); // consume 'if'
                    let if_expr = self.parse_if()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::If(Box::new(if_expr)));
                }
                Token::Cond => {
                    self.advance(); // consume 'cond'
                    let cond_expr = self.parse_cond()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Cond(Box::new(cond_expr)));
                }
                Token::Let => {
                    self.advance(); // consume 'let'
                    // Check if this is a named let (let loop)
                    if let Some(token_info) = self.current_token() {
                        if let Token::Identifier(_) = &token_info.token {
                            let let_loop_expr = self.parse_let_loop()?;
                            self.expect_token(Token::RightParen)?;
                            return Ok(Expr::LetLoop(Box::new(let_loop_expr)));
                        }
                    }
                    let let_expr = self.parse_let()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Let(Box::new(let_expr)));
                }
                Token::LetStar => {
                    self.advance(); // consume 'let*'
                    let let_star_expr = self.parse_let_star()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::LetStar(Box::new(let_star_expr)));
                }
                Token::LetValues => {
                    self.advance(); // consume 'let-values'
                    let let_values_expr = self.parse_let_values()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::LetValues(Box::new(let_values_expr)));
                }
                // Handle call-with-values special form
                Token::CallWithValues => {
                    self.advance(); // consume 'call-with-values'
                    let call_with_values_expr = self.parse_call_with_values()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::CallWithValues(Box::new(call_with_values_expr)));
                }
                // Handle import special form
                Token::Import => {
                    self.advance(); // consume 'import'
                    let import_expr = self.parse_import()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Import(Box::new(import_expr)));
                }
                // Handle set! special form
                Token::SetBang => {
                    self.advance(); // consume 'set!'
                    let set_expr = self.parse_set()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(Expr::Set(Box::new(set_expr)));
                }
                // Handle begin special form
                Token::Begin => {
                    self.advance(); // consume 'begin'
                    let begin_expr = self.parse_begin()?;
                    self.expect_token(Token::RightParen)?;
                    return Ok(begin_expr);
                }
                _ => {}
            }
        }

        // Parse the first expression
        let first_expr = self.parse_expr()?;

        // Parse remaining expressions
        let mut args = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }
            args.push(self.parse_expr()?);
        }

        // Expect closing parenthesis
        match self.current_token() {
            Some(token_info) if matches!(token_info.token, Token::RightParen) => {
                self.advance();

                // Treat as function call
                Ok(Expr::Call(Box::new(first_expr), args))
            }
            Some(token_info) => Err(ParseError::UnexpectedToken {
                expected: ")".to_string(),
                found: format!("{}", token_info.token),
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
        self.expect_token(Token::QuoteMark)?;
        let expr = self.parse_expr()?;
        Ok(Expr::Quote(Box::new(expr)))
    }

    /// Parse quasiquoted expressions: `expr
    fn parse_quasiquote(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::BackQuote)?;
        let expr = self.parse_expr()?;
        Ok(Expr::QuasiQuote(Box::new(expr)))
    }

    /// Parse unquoted expressions: ,expr
    fn parse_unquote(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::Comma)?;
        let expr = self.parse_expr()?;
        Ok(Expr::UnQuote(Box::new(expr)))
    }

    /// Parse unquote-splicing expressions: ,@expr
    fn parse_unquote_splicing(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::CommaAt)?;
        let expr = self.parse_expr()?;
        Ok(Expr::UnQuoteSplicing(Box::new(expr)))
    }

    /// Parse vector expressions: #(expr ...)
    fn parse_vector(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::VectorPrefix)?;
        self.expect_token(Token::LeftParen)?;

        let mut elements = Vec::new();

        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }
            elements.push(self.parse_expr()?);
        }

        self.expect_token(Token::RightParen)?;
        Ok(Expr::Vector(elements))
    }

    /// Parse begin expressions: (begin expr1 expr2 ...)
    fn parse_begin(&mut self) -> Result<Expr, ParseError> {
        let mut expressions = Vec::new();

        // Parse all expressions until closing parenthesis
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
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

        Ok(Expr::Begin(expressions))
    }

    // Utility methods
    fn current_token(&self) -> Option<&TokenInfo> {
        self.tokens.get(self.position)
    }

    fn peek_token(&self) -> Option<&TokenInfo> {
        self.tokens.get(self.position + 1)
    }

    fn advance(&mut self) -> Option<&TokenInfo> {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.tokens.get(self.position - 1)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
            || matches!(self.current_token().map(|t| &t.token), Some(Token::Eof))
    }

    fn expect_token(&mut self, expected: Token) -> Result<&TokenInfo, ParseError> {
        match self.current_token() {
            Some(token_info)
                if std::mem::discriminant(&token_info.token)
                    == std::mem::discriminant(&expected) =>
            {
                Ok(self.advance().unwrap())
            }
            Some(token_info) => Err(ParseError::UnexpectedToken {
                expected: format!("{}", expected),
                found: format!("{}", token_info.token),
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
        // Simplified define parser - just handle variable definitions for now
        let name = match self.current_token() {
            Some(token_info) => match &token_info.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(ParseError::InvalidSpecialForm {
                        form: "define".to_string(),
                        reason: "variable name must be an identifier".to_string(),
                        line: token_info.line,
                        column: token_info.column,
                        suggestion: Some("Use (define variable-name value)".to_string()),
                    });
                }
            },
            None => {
                return Err(ParseError::UnexpectedEof {
                    expected: "variable name".to_string(),
                    context: Some("define expression".to_string()),
                });
            }
        };

        let value = self.parse_expr()?;
        Ok(crate::ast::DefineExpr { name, value })
    }

    fn parse_lambda(&mut self) -> Result<crate::ast::LambdaExpr, ParseError> {
        self.expect_token(Token::LeftParen)?;

        let mut params = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }
            match &token_info.token {
                Token::Identifier(param) => {
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

        self.expect_token(Token::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::invalid_special_form(
                "lambda".to_string(),
                "lambda body cannot be empty".to_string(),
                0,
                0,
                Some("Lambda must have at least one body expression".to_string()),
            ));
        }

        Ok(crate::ast::LambdaExpr { params, body })
    }

    fn parse_if(&mut self) -> Result<crate::ast::IfExpr, ParseError> {
        let condition = self.parse_expr()?;
        let then_expr = self.parse_expr()?;
        let else_expr = if let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                Expr::Boolean(false)
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
        })
    }

    fn parse_cond(&mut self) -> Result<crate::ast::CondExpr, ParseError> {
        let mut clauses = Vec::new();

        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }

            if !matches!(token_info.token, Token::LeftParen) {
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
                match &token_info.token {
                    Token::Else => {
                        self.advance();
                        Expr::Boolean(true)
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
                if matches!(token_info.token, Token::RightParen) {
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

            self.expect_token(Token::RightParen)?;
            clauses.push(crate::ast::CondClause { test, body });
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

        Ok(crate::ast::CondExpr { clauses })
    }

    fn parse_let(&mut self) -> Result<crate::ast::LetExpr, ParseError> {
        self.expect_token(Token::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }

            if !matches!(token_info.token, Token::LeftParen) {
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
                Some(token_info) => match &token_info.token {
                    Token::Identifier(name) => {
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
            self.expect_token(Token::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(Token::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
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

        Ok(crate::ast::LetExpr { bindings, body })
    }

    fn parse_let_star(&mut self) -> Result<crate::ast::LetStarExpr, ParseError> {
        // Similar to parse_let but for let*
        self.expect_token(Token::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }

            self.expect_token(Token::LeftParen)?;

            let var_name = match self.current_token() {
                Some(token_info) => match &token_info.token {
                    Token::Identifier(name) => {
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
            self.expect_token(Token::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(Token::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
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

        Ok(crate::ast::LetStarExpr { bindings, body })
    }

    fn parse_let_loop(&mut self) -> Result<crate::ast::LetLoopExpr, ParseError> {
        let name = match self.current_token() {
            Some(token_info) => match &token_info.token {
                Token::Identifier(name) => {
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

        self.expect_token(Token::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }

            self.expect_token(Token::LeftParen)?;

            let var_name = match self.current_token() {
                Some(token_info) => match &token_info.token {
                    Token::Identifier(name) => {
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
            self.expect_token(Token::RightParen)?;
            bindings.push((var_name, value));
        }

        self.expect_token(Token::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
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
        })
    }

    fn parse_let_values(&mut self) -> Result<crate::ast::LetValuesExpr, ParseError> {
        self.expect_token(Token::LeftParen)?;

        let mut bindings = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
                break;
            }

            self.expect_token(Token::LeftParen)?;
            self.expect_token(Token::LeftParen)?;

            let mut vars = Vec::new();
            while let Some(token_info) = self.current_token() {
                if matches!(token_info.token, Token::RightParen) {
                    break;
                }
                match &token_info.token {
                    Token::Identifier(name) => {
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

            self.expect_token(Token::RightParen)?;
            let value = self.parse_expr()?;
            self.expect_token(Token::RightParen)?;
            bindings.push((vars, value));
        }

        self.expect_token(Token::RightParen)?;

        let mut body = Vec::new();
        while let Some(token_info) = self.current_token() {
            if matches!(token_info.token, Token::RightParen) {
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

        Ok(crate::ast::LetValuesExpr { bindings, body })
    }

    fn parse_call_with_values(&mut self) -> Result<crate::ast::CallWithValuesExpr, ParseError> {
        let producer = self.parse_expr()?;
        let consumer = self.parse_expr()?;
        Ok(crate::ast::CallWithValuesExpr { producer, consumer })
    }

    fn parse_import(&mut self) -> Result<crate::ast::ImportExpr, ParseError> {
        let module_spec = self.parse_expr()?;
        Ok(crate::ast::ImportExpr { module_spec })
    }

    fn parse_set(&mut self) -> Result<crate::ast::SetExpr, ParseError> {
        let variable = match self.current_token() {
            Some(token_info) => match &token_info.token {
                Token::Identifier(name) => {
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
        Ok(crate::ast::SetExpr { variable, value })
    }
}
