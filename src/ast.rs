use crate::Token;

/// Main expression type representing all possible Scheme expressions
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Literals
    Number(f64, Option<Token>),
    String(String, Option<Token>),
    Character(char, Option<Token>),
    Boolean(bool, Option<Token>),
    Variable(String, Option<Token>),

    // Special Forms
    Define(Box<DefineExpr>),
    Lambda(Box<LambdaExpr>),
    If(Box<IfExpr>),
    Cond(Box<CondExpr>),
    Let(Box<LetExpr>),
    LetStar(Box<LetStarExpr>),
    LetLoop(Box<LetLoopExpr>),
    LetValues(Box<LetValuesExpr>),
    CallWithValues(Box<CallWithValuesExpr>),
    Import(Box<ImportExpr>),
    Set(Box<SetExpr>),

    // Function Application
    Call(Box<Expr>, Vec<Expr>),

    // Quotation
    Quote(Box<Expr>, Option<Token>),
    QuasiQuote(Box<Expr>, Option<Token>),
    UnQuote(Box<Expr>, Option<Token>),
    UnQuoteSplicing(Box<Expr>, Option<Token>),

    // Sequencing
    Begin(Box<BeginExpr>),

    // Data Structures
    List(Vec<Expr>, Option<Token>),
    Vector(Vec<Expr>, Option<Token>),
}

/// Define expression: (define name value) or (define (name args...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
    pub docstring: Option<String>,
    pub token: Option<Token>,
}

/// Lambda expression: (lambda (args...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
    pub docstring: Option<String>,
    pub token: Option<Token>,
}

/// If expression: (if condition then else)
#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_expr: Expr,
    pub else_expr: Expr,
    pub token: Option<Token>,
}

/// Cond expression: (cond (test body...) ... (else body...))
#[derive(Debug, PartialEq, Clone)]
pub struct CondExpr {
    pub clauses: Vec<CondClause>,
    pub token: Option<Token>,
}

/// Individual clause in a cond expression
#[derive(Debug, PartialEq, Clone)]
pub struct CondClause {
    pub test: Expr,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Let expression: (let ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Let* expression: (let* ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetStarExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Named let expression: (let loop ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetLoopExpr {
    pub name: String,
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Let-values expression: (let-values (((var ...) expr) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetValuesExpr {
    pub bindings: Vec<(Vec<String>, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Call-with-values expression: (call-with-values producer consumer)
#[derive(Debug, PartialEq, Clone)]
pub struct CallWithValuesExpr {
    pub producer: Expr,
    pub consumer: Expr,
    pub token: Option<Token>,
}

/// Import expression: (import module-spec)
#[derive(Debug, PartialEq, Clone)]
pub struct ImportExpr {
    pub module_spec: Expr,
    pub token: Option<Token>,
}

/// Begin expression: (begin expression expression  … expression )
#[derive(Debug, PartialEq, Clone)]
pub struct BeginExpr {
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

/// Set! expression: (set! var value)
#[derive(Debug, PartialEq, Clone)]
pub struct SetExpr {
    pub variable: String,
    pub value: Expr,
    pub token: Option<Token>,
}

impl DefineExpr {
    /// Create a new DefineExpr without a docstring
    pub fn new(name: String, value: Expr, token: Token) -> Self {
        Self {
            name,
            value,
            docstring: None,
            token: Some(token),
        }
    }

    /// Create a new DefineExpr with a docstring
    pub fn new_with_docstring(name: String, value: Expr, docstring: String, token: Token) -> Self {
        Self {
            name,
            value,
            docstring: Some(docstring),
            token: Some(token),
        }
    }
}

impl LambdaExpr {
    /// Create a new LambdaExpr without a docstring
    pub fn new(params: Vec<String>, body: Vec<Expr>, token: Token) -> Self {
        Self {
            params,
            body,
            docstring: None,
            token: Some(token),
        }
    }

    /// Create a new LambdaExpr with a docstring
    pub fn new_with_docstring(
        params: Vec<String>,
        body: Vec<Expr>,
        docstring: String,
        token: Token,
    ) -> Self {
        Self {
            params,
            body,
            docstring: Some(docstring),
            token: Some(token),
        }
    }
}
