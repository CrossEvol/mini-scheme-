/// Main expression type representing all possible Scheme expressions
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Literals
    Number(f64),
    String(String),
    Character(char),
    Boolean(bool),
    Variable(String),
    
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
    Quote(Box<Expr>),
    QuasiQuote(Box<Expr>),
    UnQuote(Box<Expr>),
    UnQuoteSplicing(Box<Expr>),
    
    // Sequencing
    Begin(Vec<Expr>),
    
    // Data Structures
    List(Vec<Expr>),
    Vector(Vec<Expr>),
}

/// Define expression: (define name value) or (define (name args...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
}

/// Lambda expression: (lambda (args...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
}

/// If expression: (if condition then else)
#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_expr: Expr,
    pub else_expr: Expr,
}

/// Cond expression: (cond (test body...) ... (else body...))
#[derive(Debug, PartialEq, Clone)]
pub struct CondExpr {
    pub clauses: Vec<CondClause>,
}

/// Individual clause in a cond expression
#[derive(Debug, PartialEq, Clone)]
pub struct CondClause {
    pub test: Expr,
    pub body: Vec<Expr>,
}

/// Let expression: (let ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
}

/// Let* expression: (let* ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetStarExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
}

/// Named let expression: (let loop ((var val) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetLoopExpr {
    pub name: String,
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
}

/// Let-values expression: (let-values (((var ...) expr) ...) body...)
#[derive(Debug, PartialEq, Clone)]
pub struct LetValuesExpr {
    pub bindings: Vec<(Vec<String>, Expr)>,
    pub body: Vec<Expr>,
}

/// Call-with-values expression: (call-with-values producer consumer)
#[derive(Debug, PartialEq, Clone)]
pub struct CallWithValuesExpr {
    pub producer: Expr,
    pub consumer: Expr,
}

/// Import expression: (import module-spec)
#[derive(Debug, PartialEq, Clone)]
pub struct ImportExpr {
    pub module_spec: Expr,
}

/// Set! expression: (set! var value)
#[derive(Debug, PartialEq, Clone)]
pub struct SetExpr {
    pub variable: String,
    pub value: Expr,
}