use crate::Token;
use std::fmt;

/// Main expression type representing all possible Scheme expressions
#[derive(PartialEq, Clone)]
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

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n, _) => write!(f, "Number({})", n),
            Expr::String(s, _) => write!(f, "String({:?})", s),
            Expr::Character(c, _) => write!(f, "Character('{}')", c),
            Expr::Boolean(b, _) => write!(f, "Boolean({})", b),
            Expr::Variable(name, _) => write!(f, "Variable({:?})", name),
            Expr::Define(def) => write!(f, "Define({:?})", def),
            Expr::Lambda(lambda) => write!(f, "Lambda({:?})", lambda),
            Expr::If(if_expr) => write!(f, "If({:?})", if_expr),
            Expr::Cond(cond) => write!(f, "Cond({:?})", cond),
            Expr::Let(let_expr) => write!(f, "Let({:?})", let_expr),
            Expr::LetStar(let_star) => write!(f, "LetStar({:?})", let_star),
            Expr::LetLoop(let_loop) => write!(f, "LetLoop({:?})", let_loop),
            Expr::LetValues(let_values) => write!(f, "LetValues({:?})", let_values),
            Expr::CallWithValues(cwv) => write!(f, "CallWithValues({:?})", cwv),
            Expr::Import(import) => write!(f, "Import({:?})", import),
            Expr::Set(set) => write!(f, "Set({:?})", set),
            Expr::Call(func, args) => write!(f, "Call({:?}, {:?})", func, args),
            Expr::Quote(expr, _) => write!(f, "Quote({:?})", expr),
            Expr::QuasiQuote(expr, _) => write!(f, "QuasiQuote({:?})", expr),
            Expr::UnQuote(expr, _) => write!(f, "UnQuote({:?})", expr),
            Expr::UnQuoteSplicing(expr, _) => write!(f, "UnQuoteSplicing({:?})", expr),
            Expr::Begin(begin) => write!(f, "Begin({:?})", begin),
            Expr::List(exprs, _) => write!(f, "List({:?})", exprs),
            Expr::Vector(exprs, _) => write!(f, "Vector({:?})", exprs),
        }
    }
}

/// Define expression: (define name value) or (define (name args...) body...)
#[derive(PartialEq, Clone)]
pub struct DefineExpr {
    pub name: String,
    pub value: Expr,
    pub docstring: Option<String>,
    pub token: Option<Token>,
}

impl fmt::Debug for DefineExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DefineExpr")
            .field("name", &self.name)
            .field("value", &self.value)
            .field("docstring", &self.docstring)
            .finish()
    }
}

/// Lambda expression: (lambda (args...) body...)
#[derive(PartialEq, Clone)]
pub struct LambdaExpr {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
    pub docstring: Option<String>,
    pub token: Option<Token>,
}

impl fmt::Debug for LambdaExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LambdaExpr")
            .field("params", &self.params)
            .field("body", &self.body)
            .field("docstring", &self.docstring)
            .finish()
    }
}

/// If expression: (if condition then else)
#[derive(PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_expr: Expr,
    pub else_expr: Expr,
    pub token: Option<Token>,
}

impl fmt::Debug for IfExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IfExpr")
            .field("condition", &self.condition)
            .field("then_expr", &self.then_expr)
            .field("else_expr", &self.else_expr)
            .finish()
    }
}

/// Cond expression: (cond (test body...) ... (else body...))
#[derive(PartialEq, Clone)]
pub struct CondExpr {
    pub clauses: Vec<CondClause>,
    pub token: Option<Token>,
}

impl fmt::Debug for CondExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CondExpr")
            .field("clauses", &self.clauses)
            .finish()
    }
}

/// Individual clause in a cond expression
#[derive(PartialEq, Clone)]
pub struct CondClause {
    pub test: Expr,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for CondClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CondClause")
            .field("test", &self.test)
            .field("body", &self.body)
            .finish()
    }
}

/// Let expression: (let ((var val) ...) body...)
#[derive(PartialEq, Clone)]
pub struct LetExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for LetExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetExpr")
            .field("bindings", &self.bindings)
            .field("body", &self.body)
            .finish()
    }
}

/// Let* expression: (let* ((var val) ...) body...)
#[derive(PartialEq, Clone)]
pub struct LetStarExpr {
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for LetStarExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetStarExpr")
            .field("bindings", &self.bindings)
            .field("body", &self.body)
            .finish()
    }
}

/// Named let expression: (let loop ((var val) ...) body...)
#[derive(PartialEq, Clone)]
pub struct LetLoopExpr {
    pub name: String,
    pub bindings: Vec<(String, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for LetLoopExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetLoopExpr")
            .field("name", &self.name)
            .field("bindings", &self.bindings)
            .field("body", &self.body)
            .finish()
    }
}

/// Let-values expression: (let-values (((var ...) expr) ...) body...)
#[derive(PartialEq, Clone)]
pub struct LetValuesExpr {
    pub bindings: Vec<(Vec<String>, Expr)>,
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for LetValuesExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetValuesExpr")
            .field("bindings", &self.bindings)
            .field("body", &self.body)
            .finish()
    }
}

/// Call-with-values expression: (call-with-values producer consumer)
#[derive(PartialEq, Clone)]
pub struct CallWithValuesExpr {
    pub producer: Expr,
    pub consumer: Expr,
    pub token: Option<Token>,
}

impl fmt::Debug for CallWithValuesExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CallWithValuesExpr")
            .field("producer", &self.producer)
            .field("consumer", &self.consumer)
            .finish()
    }
}

/// Import expression: (import module-spec)
#[derive(PartialEq, Clone)]
pub struct ImportExpr {
    pub module_spec: Expr,
    pub token: Option<Token>,
}

impl fmt::Debug for ImportExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImportExpr")
            .field("module_spec", &self.module_spec)
            .finish()
    }
}

/// Begin expression: (begin expression expression  … expression )
#[derive(PartialEq, Clone)]
pub struct BeginExpr {
    pub body: Vec<Expr>,
    pub token: Option<Token>,
}

impl fmt::Debug for BeginExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BeginExpr")
            .field("body", &self.body)
            .finish()
    }
}

/// Set! expression: (set! var value)
#[derive(PartialEq, Clone)]
pub struct SetExpr {
    pub variable: String,
    pub value: Expr,
    pub token: Option<Token>,
}

impl fmt::Debug for SetExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SetExpr")
            .field("variable", &self.variable)
            .field("value", &self.value)
            .finish()
    }
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
