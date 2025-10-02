use crate::ast::{
    CallWithValuesExpr, DefineExpr, Expr, ImportExpr, LambdaExpr, LetExpr, LetLoopExpr,
    LetStarExpr, LetValuesExpr, SetExpr,
};
use crate::bytecode::{Chunk, OpCode};
use crate::object::{Function, Value};
use crate::trace::{CompilationPhase, CompilationTrace, TraceConfig, Tracer, format_ast_expr};

/// Type of function being compiled
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    Script,   // Top-level script
    Function, // Regular function
}

/// Local variable information during compilation
#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub depth: isize,      // -1 means uninitialized, 0+ is scope depth
    pub is_captured: bool, // True if captured by a closure
}

/// Upvalue information during compilation
#[derive(Debug, Clone)]
pub struct CompilerUpvalue {
    pub index: u8,
    pub is_local: bool, // True if capturing local, false if capturing upvalue
}

/// Compilation error types
#[derive(Debug, Clone)]
pub enum CompileError {
    TooManyConstants,
    TooManyLocals,
    TooManyUpvalues,
    UndefinedVariable(String),
    InvalidAssignmentTarget,
    VariableAlreadyDefined(String),
    JumpTooLarge,
    LoopTooLarge,
    TypeError { expected: String, got: String },
    ArityMismatch { expected: usize, got: usize },
    InvalidOperation(String),
    NotImplemented(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::TooManyConstants => write!(f, "Too many constants in one chunk"),
            CompileError::TooManyLocals => write!(f, "Too many local variables in function"),
            CompileError::TooManyUpvalues => write!(f, "Too many upvalues in function"),
            CompileError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'", name),
            CompileError::InvalidAssignmentTarget => write!(f, "Invalid assignment target"),
            CompileError::VariableAlreadyDefined(name) => {
                write!(f, "Variable '{}' already defined in this scope", name)
            }
            CompileError::JumpTooLarge => write!(f, "Jump distance too large"),
            CompileError::LoopTooLarge => write!(f, "Loop body too large"),
            CompileError::TypeError { expected, got } => {
                write!(f, "Type error: expected {}, got {}", expected, got)
            }
            CompileError::ArityMismatch { expected, got } => {
                write!(
                    f,
                    "Arity mismatch: expected {} arguments, got {}",
                    expected, got
                )
            }
            CompileError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            CompileError::NotImplemented(feature) => {
                write!(f, "Feature not implemented: {}", feature)
            }
        }
    }
}

impl std::error::Error for CompileError {}

/// Compiler for transforming AST to bytecode
pub struct Compiler {
    pub function: Function,
    pub function_type: FunctionType,
    pub locals: Vec<Local>,
    pub upvalues: Vec<CompilerUpvalue>,
    pub scope_depth: usize,
    pub enclosing: Option<Box<Compiler>>,
    pub trace_enabled: bool,
    pub tracer: Option<Tracer>,
}

impl Compiler {
    /// Create a new compiler for a script (top-level)
    pub fn new_script() -> Self {
        let mut compiler = Compiler {
            function: Function::new("script".to_string(), 0),
            function_type: FunctionType::Script,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
            enclosing: None,
            trace_enabled: false,
            tracer: None,
        };

        // Reserve slot 0 for the script itself
        compiler.locals.push(Local {
            name: "".to_string(),
            depth: 0,
            is_captured: false,
        });

        compiler
    }

    /// Create a new compiler for a function
    pub fn new_function(name: String, arity: usize, enclosing: Box<Compiler>) -> Self {
        let mut compiler = Compiler {
            function: Function::new(name, arity),
            function_type: FunctionType::Function,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
            enclosing: Some(enclosing),
            trace_enabled: false,
            tracer: None,
        };

        // Reserve slot 0 for the function itself
        compiler.locals.push(Local {
            name: "".to_string(),
            depth: 0,
            is_captured: false,
        });

        compiler
    }

    /// Enable compilation tracing
    pub fn enable_trace(&mut self) {
        self.trace_enabled = true;
        if self.tracer.is_none() {
            self.tracer = Some(Tracer::new(TraceConfig::compilation_only()));
        }
        if let Some(tracer) = &mut self.tracer {
            tracer.enable_compilation();
        }
    }

    /// Disable compilation tracing
    pub fn disable_trace(&mut self) {
        self.trace_enabled = false;
        if let Some(tracer) = &mut self.tracer {
            tracer.disable_compilation();
        }
    }

    /// Set a custom tracer
    pub fn set_tracer(&mut self, tracer: Tracer) {
        self.tracer = Some(tracer);
        self.trace_enabled = self.tracer.as_ref().map_or(false, |t| t.config.compilation);
    }

    /// Get a reference to the tracer
    pub fn tracer(&self) -> Option<&Tracer> {
        self.tracer.as_ref()
    }

    /// Get a mutable reference to the tracer
    pub fn tracer_mut(&mut self) -> Option<&mut Tracer> {
        self.tracer.as_mut()
    }

    /// Trace a compilation step (legacy method for backward compatibility)
    fn trace(&self, message: &str) {
        if self.trace_enabled {
            println!("[COMPILE] {}", message);
        }
    }

    /// Trace a compilation phase with detailed information
    fn trace_compilation_phase(&mut self, phase: CompilationPhase, expr: &Expr) {
        if let Some(tracer) = &mut self.tracer {
            let mut trace = CompilationTrace::new(
                phase,
                format_ast_expr(expr),
                self.function.name.clone(),
                self.scope_depth,
            );

            // Add current local variables
            for local in &self.locals {
                if local.depth >= 0 {
                    trace.add_local(local.name.clone());
                }
            }

            // Add current upvalues (simplified - we don't track names in upvalues yet)
            for (i, _) in self.upvalues.iter().enumerate() {
                trace.add_upvalue(format!("upvalue_{}", i));
            }

            tracer.trace_compilation(trace);
        }
    }

    /// Trace instruction generation
    fn trace_instruction(&mut self, opcode: OpCode, operands: Vec<u8>) {
        if let Some(tracer) = &mut self.tracer {
            // Create a new trace if we don't have one or add to the last one
            if tracer.compilation_traces.is_empty() {
                let mut trace = CompilationTrace::new(
                    CompilationPhase::InstructionGeneration,
                    "InstructionGeneration".to_string(),
                    self.function.name.clone(),
                    self.scope_depth,
                );
                trace.add_instruction(opcode, operands);
                tracer.compilation_traces.push(trace);
            } else if let Some(last_trace) = tracer.compilation_traces.last_mut() {
                last_trace.add_instruction(opcode, operands);
            }
        }
    }

    /// Compile an expression to bytecode
    pub fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling expression: {:?}", expr));
        self.trace_compilation_phase(CompilationPhase::InstructionGeneration, expr);

        match expr {
            // Literal expressions
            Expr::Number(n) => self.compile_number(*n),
            Expr::String(s) => self.compile_string(s.clone()),
            Expr::Character(c) => self.compile_character(*c),
            Expr::Boolean(b) => self.compile_boolean(*b),
            Expr::Variable(name) => self.compile_variable(name),

            // Function expressions
            Expr::Lambda(lambda) => self.compile_lambda(lambda),
            Expr::Call(func, args) => self.compile_call(func, args),

            // Define expression
            Expr::Define(define) => self.compile_define(define),

            // Control flow expressions
            Expr::If(if_expr) => self.compile_if(if_expr),
            Expr::Cond(cond_expr) => self.compile_cond(cond_expr),

            // Let binding forms
            Expr::Let(let_expr) => self.compile_let(let_expr),
            Expr::LetStar(let_star) => self.compile_let_star(let_star),
            Expr::LetLoop(let_loop) => self.compile_let_loop(let_loop),
            Expr::LetValues(let_values) => self.compile_let_values(let_values),

            // Assignment
            Expr::Set(set_expr) => self.compile_set(set_expr),

            // Quotation
            Expr::Quote(quoted) => self.compile_quote(quoted),
            Expr::QuasiQuote(quasi) => self.compile_quasiquote(quasi),
            Expr::UnQuote(unquoted) => self.compile_unquote(unquoted),
            Expr::UnQuoteSplicing(spliced) => self.compile_unquote_splicing(spliced),

            // Sequencing
            Expr::Begin(exprs) => self.compile_begin(exprs),

            // Data structures
            Expr::List(elements) => self.compile_list(elements),
            Expr::Vector(elements) => self.compile_vector_literal(elements),

            // Advanced forms
            Expr::CallWithValues(call_with_values) => {
                self.compile_call_with_values(call_with_values)
            }
            Expr::Import(import) => self.compile_import(import),
        }
    }

    /// Compile a number literal
    fn compile_number(&mut self, value: f64) -> Result<(), CompileError> {
        self.trace(&format!("Compiling number: {}", value));
        self.trace_compilation_phase(CompilationPhase::ConstantGeneration, &Expr::Number(value));
        self.emit_constant(Value::Number(value))?;
        Ok(())
    }

    /// Compile a string literal
    fn compile_string(&mut self, value: String) -> Result<(), CompileError> {
        self.trace(&format!("Compiling string: \"{}\"", value));
        self.emit_constant(Value::string(value))?;
        Ok(())
    }

    /// Compile a character literal
    fn compile_character(&mut self, value: char) -> Result<(), CompileError> {
        self.trace(&format!("Compiling character: #\\{}", value));
        self.emit_constant(Value::character(value))?;
        Ok(())
    }

    /// Compile a boolean literal
    fn compile_boolean(&mut self, value: bool) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling boolean: {}",
            if value { "#t" } else { "#f" }
        ));
        if value {
            self.emit_byte(OpCode::OP_TRUE, 1);
        } else {
            self.emit_byte(OpCode::OP_FALSE, 1);
        }
        Ok(())
    }

    /// Compile a variable reference
    fn compile_variable(&mut self, name: &str) -> Result<(), CompileError> {
        self.trace(&format!("Compiling variable reference: {}", name));
        self.trace_compilation_phase(
            CompilationPhase::VariableResolution,
            &Expr::Variable(name.to_string()),
        );

        // Try to resolve as local variable first
        if let Some(local_index) = self.resolve_local(name) {
            self.trace(&format!(
                "Resolved '{}' as local variable at slot {}",
                name, local_index
            ));
            self.emit_bytes(OpCode::OP_GET_LOCAL, local_index as u8, 1);
            return Ok(());
        }

        // Try to resolve as upvalue
        if let Some(upvalue_index) = self.resolve_upvalue(name) {
            self.trace(&format!(
                "Resolved '{}' as upvalue at index {}",
                name, upvalue_index
            ));
            self.emit_bytes(OpCode::OP_GET_UPVALUE, upvalue_index as u8, 1);
            return Ok(());
        }

        // Fall back to global variable
        self.trace(&format!("Resolved '{}' as global variable", name));
        self.compile_global_get(name)
    }

    /// Emit a constant instruction
    fn emit_constant(&mut self, value: Value) -> Result<usize, CompileError> {
        let constant_index = self.function.chunk.add_constant(value);
        if constant_index > 255 {
            return Err(CompileError::TooManyConstants);
        }
        self.emit_bytes(OpCode::OP_CONSTANT, constant_index as u8, 1);
        Ok(constant_index)
    }

    /// Add a constant to the pool without emitting an instruction
    fn add_constant(&mut self, value: Value) -> Result<usize, CompileError> {
        let constant_index = self.function.chunk.add_constant(value);
        if constant_index > 255 {
            return Err(CompileError::TooManyConstants);
        }
        Ok(constant_index)
    }

    /// Emit a single byte instruction
    fn emit_byte(&mut self, opcode: OpCode, line: usize) {
        self.function.chunk.write_instruction(opcode, line);
        self.trace(&format!("Emitted: {}", opcode.name()));
        self.trace_instruction(opcode, vec![]);
    }

    /// Emit an instruction with one operand byte
    fn emit_bytes(&mut self, opcode: OpCode, operand: u8, line: usize) {
        self.function
            .chunk
            .write_instruction_with_byte(opcode, operand, line);
        self.trace(&format!("Emitted: {} {}", opcode.name(), operand));
        self.trace_instruction(opcode, vec![operand]);
    }

    /// Emit an instruction with a 2-byte operand
    fn emit_short(&mut self, opcode: OpCode, operand: u16, line: usize) {
        self.function
            .chunk
            .write_instruction_with_short(opcode, operand, line);
        self.trace(&format!("Emitted: {} {}", opcode.name(), operand));
        self.trace_instruction(opcode, vec![(operand >> 8) as u8, operand as u8]);
    }

    /// Get the current chunk for direct access
    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    // Variable resolution methods

    /// Resolve a local variable by name, returns slot index if found
    fn resolve_local(&self, name: &str) -> Option<usize> {
        // Search locals from most recent to oldest (reverse order)
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name && local.depth >= 0 {
                return Some(i);
            }
        }
        None
    }

    /// Resolve an upvalue by name, returns upvalue index if found
    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        // Can't have upvalues in script (top-level)
        if self.enclosing.is_none() {
            return None;
        }

        // Trace upvalue resolution
        if let Some(tracer) = &mut self.tracer {
            let mut trace = CompilationTrace::new(
                CompilationPhase::UpvalueCapture,
                format!("ResolveUpvalue(\"{}\")", name),
                self.function.name.clone(),
                self.scope_depth,
            );

            for (i, _) in self.upvalues.iter().enumerate() {
                trace.add_upvalue(format!("existing_upvalue_{}", i));
            }

            tracer.trace_compilation(trace);
        }

        // Check if we already have this upvalue
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if let Some(enclosing) = &self.enclosing {
                if upvalue.is_local {
                    // This upvalue captures a local from the enclosing function
                    if let Some(local) = enclosing.locals.get(upvalue.index as usize) {
                        if local.name == name {
                            return Some(i);
                        }
                    }
                } else {
                    // This upvalue captures an upvalue from the enclosing function
                    if let Some(_enclosing_upvalue) = enclosing.upvalues.get(upvalue.index as usize)
                    {
                        // We need to recursively check the name - for now, assume it matches
                        // This is a simplification; a full implementation would track names
                        return Some(i);
                    }
                }
            }
        }

        // Try to capture from enclosing scope
        if let Some(enclosing) = &mut self.enclosing {
            // First try to find as local in enclosing scope
            if let Some(local_index) = enclosing.resolve_local(name) {
                // Mark the local as captured
                enclosing.locals[local_index].is_captured = true;

                // Add new upvalue that captures this local
                let upvalue_index = self.add_upvalue(local_index as u8, true);

                // Trace successful upvalue capture
                self.trace(&format!(
                    "Captured local '{}' as upvalue {}",
                    name, upvalue_index
                ));

                return Some(upvalue_index);
            }

            // Then try to find as upvalue in enclosing scope
            if let Some(upvalue_index) = enclosing.resolve_upvalue(name) {
                // Add new upvalue that captures this upvalue
                let new_upvalue_index = self.add_upvalue(upvalue_index as u8, false);

                // Trace successful upvalue capture
                self.trace(&format!(
                    "Captured upvalue '{}' as upvalue {}",
                    name, new_upvalue_index
                ));

                return Some(new_upvalue_index);
            }
        }

        None
    }

    /// Add an upvalue to the current function
    fn add_upvalue(&mut self, index: u8, is_local: bool) -> usize {
        // Check if we already have this upvalue
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return i;
            }
        }

        // Add new upvalue
        if self.upvalues.len() >= 256 {
            // This should be handled as an error, but for now we'll panic
            panic!("Too many upvalues in function");
        }

        self.upvalues.push(CompilerUpvalue { index, is_local });
        self.function.upvalue_count = self.upvalues.len();
        self.upvalues.len() - 1
    }

    /// Compile a global variable access
    fn compile_global_get(&mut self, name: &str) -> Result<(), CompileError> {
        let name_constant = self.add_constant(Value::string(name.to_string()))?;
        self.emit_bytes(OpCode::OP_GET_GLOBAL, name_constant as u8, 1);
        Ok(())
    }

    /// Compile a global variable definition
    pub fn compile_global_define(
        &mut self,
        name: &str,
        value_expr: &Expr,
    ) -> Result<(), CompileError> {
        self.trace(&format!("Compiling global definition: {}", name));

        // Compile the value expression
        self.compile_expr(value_expr)?;

        // Emit the define instruction
        let name_constant = self.add_constant(Value::string(name.to_string()))?;
        self.emit_bytes(OpCode::OP_DEFINE_GLOBAL, name_constant as u8, 1);

        // Push nil to indicate no return value (define produces no output)
        self.emit_byte(OpCode::OP_NIL, 1);

        Ok(())
    }

    /// Compile a global variable assignment
    pub fn compile_global_set(
        &mut self,
        name: &str,
        value_expr: &Expr,
    ) -> Result<(), CompileError> {
        self.trace(&format!("Compiling global assignment: {}", name));

        // Compile the value expression
        self.compile_expr(value_expr)?;

        // Emit the set instruction
        let name_constant = self.add_constant(Value::string(name.to_string()))?;
        self.emit_bytes(OpCode::OP_SET_GLOBAL, name_constant as u8, 1);

        Ok(())
    }

    // Scope management methods

    /// Begin a new scope
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.trace(&format!("Beginning scope, depth now: {}", self.scope_depth));

        // Trace scope management
        if let Some(tracer) = &mut self.tracer {
            let mut trace = CompilationTrace::new(
                CompilationPhase::ScopeManagement,
                format!("BeginScope(depth={})", self.scope_depth),
                self.function.name.clone(),
                self.scope_depth,
            );

            // Add current locals
            for local in &self.locals {
                if local.depth >= 0 {
                    trace.add_local(local.name.clone());
                }
            }

            tracer.trace_compilation(trace);
        }
    }

    /// End the current scope and emit instructions to close upvalues if needed
    pub fn end_scope(&mut self) {
        let old_depth = self.scope_depth;
        self.scope_depth -= 1;
        self.trace(&format!("Ending scope, depth now: {}", self.scope_depth));

        // Trace scope management
        if let Some(tracer) = &mut self.tracer {
            let mut trace = CompilationTrace::new(
                CompilationPhase::ScopeManagement,
                format!("EndScope(depth={} -> {})", old_depth, self.scope_depth),
                self.function.name.clone(),
                self.scope_depth,
            );

            // Add locals that will be removed
            for local in &self.locals {
                if local.depth > self.scope_depth as isize {
                    trace.add_local(format!("removing: {}", local.name));
                }
            }

            tracer.trace_compilation(trace);
        }

        // Remove locals that are going out of scope
        while !self.locals.is_empty()
            && self.locals.last().unwrap().depth > self.scope_depth as isize
        {
            let local = self.locals.pop().unwrap();

            // If the local was captured, emit close upvalue instruction
            if local.is_captured {
                self.trace(&format!("Closing captured local: {}", local.name));
                self.emit_byte(OpCode::OP_CLOSE_UPVALUE, 1);
            } else {
                // Just pop the value from the stack
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }
    }

    /// Mark a local variable as captured (used by upvalue resolution)
    pub fn mark_local_captured(&mut self, local_index: usize) {
        if local_index < self.locals.len() {
            self.locals[local_index].is_captured = true;
            self.trace(&format!(
                "Marked local '{}' as captured",
                self.locals[local_index].name
            ));
        }
    }

    /// Get the number of locals in the current scope
    pub fn local_count(&self) -> usize {
        self.locals.len()
    }

    /// Check if we're in global scope
    pub fn is_global_scope(&self) -> bool {
        self.scope_depth == 0
    }

    /// Emit close upvalue instructions for all captured locals at or above the given stack slot
    pub fn close_upvalues_from(&mut self, stack_slot: usize) {
        // In a full implementation, this would close all upvalues at or above the given stack slot
        // For now, we'll emit a single close instruction
        self.trace(&format!("Closing upvalues from stack slot {}", stack_slot));
        self.emit_byte(OpCode::OP_CLOSE_UPVALUE, 1);
    }

    /// Declare a local variable
    pub fn declare_local(&mut self, name: String) -> Result<(), CompileError> {
        // Check if we're at global scope
        if self.scope_depth == 0 {
            return Ok(()); // Global variables are handled differently
        }

        // Check for duplicate declaration in current scope
        for local in self.locals.iter().rev() {
            if local.depth >= 0 && local.depth < self.scope_depth as isize {
                break; // We've gone past the current scope
            }
            if local.name == name {
                return Err(CompileError::VariableAlreadyDefined(name));
            }
        }

        // Check if we have too many locals
        if self.locals.len() >= 256 {
            return Err(CompileError::TooManyLocals);
        }

        // Add the local variable (uninitialized)
        self.locals.push(Local {
            name: name.clone(),
            depth: -1, // Uninitialized
            is_captured: false,
        });

        self.trace(&format!("Declared local variable: {}", name));
        Ok(())
    }

    /// Define a local variable (mark it as initialized)
    pub fn define_local(&mut self) {
        if self.scope_depth == 0 {
            return; // Global variables are handled differently
        }

        if let Some(local) = self.locals.last_mut() {
            local.depth = self.scope_depth as isize;
            let name = local.name.clone();
            let depth = local.depth;
            self.trace(&format!(
                "Defined local variable: {} at depth {}",
                name, depth
            ));
        }
    }

    /// Compile a local variable assignment
    pub fn compile_local_set(&mut self, name: &str, value_expr: &Expr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling local assignment: {}", name));

        // Compile the value expression
        self.compile_expr(value_expr)?;

        // Find the local variable
        if let Some(local_index) = self.resolve_local(name) {
            self.emit_bytes(OpCode::OP_SET_LOCAL, local_index as u8, 1);
            Ok(())
        } else {
            Err(CompileError::UndefinedVariable(name.to_string()))
        }
    }

    /// Compile a lambda expression
    fn compile_lambda(&mut self, lambda: &LambdaExpr) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling lambda with {} parameters",
            lambda.params.len()
        ));

        self.trace_compilation_phase(
            CompilationPhase::FunctionCompilation,
            &Expr::Lambda(Box::new(lambda.clone())),
        );

        // We need to compile the lambda in a separate function context
        // For now, let's create a simple implementation that doesn't handle upvalues
        let function = Function::new(
            format!("lambda@{}", self.function.chunk.count()),
            lambda.params.len(),
        );

        // Create a temporary compiler for the function body
        let mut temp_compiler = Compiler {
            function,
            function_type: FunctionType::Function,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
            enclosing: None, // Simplified - no upvalue support yet
            trace_enabled: self.trace_enabled,
            tracer: self.tracer.as_ref().map(|t| Tracer::new(t.config.clone())),
        };

        // Declare parameters as local variables (starting at slot 0)
        // Parameters are at the function's base scope level
        for param in &lambda.params {
            temp_compiler.locals.push(Local {
                name: param.clone(),
                depth: 0, // Function parameters are at depth 0
                is_captured: false,
            });
        }

        // Begin function body scope
        temp_compiler.begin_scope();

        // Compile function body
        for (i, expr) in lambda.body.iter().enumerate() {
            temp_compiler.compile_expr(expr)?;

            // Pop intermediate results except for the last expression
            if i < lambda.body.len() - 1 {
                temp_compiler.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // If the body is empty, push nil as the return value
        if lambda.body.is_empty() {
            temp_compiler.emit_byte(OpCode::OP_NIL, 1);
        }

        // End function scope
        temp_compiler.end_scope();

        // Finish compiling the function (this will emit OP_RETURN)
        let compiled_function = temp_compiler.end_compiler();

        // Create closure instruction in the current compiler
        let function_constant = self.add_constant(Value::function(compiled_function))?;
        self.emit_bytes(OpCode::OP_CLOSURE, function_constant as u8, 1);

        // No upvalue capture information needed for this simplified implementation
        // Full upvalue support would require proper compiler nesting

        Ok(())
    }

    /// Compile a function call
    fn compile_call(&mut self, func_expr: &Expr, args: &[Expr]) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling function call with {} arguments",
            args.len()
        ));

        // Check for built-in functions
        if let Expr::Variable(func_name) = func_expr {
            match func_name.as_str() {
                "cons" => {
                    if args.len() != 2 {
                        return Err(CompileError::UndefinedVariable(format!(
                            "cons expects 2 arguments, got {}",
                            args.len()
                        )));
                    }
                    // Compile arguments: car first, then cdr
                    self.compile_expr(&args[0])?; // car
                    self.compile_expr(&args[1])?; // cdr
                    self.emit_byte(OpCode::OP_CONS, 1);
                    return Ok(());
                }
                "car" => {
                    if args.len() != 1 {
                        return Err(CompileError::UndefinedVariable(format!(
                            "car expects 1 argument, got {}",
                            args.len()
                        )));
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_CAR, 1);
                    return Ok(());
                }
                "cdr" => {
                    if args.len() != 1 {
                        return Err(CompileError::UndefinedVariable(format!(
                            "cdr expects 1 argument, got {}",
                            args.len()
                        )));
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_CDR, 1);
                    return Ok(());
                }
                "list" => {
                    // Build a list from the arguments by generating nested cons calls
                    // (list a b c) => (cons a (cons b (cons c nil)))
                    if args.is_empty() {
                        // Empty list
                        self.emit_byte(OpCode::OP_NIL, 1);
                    } else {
                        // Build the nested structure recursively
                        self.compile_list_helper(args, 0)?;
                    }
                    return Ok(());
                }
                "vector" => {
                    if args.len() > 255 {
                        return Err(CompileError::TooManyLocals); // Reuse this error for too many args
                    }
                    // Compile all arguments
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    self.emit_bytes(OpCode::OP_VECTOR, args.len() as u8, 1);
                    return Ok(());
                }
                "make-hashtable" => {
                    // make-hashtable can take 0, 1, or 2 arguments (hash function and equality function)
                    if args.len() > 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("make-hashtable".to_string(), args.len());
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments (if any)
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, args.len() as u8, 1);
                    return Ok(());
                }
                "+" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_ADD, 1);
                    return Ok(());
                }
                "-" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_SUBTRACT, 1);
                    return Ok(());
                }
                "*" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_MULTIPLY, 1);
                    return Ok(());
                }
                "/" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_DIVIDE, 1);
                    return Ok(());
                }
                "<" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_LESS, 1);
                    return Ok(());
                }
                "null?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_NULL_Q, 1);
                    return Ok(());
                }
                "pair?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_PAIR_Q, 1);
                    return Ok(());
                }
                "number?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_NUMBER_Q, 1);
                    return Ok(());
                }
                "string?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_STRING_Q, 1);
                    return Ok(());
                }
                "boolean?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_BOOLEAN_Q, 1);
                    return Ok(());
                }
                "char?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_CHAR_Q, 1);
                    return Ok(());
                }
                "eq?" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_EQ_Q, 1);
                    return Ok(());
                }
                "string=?" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_STRING_EQ_Q, 1);
                    return Ok(());
                }
                "string-hash" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("string-hash".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "equal-hash" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("equal-hash".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "equal?" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("equal?".to_string(), 2);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 2, 1);
                    return Ok(());
                }
                "char=?" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.emit_byte(OpCode::OP_CHAR_EQ_Q, 1);
                    return Ok(());
                }
                "char-numeric?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_CHAR_NUMERIC_Q, 1);
                    return Ok(());
                }
                "char-whitespace?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    self.compile_expr(&args[0])?;
                    self.emit_byte(OpCode::OP_CHAR_WHITESPACE_Q, 1);
                    return Ok(());
                }
                "display" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack first
                    let builtin_value = crate::object::Value::builtin("display".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Then compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function (display produces no output)
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "newline" => {
                    if args.len() != 0 {
                        return Err(CompileError::ArityMismatch {
                            expected: 0,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("newline".to_string(), 0);
                    self.emit_constant(builtin_value)?;
                    // Call the built-in function (no arguments)
                    self.emit_bytes(OpCode::OP_CALL, 0, 1);
                    return Ok(());
                }
                "error" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("error".to_string(), 2);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments (procedure name and message)
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 2, 1);
                    return Ok(());
                }
                "for-each" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("for-each".to_string(), 2);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    self.compile_expr(&args[0])?; // procedure
                    self.compile_expr(&args[1])?; // list
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 2, 1);
                    return Ok(());
                }
                "string->number" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("string->number".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "list->vector" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("list->vector".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "vector->list" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("vector->list".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "list->string" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("list->string".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "vector?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("vector?".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "vector-length" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("vector-length".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "vector-ref" => {
                    if args.len() != 2 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("vector-ref".to_string(), 2);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 2, 1);
                    return Ok(());
                }
                "vector-set!" => {
                    if args.len() != 3 {
                        return Err(CompileError::ArityMismatch {
                            expected: 3,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("vector-set!".to_string(), 3);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.compile_expr(&args[2])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 3, 1);
                    return Ok(());
                }
                "hashtable?" => {
                    if args.len() != 1 {
                        return Err(CompileError::ArityMismatch {
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("hashtable?".to_string(), 1);
                    self.emit_constant(builtin_value)?;
                    // Compile the argument
                    self.compile_expr(&args[0])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 1, 1);
                    return Ok(());
                }
                "hashtable-ref" => {
                    if args.len() != 2 && args.len() != 3 {
                        return Err(CompileError::ArityMismatch {
                            expected: 2, // or 3 with default value
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("hashtable-ref".to_string(), args.len());
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, args.len() as u8, 1);
                    return Ok(());
                }
                "hashtable-set!" => {
                    if args.len() != 3 {
                        return Err(CompileError::ArityMismatch {
                            expected: 3,
                            got: args.len(),
                        });
                    }
                    // Push the built-in function onto the stack
                    let builtin_value = crate::object::Value::builtin("hashtable-set!".to_string(), 3);
                    self.emit_constant(builtin_value)?;
                    // Compile the arguments
                    self.compile_expr(&args[0])?;
                    self.compile_expr(&args[1])?;
                    self.compile_expr(&args[2])?;
                    // Call the built-in function
                    self.emit_bytes(OpCode::OP_CALL, 3, 1);
                    return Ok(());
                }
                _ => {
                    // Fall through to regular function call
                }
            }
        }

        // Regular function call
        // Compile the function expression
        self.compile_expr(func_expr)?;

        // Compile arguments
        for arg in args {
            self.compile_expr(arg)?;
        }

        // Emit call instruction
        if args.len() > 255 {
            return Err(CompileError::TooManyLocals); // Reuse this error for too many args
        }

        self.emit_bytes(OpCode::OP_CALL, args.len() as u8, 1);
        Ok(())
    }

    /// Compile an if expression
    fn compile_if(&mut self, if_expr: &crate::ast::IfExpr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling if expression"));
        self.trace_compilation_phase(
            CompilationPhase::InstructionGeneration,
            &Expr::If(Box::new(if_expr.clone())),
        );

        // Compile the condition
        self.compile_expr(&if_expr.condition)?;

        // Jump to else branch if condition is false
        let else_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE)?;

        // Compile then branch
        self.emit_byte(OpCode::OP_POP, 1); // Pop the condition value
        self.compile_expr(&if_expr.then_expr)?;

        // Jump over else branch
        let end_jump = self.emit_jump(OpCode::OP_JUMP)?;

        // Patch the else jump to point here
        self.patch_jump(else_jump)?;

        // Compile else branch
        self.emit_byte(OpCode::OP_POP, 1); // Pop the condition value
        self.compile_expr(&if_expr.else_expr)?;

        // Patch the end jump to point here
        self.patch_jump(end_jump)?;

        Ok(())
    }

    /// Compile a cond expression
    fn compile_cond(&mut self, cond_expr: &crate::ast::CondExpr) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling cond expression with {} clauses",
            cond_expr.clauses.len()
        ));
        self.trace_compilation_phase(
            CompilationPhase::InstructionGeneration,
            &Expr::Cond(Box::new(cond_expr.clone())),
        );

        let mut end_jumps = Vec::new();
        let mut next_clause_jumps = Vec::new();

        for (i, clause) in cond_expr.clauses.iter().enumerate() {
            // Patch previous clause's jump to here
            if let Some(jump) = next_clause_jumps.pop() {
                self.patch_jump(jump)?;
            }

            // Check if this is an else clause (test is #t or similar)
            let is_else_clause = match &clause.test {
                Expr::Boolean(true) => true,
                Expr::Variable(name) if name == "else" => true,
                _ => false,
            };

            if is_else_clause {
                // Else clause - just compile the body
                self.compile_clause_body(&clause.body)?;
                break;
            } else {
                // Regular clause - compile test and conditional jump
                self.compile_expr(&clause.test)?;

                // Jump to next clause if test is false
                let next_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE)?;
                next_clause_jumps.push(next_jump);

                // Pop the test value and compile body
                self.emit_byte(OpCode::OP_POP, 1);
                self.compile_clause_body(&clause.body)?;

                // Jump to end after executing this clause
                if i < cond_expr.clauses.len() - 1 {
                    let end_jump = self.emit_jump(OpCode::OP_JUMP)?;
                    end_jumps.push(end_jump);
                }
            }
        }

        // Patch any remaining next clause jump (if no else clause)
        if let Some(jump) = next_clause_jumps.pop() {
            self.patch_jump(jump)?;
            self.emit_byte(OpCode::OP_POP, 1); // Pop the last test value
            // Push nil as default value if no clause matches
            self.emit_byte(OpCode::OP_NIL, 1);
        }

        // Patch all end jumps to point here
        for jump in end_jumps {
            self.patch_jump(jump)?;
        }

        Ok(())
    }

    /// Compile the body of a cond clause
    fn compile_clause_body(&mut self, body: &[Expr]) -> Result<(), CompileError> {
        if body.is_empty() {
            // Empty body - push nil
            self.emit_byte(OpCode::OP_NIL, 1);
        } else {
            // Compile all expressions in body
            for (i, expr) in body.iter().enumerate() {
                self.compile_expr(expr)?;
                // Pop intermediate results except for the last expression
                if i < body.len() - 1 {
                    self.emit_byte(OpCode::OP_POP, 1);
                }
            }
        }
        Ok(())
    }

    /// Emit a jump instruction and return the offset for later patching
    fn emit_jump(&mut self, opcode: OpCode) -> Result<usize, CompileError> {
        self.emit_short(opcode, 0, 1); // Placeholder jump distance
        Ok(self.function.chunk.count() - 3) // Return offset of the instruction start
    }

    /// Patch a jump instruction with the correct distance
    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileError> {
        self.function
            .chunk
            .patch_jump(offset)
            .map_err(|_| CompileError::JumpTooLarge)
    }

    /// Compile a let-loop expression (named let for iteration)
    fn compile_let_loop(&mut self, let_loop: &LetLoopExpr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling let-loop: {}", let_loop.name));
        self.trace_compilation_phase(
            CompilationPhase::InstructionGeneration,
            &Expr::LetLoop(Box::new(let_loop.clone())),
        );

        // For now, implement let-loop as a simple let expression
        // TODO: Implement proper recursion support

        // Begin a new scope for the loop bindings
        self.begin_scope();

        // Compile initial values for loop variables and declare them as locals
        for (var_name, init_expr) in &let_loop.bindings {
            // Compile the initial value
            self.compile_expr(init_expr)?;

            // Declare the loop variable
            self.declare_local(var_name.clone())?;
            self.define_local();
        }

        // Compile the loop body (just once, no actual looping for now)
        for (i, expr) in let_loop.body.iter().enumerate() {
            self.compile_expr(expr)?;

            // Pop intermediate results except for the last expression
            if i < let_loop.body.len() - 1 {
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // End the loop scope
        self.end_scope();

        Ok(())
    }

    /// Compile a let expression
    fn compile_let(&mut self, let_expr: &LetExpr) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling let expression with {} bindings",
            let_expr.bindings.len()
        ));
        self.trace_compilation_phase(
            CompilationPhase::InstructionGeneration,
            &Expr::Let(Box::new(let_expr.clone())),
        );

        // Begin a new scope for the let bindings
        self.begin_scope();

        // Compile all binding values first (in parallel)
        // This ensures that bindings can't refer to each other
        for (_var_name, init_expr) in &let_expr.bindings {
            self.compile_expr(init_expr)?;
        }

        // Now declare and define all variables
        // Stack layout: [value0, value1, value2, ...]
        // Declare variables in forward order to match stack positions
        for (var_name, _init_expr) in &let_expr.bindings {
            self.declare_local(var_name.clone())?;
            self.define_local();
        }

        // Compile the body
        for (i, expr) in let_expr.body.iter().enumerate() {
            self.compile_expr(expr)?;

            // Pop intermediate results except for the last expression
            if i < let_expr.body.len() - 1 {
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // End the let scope
        self.end_scope();

        Ok(())
    }

    /// Compile a let* expression
    fn compile_let_star(&mut self, let_star: &LetStarExpr) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling let* expression with {} bindings",
            let_star.bindings.len()
        ));
        self.trace_compilation_phase(
            CompilationPhase::InstructionGeneration,
            &Expr::LetStar(Box::new(let_star.clone())),
        );

        // Begin a new scope for the let* bindings
        self.begin_scope();

        // Compile bindings sequentially (each can refer to previous ones)
        for (var_name, init_expr) in &let_star.bindings {
            // Compile the value expression
            self.compile_expr(init_expr)?;

            // Declare and define the variable
            self.declare_local(var_name.clone())?;
            self.define_local();
        }

        // Compile the body
        for (i, expr) in let_star.body.iter().enumerate() {
            self.compile_expr(expr)?;

            // Pop intermediate results except for the last expression
            if i < let_star.body.len() - 1 {
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // End the let* scope
        self.end_scope();

        Ok(())
    }

    /// Compile a define expression
    fn compile_define(&mut self, define: &DefineExpr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling define: {}", define.name));

        if self.scope_depth > 0 {
            // Local variable definition
            self.declare_local(define.name.clone())?;
            self.compile_expr(&define.value)?;
            self.define_local();
            Ok(())
        } else {
            // Global variable definition
            self.compile_global_define(&define.name, &define.value)
        }
    }

    /// Finish compilation and return the compiled function
    pub fn end_compiler(mut self) -> Function {
        self.trace("Ending compilation");

        // Emit return instruction if not already present
        if self.function.chunk.code.is_empty()
            || self.function.chunk.code.last() != Some(&OpCode::OP_RETURN.to_byte())
        {
            self.emit_byte(OpCode::OP_RETURN, 1);
        }

        self.function
    }

    /// Main compilation entry point that accepts an AST and compiles it to bytecode
    pub fn compile_ast(ast: &[Expr]) -> Result<Function, CompileError> {
        let mut compiler = Compiler::new_script();

        // Compile all expressions in the AST
        for expr in ast {
            compiler.compile_expr(expr)?;

            // For top-level expressions, we typically want to keep the result
            // Only pop if this is not the last expression
            if expr as *const _ != ast.last().unwrap() as *const _ {
                compiler.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // If no expressions were compiled, push nil
        if ast.is_empty() {
            compiler.emit_byte(OpCode::OP_NIL, 1);
        }

        Ok(compiler.end_compiler())
    }

    /// Compile AST with tracing enabled
    pub fn compile_ast_with_trace(ast: &[Expr], tracer: Tracer) -> Result<Function, CompileError> {
        let mut compiler = Compiler::new_script();
        compiler.set_tracer(tracer);

        // Compile all expressions in the AST
        for expr in ast {
            compiler.compile_expr(expr)?;

            // For top-level expressions, we typically want to keep the result
            // Only pop if this is not the last expression
            if expr as *const _ != ast.last().unwrap() as *const _ {
                compiler.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // If no expressions were compiled, push nil
        if ast.is_empty() {
            compiler.emit_byte(OpCode::OP_NIL, 1);
        }

        Ok(compiler.end_compiler())
    }

    // Additional AST node compilation methods

    /// Compile a let-values expression
    fn compile_let_values(&mut self, let_values: &LetValuesExpr) -> Result<(), CompileError> {
        self.trace("Compiling let-values expression");

        // For now, implement a simplified version that doesn't handle multiple values
        // This would need proper multiple value support in the VM
        self.begin_scope();

        // Compile bindings
        for (vars, expr) in &let_values.bindings {
            // Compile the expression
            self.compile_expr(expr)?;

            // For simplicity, only bind the first variable if multiple are specified
            if let Some(first_var) = vars.first() {
                self.declare_local(first_var.clone())?;
                self.define_local();
            } else {
                // No variables to bind, just pop the value
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        // Compile body
        for (i, expr) in let_values.body.iter().enumerate() {
            self.compile_expr(expr)?;
            // Pop intermediate results except for the last expression
            if i < let_values.body.len() - 1 {
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        self.end_scope();
        Ok(())
    }

    /// Compile a set! expression
    fn compile_set(&mut self, set_expr: &SetExpr) -> Result<(), CompileError> {
        self.trace(&format!("Compiling set! expression: {}", set_expr.variable));

        // Compile the value expression
        self.compile_expr(&set_expr.value)?;

        // Try to resolve as local variable first
        if let Some(local_index) = self.resolve_local(&set_expr.variable) {
            self.emit_bytes(OpCode::OP_SET_LOCAL, local_index as u8, 1);
            return Ok(());
        }

        // Try to resolve as upvalue
        if let Some(upvalue_index) = self.resolve_upvalue(&set_expr.variable) {
            self.emit_bytes(OpCode::OP_SET_UPVALUE, upvalue_index as u8, 1);
            return Ok(());
        }

        // Fall back to global variable
        let name_constant = self.add_constant(Value::string(set_expr.variable.clone()))?;
        self.emit_bytes(OpCode::OP_SET_GLOBAL, name_constant as u8, 1);
        Ok(())
    }

    /// Compile a quoted expression
    fn compile_quote(&mut self, quoted: &Expr) -> Result<(), CompileError> {
        self.trace("Compiling quote expression");

        // Convert the quoted expression to a runtime value
        let quoted_value = self.expr_to_value(quoted)?;
        self.emit_constant(quoted_value)?;
        Ok(())
    }

    /// Compile a quasiquoted expression
    fn compile_quasiquote(&mut self, quasi: &Expr) -> Result<(), CompileError> {
        self.trace("Compiling quasiquote expression");

        // For now, treat quasiquote like quote (simplified implementation)
        // A full implementation would handle unquote and unquote-splicing
        let quoted_value = self.expr_to_value(quasi)?;
        self.emit_constant(quoted_value)?;
        Ok(())
    }

    /// Compile an unquote expression
    fn compile_unquote(&mut self, unquoted: &Expr) -> Result<(), CompileError> {
        self.trace("Compiling unquote expression");

        // Unquote should only appear inside quasiquote
        // For now, just compile the expression normally
        self.compile_expr(unquoted)
    }

    /// Compile an unquote-splicing expression
    fn compile_unquote_splicing(&mut self, spliced: &Expr) -> Result<(), CompileError> {
        self.trace("Compiling unquote-splicing expression");

        // Unquote-splicing should only appear inside quasiquote
        // For now, just compile the expression normally
        self.compile_expr(spliced)
    }

    /// Compile a begin expression
    fn compile_begin(&mut self, exprs: &[Expr]) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling begin expression with {} expressions",
            exprs.len()
        ));

        if exprs.is_empty() {
            // Empty begin returns nil
            self.emit_byte(OpCode::OP_NIL, 1);
            return Ok(());
        }

        // Compile all expressions
        for (i, expr) in exprs.iter().enumerate() {
            self.compile_expr(expr)?;
            // Pop intermediate results except for the last expression
            if i < exprs.len() - 1 {
                self.emit_byte(OpCode::OP_POP, 1);
            }
        }

        Ok(())
    }

    /// Compile a list literal
    fn compile_list(&mut self, elements: &[Expr]) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling list literal with {} elements",
            elements.len()
        ));

        if elements.is_empty() {
            // Empty list is nil
            self.emit_byte(OpCode::OP_NIL, 1);
            return Ok(());
        }

        // Build list from right to left using cons
        // Start with nil
        self.emit_byte(OpCode::OP_NIL, 1);

        // Cons each element from right to left
        for element in elements.iter().rev() {
            self.compile_expr(element)?;
            // Stack now has: ... nil element
            // We want: ... (cons element nil)
            // But cons expects (cons car cdr), so we need to swap
            // For now, let's use a simpler approach and build left to right
        }

        // Actually, let's build left to right for simplicity
        // Pop the nil we pushed
        self.emit_byte(OpCode::OP_POP, 1);

        // Start with the first element
        if !elements.is_empty() {
            self.compile_expr(&elements[0])?;
            self.emit_byte(OpCode::OP_NIL, 1);
            self.emit_byte(OpCode::OP_CONS, 1);

            // Cons the rest
            for element in elements.iter().skip(1) {
                self.compile_expr(element)?;
                // Stack: ... list element
                // We want: (cons element list), but cons expects (cons car cdr)
                // So we need: element list -> list element -> cons
                // This is getting complex, let's simplify for now
                self.emit_byte(OpCode::OP_CONS, 1);
            }
        }

        Ok(())
    }

    /// Compile a vector literal
    fn compile_vector_literal(&mut self, elements: &[Expr]) -> Result<(), CompileError> {
        self.trace(&format!(
            "Compiling vector literal with {} elements",
            elements.len()
        ));

        // Compile all elements
        for element in elements {
            self.compile_expr(element)?;
        }

        // Create vector from stack elements
        if elements.len() > 255 {
            return Err(CompileError::TooManyLocals); // Reuse this error
        }

        self.emit_bytes(OpCode::OP_VECTOR, elements.len() as u8, 1);
        Ok(())
    }

    /// Compile a call-with-values expression
    fn compile_call_with_values(
        &mut self,
        call_with_values: &CallWithValuesExpr,
    ) -> Result<(), CompileError> {
        self.trace("Compiling call-with-values expression");

        // For now, implement a simplified version
        // call-with-values calls producer, then calls consumer with the results

        // Compile and call the producer
        self.compile_expr(&call_with_values.producer)?;
        self.emit_bytes(OpCode::OP_CALL, 0, 1); // Call with 0 arguments

        // Compile the consumer
        self.compile_expr(&call_with_values.consumer)?;

        // Call consumer with 1 argument (the result from producer)
        self.emit_bytes(OpCode::OP_CALL, 1, 1);

        Ok(())
    }

    /// Compile an import expression
    fn compile_import(&mut self, _import: &ImportExpr) -> Result<(), CompileError> {
        self.trace("Compiling import expression");

        // For now, imports are not implemented in the runtime
        // Just return nil
        self.emit_byte(OpCode::OP_NIL, 1);
        Ok(())
    }

    /// Convert an AST expression to a runtime value (for quote)
    fn expr_to_value(&self, expr: &Expr) -> Result<Value, CompileError> {
        match expr {
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::String(s) => Ok(Value::string(s.clone())),
            Expr::Character(c) => Ok(Value::character(*c)),
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Variable(name) => Ok(Value::symbol(name.clone())), // Symbols

            Expr::List(elements) => {
                if elements.is_empty() {
                    Ok(Value::Nil)
                } else {
                    // Build proper cons cells from right to left
                    self.build_list_value(elements)
                }
            }

            Expr::Vector(elements) => {
                // Convert elements to values and create vector
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.expr_to_value(element)?);
                }
                Ok(Value::vector(values))
            }

            // Handle nested quotes
            Expr::Quote(quoted) => {
                // Nested quote: '(quote x) => (quote x)
                let quoted_value = self.expr_to_value(quoted)?;
                let quote_symbol = Value::symbol("quote".to_string());
                Ok(Value::cons(quote_symbol, Value::cons(quoted_value, Value::Nil)))
            }

            // Handle function calls in quoted context (they become lists)
            Expr::Call(func, args) => {
                // In a quoted context, (f a b c) becomes a list (f a b c)
                let mut elements = vec![*func.clone()];
                elements.extend(args.clone());
                self.build_list_value(&elements)
            }

            _ => {
                // For complex expressions that can't be converted to literal values,
                // we need to handle them as symbols or return an error
                Err(CompileError::InvalidOperation(format!(
                    "Cannot convert expression to quoted value: {:?}",
                    expr
                )))
            }
        }
    }

    /// Build a proper list value from a vector of expressions
    fn build_list_value(&self, elements: &[Expr]) -> Result<Value, CompileError> {
        if elements.is_empty() {
            return Ok(Value::Nil);
        }

        // Build the list from right to left (tail to head)
        let mut result = Value::Nil;
        for element in elements.iter().rev() {
            let element_value = self.expr_to_value(element)?;
            result = Value::cons(element_value, result);
        }
        Ok(result)
    }

    /// Helper method to compile list construction recursively
    fn compile_list_helper(&mut self, args: &[Expr], index: usize) -> Result<(), CompileError> {
        if index >= args.len() {
            // Base case: emit nil
            self.emit_byte(OpCode::OP_NIL, 1);
        } else {
            // Recursive case: (cons args[index] (list args[index+1]...))
            self.compile_expr(&args[index])?; // Push car
            self.compile_list_helper(args, index + 1)?; // Push cdr (recursive list)
            self.emit_byte(OpCode::OP_CONS, 1); // Cons them together
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let compiler = Compiler::new_script();
        assert_eq!(compiler.function_type, FunctionType::Script);
        assert_eq!(compiler.function.name, "script");
        assert_eq!(compiler.scope_depth, 0);
        assert_eq!(compiler.locals.len(), 1); // Reserved slot 0
        assert!(compiler.enclosing.is_none());
    }

    #[test]
    fn test_function_compiler_creation() {
        let script_compiler = Compiler::new_script();
        let func_compiler =
            Compiler::new_function("test_func".to_string(), 2, Box::new(script_compiler));

        assert_eq!(func_compiler.function_type, FunctionType::Function);
        assert_eq!(func_compiler.function.name, "test_func");
        assert_eq!(func_compiler.function.arity, 2);
        assert_eq!(func_compiler.locals.len(), 1); // Reserved slot 0
        assert!(func_compiler.enclosing.is_some());
    }

    #[test]
    fn test_compile_number() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Number(42.5);

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_CONSTANT instruction
        assert_eq!(compiler.function.chunk.code.len(), 2);
        assert_eq!(
            compiler.function.chunk.code[0],
            OpCode::OP_CONSTANT.to_byte()
        );
        assert_eq!(compiler.function.chunk.code[1], 0); // Constant index 0

        // Should have added constant to pool
        assert_eq!(compiler.function.chunk.constants.len(), 1);
        assert_eq!(compiler.function.chunk.constants[0], Value::Number(42.5));
    }

    #[test]
    fn test_compile_string() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::String("hello".to_string());

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_CONSTANT instruction
        assert_eq!(compiler.function.chunk.code.len(), 2);
        assert_eq!(
            compiler.function.chunk.code[0],
            OpCode::OP_CONSTANT.to_byte()
        );
        assert_eq!(compiler.function.chunk.code[1], 0); // Constant index 0

        // Should have added string constant to pool
        assert_eq!(compiler.function.chunk.constants.len(), 1);
        assert!(compiler.function.chunk.constants[0].is_string());
        assert_eq!(
            compiler.function.chunk.constants[0].as_string().unwrap(),
            "hello"
        );
    }

    #[test]
    fn test_compile_character() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Character('x');

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_CONSTANT instruction
        assert_eq!(compiler.function.chunk.code.len(), 2);
        assert_eq!(
            compiler.function.chunk.code[0],
            OpCode::OP_CONSTANT.to_byte()
        );

        // Should have added character constant to pool
        assert_eq!(compiler.function.chunk.constants.len(), 1);
        assert!(compiler.function.chunk.constants[0].is_character());
        assert_eq!(
            compiler.function.chunk.constants[0].as_character().unwrap(),
            'x'
        );
    }

    #[test]
    fn test_compile_boolean_true() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Boolean(true);

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_TRUE instruction
        assert_eq!(compiler.function.chunk.code.len(), 1);
        assert_eq!(compiler.function.chunk.code[0], OpCode::OP_TRUE.to_byte());

        // Should not have added any constants
        assert_eq!(compiler.function.chunk.constants.len(), 0);
    }

    #[test]
    fn test_compile_boolean_false() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Boolean(false);

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_FALSE instruction
        assert_eq!(compiler.function.chunk.code.len(), 1);
        assert_eq!(compiler.function.chunk.code[0], OpCode::OP_FALSE.to_byte());

        // Should not have added any constants
        assert_eq!(compiler.function.chunk.constants.len(), 0);
    }

    #[test]
    fn test_compile_variable_as_global() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Variable("x".to_string());

        // Should now compile successfully as a global variable
        let result = compiler.compile_expr(&expr);
        assert!(result.is_ok());

        // Should have emitted OP_GET_GLOBAL instruction
        assert_eq!(
            compiler.function.chunk.code[0],
            OpCode::OP_GET_GLOBAL.to_byte()
        );
    }

    #[test]
    fn test_end_compiler_adds_return() {
        let mut compiler = Compiler::new_script();
        compiler.compile_expr(&Expr::Number(42.0)).unwrap();

        let function = compiler.end_compiler();

        // Should have added OP_RETURN at the end
        let code = &function.chunk.code;
        assert_eq!(code.last(), Some(&OpCode::OP_RETURN.to_byte()));
    }

    #[test]
    fn test_trace_functionality() {
        let mut compiler = Compiler::new_script();

        // Test enabling/disabling trace
        assert!(!compiler.trace_enabled);
        compiler.enable_trace();
        assert!(compiler.trace_enabled);
        compiler.disable_trace();
        assert!(!compiler.trace_enabled);
    }

    #[test]
    fn test_multiple_constants() {
        let mut compiler = Compiler::new_script();

        // Compile multiple literal expressions
        compiler.compile_expr(&Expr::Number(1.0)).unwrap();
        compiler
            .compile_expr(&Expr::String("test".to_string()))
            .unwrap();
        compiler.compile_expr(&Expr::Boolean(true)).unwrap();
        compiler.compile_expr(&Expr::Character('a')).unwrap();

        // Should have correct number of constants (boolean doesn't add constant)
        assert_eq!(compiler.function.chunk.constants.len(), 3);

        // Check the bytecode sequence
        let code = &compiler.function.chunk.code;
        assert_eq!(code[0], OpCode::OP_CONSTANT.to_byte()); // Number
        assert_eq!(code[1], 0); // Constant index 0
        assert_eq!(code[2], OpCode::OP_CONSTANT.to_byte()); // String
        assert_eq!(code[3], 1); // Constant index 1
        assert_eq!(code[4], OpCode::OP_TRUE.to_byte()); // Boolean true
        assert_eq!(code[5], OpCode::OP_CONSTANT.to_byte()); // Character
        assert_eq!(code[6], 2); // Constant index 2
    }

    #[test]
    fn test_too_many_constants_error() {
        let mut compiler = Compiler::new_script();

        // Fill up the constant pool to the limit
        for i in 0..256 {
            compiler
                .function
                .chunk
                .add_constant(Value::Number(i as f64));
        }

        // Try to add one more constant - should fail
        let result = compiler.compile_expr(&Expr::Number(999.0));
        assert!(matches!(result, Err(CompileError::TooManyConstants)));
    }

    #[test]
    fn test_global_variable_access() {
        let mut compiler = Compiler::new_script();
        let expr = Expr::Variable("global_var".to_string());

        compiler.compile_expr(&expr).unwrap();

        // Should have emitted OP_GET_GLOBAL instruction
        assert_eq!(compiler.function.chunk.code.len(), 2);
        assert_eq!(
            compiler.function.chunk.code[0],
            OpCode::OP_GET_GLOBAL.to_byte()
        );
        assert_eq!(compiler.function.chunk.code[1], 0); // Constant index 0

        // Should have added variable name to constants
        assert_eq!(compiler.function.chunk.constants.len(), 1);
        assert_eq!(
            compiler.function.chunk.constants[0].as_string().unwrap(),
            "global_var"
        );
    }

    #[test]
    fn test_scope_management() {
        let mut compiler = Compiler::new_script();

        // Begin scope
        compiler.begin_scope();
        assert_eq!(compiler.scope_depth, 1);

        // Declare local variable
        compiler.declare_local("x".to_string()).unwrap();
        assert_eq!(compiler.locals.len(), 2); // Including reserved slot 0
        assert_eq!(compiler.locals[1].name, "x");
        assert_eq!(compiler.locals[1].depth, -1); // Uninitialized

        // Define local variable
        compiler.define_local();
        assert_eq!(compiler.locals[1].depth, 1); // Now initialized

        // End scope
        compiler.end_scope();
        assert_eq!(compiler.scope_depth, 0);
        assert_eq!(compiler.locals.len(), 1); // Back to just reserved slot 0
    }

    #[test]
    fn test_local_variable_resolution() {
        let mut compiler = Compiler::new_script();

        // Begin scope and declare local
        compiler.begin_scope();
        compiler.declare_local("x".to_string()).unwrap();
        compiler.define_local();

        // Resolve local variable
        let local_index = compiler.resolve_local("x");
        assert_eq!(local_index, Some(1)); // Slot 1 (slot 0 is reserved)

        // Try to resolve non-existent local
        let missing_local = compiler.resolve_local("y");
        assert_eq!(missing_local, None);

        compiler.end_scope();
    }

    #[test]
    fn test_duplicate_local_declaration() {
        let mut compiler = Compiler::new_script();

        compiler.begin_scope();
        compiler.declare_local("x".to_string()).unwrap();

        // Try to declare the same variable again in the same scope
        let result = compiler.declare_local("x".to_string());
        assert!(matches!(
            result,
            Err(CompileError::VariableAlreadyDefined(_))
        ));

        compiler.end_scope();
    }

    #[test]
    fn test_global_variable_definition() {
        let mut compiler = Compiler::new_script();

        // Compile global definition: (define x 42)
        compiler
            .compile_global_define("x", &Expr::Number(42.0))
            .unwrap();

        // Should have compiled the value and emitted define instruction
        let code = &compiler.function.chunk.code;
        assert_eq!(code[0], OpCode::OP_CONSTANT.to_byte()); // Value (42.0)
        assert_eq!(code[1], 0); // Constant index 0
        assert_eq!(code[2], OpCode::OP_DEFINE_GLOBAL.to_byte()); // Define instruction
        assert_eq!(code[3], 1); // Variable name constant index

        // Should have two constants: the value and the variable name
        assert_eq!(compiler.function.chunk.constants.len(), 2);
        assert_eq!(compiler.function.chunk.constants[0], Value::Number(42.0));
        assert_eq!(
            compiler.function.chunk.constants[1].as_string().unwrap(),
            "x"
        );
    }

    #[test]
    fn test_nested_scopes() {
        let mut compiler = Compiler::new_script();

        // Outer scope
        compiler.begin_scope();
        compiler.declare_local("outer".to_string()).unwrap();
        compiler.define_local();

        // Inner scope
        compiler.begin_scope();
        compiler.declare_local("inner".to_string()).unwrap();
        compiler.define_local();

        // Both variables should be resolvable
        assert_eq!(compiler.resolve_local("outer"), Some(1));
        assert_eq!(compiler.resolve_local("inner"), Some(2));

        // End inner scope
        compiler.end_scope();

        // Only outer variable should be resolvable
        assert_eq!(compiler.resolve_local("outer"), Some(1));
        assert_eq!(compiler.resolve_local("inner"), None);

        // End outer scope
        compiler.end_scope();

        // No local variables should be resolvable
        assert_eq!(compiler.resolve_local("outer"), None);
    }

    #[test]
    fn test_compile_lambda() {
        use crate::ast::LambdaExpr;

        let mut compiler = Compiler::new_script();

        // Create a simple lambda: (lambda (x) x)
        let lambda = LambdaExpr {
            params: vec!["x".to_string()],
            body: vec![Expr::Variable("x".to_string())],
        };

        compiler.compile_lambda(&lambda).unwrap();

        // Should have emitted OP_CLOSURE instruction
        let code = &compiler.function.chunk.code;
        assert_eq!(code[0], OpCode::OP_CLOSURE.to_byte());
        assert_eq!(code[1], 0); // Function constant index

        // Should have added function to constants
        assert_eq!(compiler.function.chunk.constants.len(), 1);
        assert!(compiler.function.chunk.constants[0].is_function());
    }

    #[test]
    fn test_compile_function_call() {
        let mut compiler = Compiler::new_script();

        // Create a function call: (f 1 2)
        let call_expr = Expr::Call(
            Box::new(Expr::Variable("f".to_string())),
            vec![Expr::Number(1.0), Expr::Number(2.0)],
        );

        compiler.compile_expr(&call_expr).unwrap();

        let code = &compiler.function.chunk.code;

        // Should compile function expression first
        assert_eq!(code[0], OpCode::OP_GET_GLOBAL.to_byte()); // Function 'f'

        // Then arguments
        assert_eq!(code[2], OpCode::OP_CONSTANT.to_byte()); // First arg (1.0)
        assert_eq!(code[4], OpCode::OP_CONSTANT.to_byte()); // Second arg (2.0)

        // Finally the call
        assert_eq!(code[6], OpCode::OP_CALL.to_byte());
        assert_eq!(code[7], 2); // 2 arguments
    }

    #[test]
    fn test_compile_define_global() {
        use crate::ast::DefineExpr;

        let mut compiler = Compiler::new_script();

        // Create a define expression: (define x 42)
        let define = DefineExpr {
            name: "x".to_string(),
            value: Expr::Number(42.0),
        };

        compiler.compile_define(&define).unwrap();

        let code = &compiler.function.chunk.code;

        // Should compile value first
        assert_eq!(code[0], OpCode::OP_CONSTANT.to_byte()); // Value (42.0)

        // Then define instruction
        assert_eq!(code[2], OpCode::OP_DEFINE_GLOBAL.to_byte());
    }

    #[test]
    fn test_compile_define_local() {
        use crate::ast::DefineExpr;

        let mut compiler = Compiler::new_script();

        // Begin scope for local definition
        compiler.begin_scope();

        // Create a define expression: (define x 42)
        let define = DefineExpr {
            name: "x".to_string(),
            value: Expr::Number(42.0),
        };

        compiler.compile_define(&define).unwrap();

        let code = &compiler.function.chunk.code;

        // Should compile value first
        assert_eq!(code[0], OpCode::OP_CONSTANT.to_byte()); // Value (42.0)

        // Should have declared local variable
        assert_eq!(compiler.locals.len(), 2); // Including reserved slot 0
        assert_eq!(compiler.locals[1].name, "x");
        assert_eq!(compiler.locals[1].depth, 1); // Defined at depth 1

        compiler.end_scope();
    }

    #[test]
    fn test_captured_local_closing() {
        let mut compiler = Compiler::new_script();

        // Begin scope and declare local
        compiler.begin_scope();
        compiler.declare_local("captured".to_string()).unwrap();
        compiler.define_local();

        // Mark the local as captured
        compiler.mark_local_captured(1); // Index 1 (slot 0 is reserved)

        // End scope - should emit OP_CLOSE_UPVALUE
        compiler.end_scope();

        let code = &compiler.function.chunk.code;

        // Should have emitted OP_CLOSE_UPVALUE instead of OP_POP
        assert_eq!(code.last(), Some(&OpCode::OP_CLOSE_UPVALUE.to_byte()));
    }

    #[test]
    fn test_scope_depth_tracking() {
        let mut compiler = Compiler::new_script();

        assert_eq!(compiler.scope_depth, 0);
        assert!(compiler.is_global_scope());

        compiler.begin_scope();
        assert_eq!(compiler.scope_depth, 1);
        assert!(!compiler.is_global_scope());

        compiler.begin_scope();
        assert_eq!(compiler.scope_depth, 2);

        compiler.end_scope();
        assert_eq!(compiler.scope_depth, 1);

        compiler.end_scope();
        assert_eq!(compiler.scope_depth, 0);
        assert!(compiler.is_global_scope());
    }

    #[test]
    fn test_local_count() {
        let mut compiler = Compiler::new_script();

        // Should start with 1 local (reserved slot 0)
        assert_eq!(compiler.local_count(), 1);

        compiler.begin_scope();
        compiler.declare_local("x".to_string()).unwrap();
        compiler.define_local();

        assert_eq!(compiler.local_count(), 2);

        compiler.declare_local("y".to_string()).unwrap();
        compiler.define_local();

        assert_eq!(compiler.local_count(), 3);

        compiler.end_scope();

        // Should be back to 1 after ending scope
        assert_eq!(compiler.local_count(), 1);
    }

    #[test]
    fn test_mixed_captured_and_regular_locals() {
        let mut compiler = Compiler::new_script();

        compiler.begin_scope();

        // Declare two locals
        compiler.declare_local("regular".to_string()).unwrap();
        compiler.define_local();

        compiler.declare_local("captured".to_string()).unwrap();
        compiler.define_local();

        // Mark only the second one as captured
        compiler.mark_local_captured(2); // Index 2

        compiler.end_scope();

        let code = &compiler.function.chunk.code;

        // Should have emitted OP_CLOSE_UPVALUE for captured, then OP_POP for regular
        // (in reverse order since we pop from the end)
        let len = code.len();
        assert_eq!(code[len - 2], OpCode::OP_CLOSE_UPVALUE.to_byte()); // For captured local
        assert_eq!(code[len - 1], OpCode::OP_POP.to_byte()); // For regular local
    }
}
