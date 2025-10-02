use crate::ast::Expr;
use crate::bytecode::OpCode;
use std::collections::HashMap;

/// Configuration for tracing compilation and execution
#[derive(Debug, Clone)]
pub struct TraceConfig {
    /// Enable compilation tracing
    pub compilation: bool,
    /// Enable execution tracing
    pub execution: bool,
    /// Enable stack operation tracing
    pub stack_operations: bool,
    /// Enable upvalue operation tracing
    pub upvalue_operations: bool,
    /// Enable function call tracing
    pub function_calls: bool,
    /// Enable variable access tracing
    pub variable_access: bool,
    /// Only trace these functions (empty means trace all)
    pub filter_functions: Vec<String>,
    /// Exclude these functions from tracing
    pub exclude_functions: Vec<String>,
    /// Only trace these instruction types (empty means trace all)
    pub filter_instructions: Vec<String>,
    /// Exclude these instruction types from tracing
    pub exclude_instructions: Vec<String>,
    /// Only trace these compilation phases (empty means trace all)
    pub filter_phases: Vec<CompilationPhase>,
    /// Exclude these compilation phases from tracing
    pub exclude_phases: Vec<CompilationPhase>,
    /// Limit trace output for deep stacks
    pub max_stack_depth: Option<usize>,
    /// Maximum number of traces to keep in memory
    pub max_trace_count: Option<usize>,
    /// Enable compact trace format (less verbose)
    pub compact_format: bool,
    /// Show line numbers in traces
    pub show_line_numbers: bool,
    /// Show instruction addresses
    pub show_addresses: bool,
}

impl Default for TraceConfig {
    fn default() -> Self {
        TraceConfig {
            compilation: false,
            execution: false,
            stack_operations: true,
            upvalue_operations: true,
            function_calls: true,
            variable_access: true,
            filter_functions: Vec::new(),
            exclude_functions: Vec::new(),
            filter_instructions: Vec::new(),
            exclude_instructions: Vec::new(),
            filter_phases: Vec::new(),
            exclude_phases: Vec::new(),
            max_stack_depth: Some(20),
            max_trace_count: Some(1000),
            compact_format: false,
            show_line_numbers: true,
            show_addresses: false,
        }
    }
}

impl TraceConfig {
    /// Create a new trace config with all tracing enabled
    pub fn all_enabled() -> Self {
        TraceConfig {
            compilation: true,
            execution: true,
            stack_operations: true,
            upvalue_operations: true,
            function_calls: true,
            variable_access: true,
            ..Default::default()
        }
    }

    /// Create a new trace config with only compilation tracing
    pub fn compilation_only() -> Self {
        TraceConfig {
            compilation: true,
            execution: false,
            ..Default::default()
        }
    }

    /// Create a new trace config with only execution tracing
    pub fn execution_only() -> Self {
        TraceConfig {
            compilation: false,
            execution: true,
            ..Default::default()
        }
    }

    /// Create a compact trace config (less verbose output)
    pub fn compact() -> Self {
        TraceConfig {
            compact_format: true,
            show_line_numbers: false,
            show_addresses: false,
            max_stack_depth: Some(10),
            ..Default::default()
        }
    }

    /// Create a debug trace config (very verbose)
    pub fn debug() -> Self {
        TraceConfig {
            compilation: true,
            execution: true,
            stack_operations: true,
            upvalue_operations: true,
            function_calls: true,
            variable_access: true,
            show_line_numbers: true,
            show_addresses: true,
            compact_format: false,
            max_stack_depth: None,
            max_trace_count: None,
            ..Default::default()
        }
    }

    /// Check if we should trace the given function
    pub fn should_trace_function(&self, function_name: &str) -> bool {
        // Check exclusion list first
        if self.exclude_functions.contains(&function_name.to_string()) {
            return false;
        }

        // If filter list is empty, trace all (except excluded)
        if self.filter_functions.is_empty() {
            return true;
        }

        // Otherwise, only trace if in filter list
        self.filter_functions.contains(&function_name.to_string())
    }

    /// Check if we should trace the given instruction
    pub fn should_trace_instruction(&self, instruction_name: &str) -> bool {
        // Check exclusion list first
        if self
            .exclude_instructions
            .contains(&instruction_name.to_string())
        {
            return false;
        }

        // If filter list is empty, trace all (except excluded)
        if self.filter_instructions.is_empty() {
            return true;
        }

        // Otherwise, only trace if in filter list
        self.filter_instructions
            .contains(&instruction_name.to_string())
    }

    /// Check if we should trace the given compilation phase
    pub fn should_trace_phase(&self, phase: &CompilationPhase) -> bool {
        // Check exclusion list first
        if self.exclude_phases.contains(phase) {
            return false;
        }

        // If filter list is empty, trace all (except excluded)
        if self.filter_phases.is_empty() {
            return true;
        }

        // Otherwise, only trace if in filter list
        self.filter_phases.contains(phase)
    }

    /// Add a function to the filter list
    pub fn filter_function(&mut self, function_name: String) {
        if !self.filter_functions.contains(&function_name) {
            self.filter_functions.push(function_name);
        }
    }

    /// Add a function to the exclusion list
    pub fn exclude_function(&mut self, function_name: String) {
        if !self.exclude_functions.contains(&function_name) {
            self.exclude_functions.push(function_name);
        }
    }

    /// Add an instruction to the filter list
    pub fn filter_instruction(&mut self, instruction_name: String) {
        if !self.filter_instructions.contains(&instruction_name) {
            self.filter_instructions.push(instruction_name);
        }
    }

    /// Add an instruction to the exclusion list
    pub fn exclude_instruction(&mut self, instruction_name: String) {
        if !self.exclude_instructions.contains(&instruction_name) {
            self.exclude_instructions.push(instruction_name);
        }
    }

    /// Add a compilation phase to the filter list
    pub fn filter_phase(&mut self, phase: CompilationPhase) {
        if !self.filter_phases.contains(&phase) {
            self.filter_phases.push(phase);
        }
    }

    /// Add a compilation phase to the exclusion list
    pub fn exclude_phase(&mut self, phase: CompilationPhase) {
        if !self.exclude_phases.contains(&phase) {
            self.exclude_phases.push(phase);
        }
    }

    /// Clear all filters (trace everything)
    pub fn clear_filters(&mut self) {
        self.filter_functions.clear();
        self.filter_instructions.clear();
        self.filter_phases.clear();
    }

    /// Clear all exclusions
    pub fn clear_exclusions(&mut self) {
        self.exclude_functions.clear();
        self.exclude_instructions.clear();
        self.exclude_phases.clear();
    }
}

/// Phases of compilation for tracing
#[derive(Debug, Clone, PartialEq)]
pub enum CompilationPhase {
    VariableResolution,
    InstructionGeneration,
    ScopeManagement,
    UpvalueCapture,
    FunctionCompilation,
    ConstantGeneration,
}

/// Information about a compilation step for tracing
#[derive(Debug, Clone)]
pub struct CompilationTrace {
    pub phase: CompilationPhase,
    pub ast_node: String,
    pub generated_instructions: Vec<(OpCode, Vec<u8>)>,
    pub local_variables: Vec<String>,
    pub upvalues: Vec<String>,
    pub scope_depth: usize,
    pub function_name: String,
}

impl CompilationTrace {
    /// Create a new compilation trace
    pub fn new(
        phase: CompilationPhase,
        ast_node: String,
        function_name: String,
        scope_depth: usize,
    ) -> Self {
        CompilationTrace {
            phase,
            ast_node,
            generated_instructions: Vec::new(),
            local_variables: Vec::new(),
            upvalues: Vec::new(),
            scope_depth,
            function_name,
        }
    }

    /// Add an instruction to the trace
    pub fn add_instruction(&mut self, opcode: OpCode, operands: Vec<u8>) {
        self.generated_instructions.push((opcode, operands));
    }

    /// Add a local variable to the trace
    pub fn add_local(&mut self, name: String) {
        self.local_variables.push(name);
    }

    /// Add an upvalue to the trace
    pub fn add_upvalue(&mut self, name: String) {
        self.upvalues.push(name);
    }

    /// Format the trace for output
    pub fn format(&self) -> String {
        self.format_with_config(&TraceConfig::default())
    }

    /// Format the trace for output with custom configuration
    pub fn format_with_config(&self, config: &TraceConfig) -> String {
        let mut output = String::new();

        if config.compact_format {
            // Compact format: single line
            output.push_str(&format!(
                "[COMPILE] {:?} {} {}",
                self.phase, self.function_name, self.ast_node
            ));

            if !self.generated_instructions.is_empty() {
                output.push_str(" -> ");
                let instructions: Vec<String> = self
                    .generated_instructions
                    .iter()
                    .map(|(opcode, operands)| {
                        if operands.is_empty() {
                            opcode.name().to_string()
                        } else {
                            format!(
                                "{}({})",
                                opcode.name(),
                                operands
                                    .iter()
                                    .map(|b| b.to_string())
                                    .collect::<Vec<_>>()
                                    .join(",")
                            )
                        }
                    })
                    .collect();
                output.push_str(&instructions.join(", "));
            }

            output.push('\n');
        } else {
            // Verbose format
            output.push_str(&format!("[COMPILE] Phase: {:?}\n", self.phase));
            output.push_str(&format!("  Function: {}\n", self.function_name));
            output.push_str(&format!("  AST: {}\n", self.ast_node));
            output.push_str(&format!("  Scope Depth: {}\n", self.scope_depth));

            if !self.local_variables.is_empty() {
                output.push_str(&format!(
                    "  Locals: [{}]\n",
                    self.local_variables.join(", ")
                ));
            }

            if !self.upvalues.is_empty() {
                output.push_str(&format!("  Upvalues: [{}]\n", self.upvalues.join(", ")));
            }

            if !self.generated_instructions.is_empty() {
                output.push_str("  Generated:\n");
                for (opcode, operands) in &self.generated_instructions {
                    if operands.is_empty() {
                        output.push_str(&format!("    {}\n", opcode.name()));
                    } else {
                        output.push_str(&format!(
                            "    {} {}\n",
                            opcode.name(),
                            operands
                                .iter()
                                .map(|b| b.to_string())
                                .collect::<Vec<_>>()
                                .join(" ")
                        ));
                    }
                }
            }
        }

        output
    }
}

/// Information about an execution step for tracing
#[derive(Debug, Clone)]
pub struct ExecutionTrace {
    pub instruction: OpCode,
    pub operands: Vec<u8>,
    pub stack_before: Vec<String>,
    pub stack_after: Vec<String>,
    pub current_frame: FrameInfo,
    pub upvalue_states: Vec<UpvalueState>,
}

/// Information about a call frame for tracing
#[derive(Debug, Clone)]
pub struct FrameInfo {
    pub function_name: String,
    pub instruction_pointer: usize,
    pub local_variables: HashMap<String, String>,
    pub stack_base: usize,
}

/// State of an upvalue for tracing
#[derive(Debug, Clone)]
pub struct UpvalueState {
    pub index: usize,
    pub is_closed: bool,
    pub value: String,
    pub stack_location: Option<usize>,
}

impl ExecutionTrace {
    /// Create a new execution trace
    pub fn new(instruction: OpCode, operands: Vec<u8>, current_frame: FrameInfo) -> Self {
        ExecutionTrace {
            instruction,
            operands,
            stack_before: Vec::new(),
            stack_after: Vec::new(),
            current_frame,
            upvalue_states: Vec::new(),
        }
    }

    /// Set the stack state before instruction execution
    pub fn set_stack_before(&mut self, stack: Vec<String>) {
        self.stack_before = stack;
    }

    /// Set the stack state after instruction execution
    pub fn set_stack_after(&mut self, stack: Vec<String>) {
        self.stack_after = stack;
    }

    /// Add an upvalue state to the trace
    pub fn add_upvalue_state(&mut self, state: UpvalueState) {
        self.upvalue_states.push(state);
    }

    /// Format the trace for output
    pub fn format(&self) -> String {
        self.format_with_config(&TraceConfig::default())
    }

    /// Format the trace for output with custom configuration
    pub fn format_with_config(&self, config: &TraceConfig) -> String {
        let mut output = String::new();

        if config.compact_format {
            // Compact format: single line
            if self.operands.is_empty() {
                output.push_str(&format!("[EXEC] {} ", self.instruction.name()));
            } else {
                output.push_str(&format!(
                    "[EXEC] {}({}) ",
                    self.instruction.name(),
                    self.operands
                        .iter()
                        .map(|b| b.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                ));
            }

            if config.show_addresses {
                output.push_str(&format!("@{} ", self.current_frame.instruction_pointer));
            }

            output.push_str(&format!(
                "{} [{}] -> [{}]",
                self.current_frame.function_name,
                self.stack_before.join(","),
                self.stack_after.join(",")
            ));

            output.push('\n');
        } else {
            // Verbose format
            if self.operands.is_empty() {
                output.push_str(&format!("[EXEC] {}\n", self.instruction.name()));
            } else {
                output.push_str(&format!(
                    "[EXEC] {} {}\n",
                    self.instruction.name(),
                    self.operands
                        .iter()
                        .map(|b| b.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                ));
            }

            // Format frame info
            if config.show_addresses {
                output.push_str(&format!(
                    "  Frame: {}@{} (base={})\n",
                    self.current_frame.function_name,
                    self.current_frame.instruction_pointer,
                    self.current_frame.stack_base
                ));
            } else {
                output.push_str(&format!("  Frame: {}\n", self.current_frame.function_name));
            }

            // Format stack states (with depth limit)
            let stack_limit = config.max_stack_depth.unwrap_or(usize::MAX);
            let before_display = if self.stack_before.len() > stack_limit {
                format!(
                    "{}...(+{})",
                    self.stack_before[..stack_limit].join(", "),
                    self.stack_before.len() - stack_limit
                )
            } else {
                self.stack_before.join(", ")
            };

            let after_display = if self.stack_after.len() > stack_limit {
                format!(
                    "{}...(+{})",
                    self.stack_after[..stack_limit].join(", "),
                    self.stack_after.len() - stack_limit
                )
            } else {
                self.stack_after.join(", ")
            };

            output.push_str(&format!("  Stack Before: [{}]\n", before_display));
            output.push_str(&format!("  Stack After:  [{}]\n", after_display));

            // Format local variables if any and if enabled
            if config.variable_access && !self.current_frame.local_variables.is_empty() {
                output.push_str("  Locals:\n");
                for (name, value) in &self.current_frame.local_variables {
                    output.push_str(&format!("    {}: {}\n", name, value));
                }
            }

            // Format upvalue states if any and if enabled
            if config.upvalue_operations && !self.upvalue_states.is_empty() {
                output.push_str("  Upvalues:\n");
                for state in &self.upvalue_states {
                    if state.is_closed {
                        output.push_str(&format!("    {}: closed={}\n", state.index, state.value));
                    } else if let Some(location) = state.stack_location {
                        output.push_str(&format!(
                            "    {}: open@stack[{}]={}\n",
                            state.index, location, state.value
                        ));
                    } else {
                        output.push_str(&format!("    {}: {}\n", state.index, state.value));
                    }
                }
            }
        }

        output
    }
}

/// Statistics about tracing
#[derive(Debug, Clone)]
pub struct TraceStats {
    pub compilation_trace_count: usize,
    pub execution_trace_count: usize,
    pub total_traces: usize,
}

/// Tracer for compilation and execution
#[derive(Clone)]
pub struct Tracer {
    pub config: TraceConfig,
    pub compilation_traces: Vec<CompilationTrace>,
    pub execution_traces: Vec<ExecutionTrace>,
}

impl Tracer {
    /// Create a new tracer with the given configuration
    pub fn new(config: TraceConfig) -> Self {
        Tracer {
            config,
            compilation_traces: Vec::new(),
            execution_traces: Vec::new(),
        }
    }

    /// Create a tracer with default configuration
    pub fn default() -> Self {
        Tracer::new(TraceConfig::default())
    }

    /// Enable compilation tracing
    pub fn enable_compilation(&mut self) {
        self.config.compilation = true;
    }

    /// Disable compilation tracing
    pub fn disable_compilation(&mut self) {
        self.config.compilation = false;
    }

    /// Enable execution tracing
    pub fn enable_execution(&mut self) {
        self.config.execution = true;
    }

    /// Disable execution tracing
    pub fn disable_execution(&mut self) {
        self.config.execution = false;
    }

    /// Add a compilation trace if compilation tracing is enabled
    pub fn trace_compilation(&mut self, trace: CompilationTrace) {
        if self.config.compilation
            && self.config.should_trace_function(&trace.function_name)
            && self.config.should_trace_phase(&trace.phase)
        {
            print!("{}", trace.format_with_config(&self.config));

            // Manage trace count limit
            if let Some(max_count) = self.config.max_trace_count {
                if self.compilation_traces.len() >= max_count {
                    self.compilation_traces.remove(0); // Remove oldest trace
                }
            }

            self.compilation_traces.push(trace);
        }
    }

    /// Add an execution trace if execution tracing is enabled
    pub fn trace_execution(&mut self, trace: ExecutionTrace) {
        if self.config.execution
            && self
                .config
                .should_trace_function(&trace.current_frame.function_name)
            && self
                .config
                .should_trace_instruction(trace.instruction.name())
        {
            print!("{}", trace.format_with_config(&self.config));

            // Manage trace count limit
            if let Some(max_count) = self.config.max_trace_count {
                if self.execution_traces.len() >= max_count {
                    self.execution_traces.remove(0); // Remove oldest trace
                }
            }

            self.execution_traces.push(trace);
        }
    }

    /// Clear all stored traces
    pub fn clear_traces(&mut self) {
        self.compilation_traces.clear();
        self.execution_traces.clear();
    }

    /// Get all compilation traces
    pub fn get_compilation_traces(&self) -> &[CompilationTrace] {
        &self.compilation_traces
    }

    /// Get all execution traces
    pub fn get_execution_traces(&self) -> &[ExecutionTrace] {
        &self.execution_traces
    }

    /// Set function filter for tracing
    pub fn set_function_filter(&mut self, functions: Vec<String>) {
        self.config.filter_functions = functions;
    }

    /// Clear function filter (trace all functions)
    pub fn clear_function_filter(&mut self) {
        self.config.filter_functions.clear();
    }

    /// Add a function to trace
    pub fn add_function_filter(&mut self, function_name: String) {
        self.config.filter_function(function_name);
    }

    /// Exclude a function from tracing
    pub fn exclude_function(&mut self, function_name: String) {
        self.config.exclude_function(function_name);
    }

    /// Set instruction filter for tracing
    pub fn set_instruction_filter(&mut self, instructions: Vec<String>) {
        self.config.filter_instructions = instructions;
    }

    /// Add an instruction to trace
    pub fn add_instruction_filter(&mut self, instruction_name: String) {
        self.config.filter_instruction(instruction_name);
    }

    /// Exclude an instruction from tracing
    pub fn exclude_instruction(&mut self, instruction_name: String) {
        self.config.exclude_instruction(instruction_name);
    }

    /// Set compilation phase filter for tracing
    pub fn set_phase_filter(&mut self, phases: Vec<CompilationPhase>) {
        self.config.filter_phases = phases;
    }

    /// Add a compilation phase to trace
    pub fn add_phase_filter(&mut self, phase: CompilationPhase) {
        self.config.filter_phase(phase);
    }

    /// Exclude a compilation phase from tracing
    pub fn exclude_phase(&mut self, phase: CompilationPhase) {
        self.config.exclude_phase(phase);
    }

    /// Enable compact format
    pub fn enable_compact_format(&mut self) {
        self.config.compact_format = true;
    }

    /// Disable compact format
    pub fn disable_compact_format(&mut self) {
        self.config.compact_format = false;
    }

    /// Set maximum stack depth for display
    pub fn set_max_stack_depth(&mut self, depth: Option<usize>) {
        self.config.max_stack_depth = depth;
    }

    /// Set maximum trace count to keep in memory
    pub fn set_max_trace_count(&mut self, count: Option<usize>) {
        self.config.max_trace_count = count;
    }

    /// Enable/disable line numbers in traces
    pub fn set_show_line_numbers(&mut self, show: bool) {
        self.config.show_line_numbers = show;
    }

    /// Enable/disable addresses in traces
    pub fn set_show_addresses(&mut self, show: bool) {
        self.config.show_addresses = show;
    }

    /// Get trace statistics
    pub fn get_stats(&self) -> TraceStats {
        TraceStats {
            compilation_trace_count: self.compilation_traces.len(),
            execution_trace_count: self.execution_traces.len(),
            total_traces: self.compilation_traces.len() + self.execution_traces.len(),
        }
    }

    /// Export traces to a formatted string
    pub fn export_traces(&self) -> String {
        let mut output = String::new();

        output.push_str("=== COMPILATION TRACES ===\n");
        for trace in &self.compilation_traces {
            output.push_str(&trace.format_with_config(&self.config));
        }

        output.push_str("\n=== EXECUTION TRACES ===\n");
        for trace in &self.execution_traces {
            output.push_str(&trace.format_with_config(&self.config));
        }

        output
    }
}

/// Helper function to format an AST expression for tracing
pub fn format_ast_expr(expr: &Expr) -> String {
    match expr {
        Expr::Number(n) => format!("Number({})", n),
        Expr::String(s) => format!("String(\"{}\")", s),
        Expr::Character(c) => format!("Character(#\\{})", c),
        Expr::Boolean(b) => format!("Boolean({})", if *b { "#t" } else { "#f" }),
        Expr::Variable(name) => format!("Variable(\"{}\")", name),
        Expr::Lambda(lambda) => format!("Lambda(params=[{}], body=...)", lambda.params.join(", ")),
        Expr::Call(func, args) => {
            format!("Call(func={}, args={})", format_ast_expr(func), args.len())
        }
        Expr::Define(define) => format!("Define(name=\"{}\", value=...)", define.name),
        Expr::If(_) => format!("If(condition=..., then=..., else=...)"),
        Expr::Let(let_expr) => format!("Let(bindings={}, body=...)", let_expr.bindings.len()),
        Expr::LetStar(let_star) => {
            format!("LetStar(bindings={}, body=...)", let_star.bindings.len())
        }
        Expr::LetLoop(let_loop) => format!(
            "LetLoop(name=\"{}\", bindings={}, body=...)",
            let_loop.name,
            let_loop.bindings.len()
        ),
        Expr::LetValues(let_values) => format!(
            "LetValues(bindings={}, body=...)",
            let_values.bindings.len()
        ),
        Expr::CallWithValues(_) => {
            format!("CallWithValues(producer=..., consumer=...)")
        }
        Expr::Import(_) => format!("Import(module=...)"),
        Expr::Cond(cond_expr) => format!("Cond(clauses={})", cond_expr.clauses.len()),
        Expr::Begin(exprs) => format!("Begin(exprs={})", exprs.len()),
        Expr::Set(set_expr) => format!("Set(variable=\"{}\", value=...)", set_expr.variable),
        Expr::Quote(quoted) => format!("Quote({})", format_ast_expr(quoted)),
        Expr::QuasiQuote(quasi) => format!("QuasiQuote({})", format_ast_expr(quasi)),
        Expr::UnQuote(unquoted) => format!("UnQuote({})", format_ast_expr(unquoted)),
        Expr::UnQuoteSplicing(spliced) => format!("UnQuoteSplicing({})", format_ast_expr(spliced)),
        Expr::List(exprs) => format!("List({})", exprs.len()),
        Expr::Vector(exprs) => format!("Vector({})", exprs.len()),
    }
}
