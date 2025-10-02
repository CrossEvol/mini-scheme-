use crate::bytecode::{Chunk, OpCode};
use crate::object::{Closure, Function, Object, Value};
use crate::trace::{ExecutionTrace, FrameInfo, TraceConfig, Tracer, UpvalueState};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Runtime error types for the virtual machine
#[derive(Debug, Clone)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError { expected: String, got: String },
    ArityMismatch { expected: usize, got: usize },
    StackOverflow,
    StackUnderflow,
    InvalidOperation(String),
    DivisionByZero,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            RuntimeError::TypeError { expected, got } => {
                write!(f, "Type error: expected {}, got {}", expected, got)
            }
            RuntimeError::ArityMismatch { expected, got } => write!(
                f,
                "Arity mismatch: expected {} arguments, got {}",
                expected, got
            ),
            RuntimeError::StackOverflow => write!(f, "Stack overflow"),
            RuntimeError::StackUnderflow => write!(f, "Stack underflow"),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}

impl std::error::Error for RuntimeError {}

/// Call frame for function calls
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub closure: Rc<Closure>,
    pub ip: usize,    // Instruction pointer
    pub slots: usize, // Base of stack frame for local variables
}

impl CallFrame {
    /// Create a new call frame
    pub fn new(closure: Rc<Closure>, slots: usize) -> Self {
        CallFrame {
            closure,
            ip: 0,
            slots,
        }
    }

    /// Get the current function being executed in this frame
    pub fn function(&self) -> &Function {
        &self.closure.function
    }

    /// Get the current chunk being executed
    pub fn chunk(&self) -> &Chunk {
        &self.closure.function.chunk
    }

    /// Check if this frame can accept the given number of arguments
    pub fn check_arity(&self, arg_count: usize) -> bool {
        self.closure.function.arity == arg_count
    }
}

/// Stack-based virtual machine for executing MiniScheme bytecode
pub struct VM {
    /// Value stack for computation
    pub stack: Vec<Value>,
    /// Current top of stack (points to next free slot)
    pub stack_top: usize,
    /// Call frame stack
    pub frames: Vec<CallFrame>,
    /// Current number of active frames
    pub frame_count: usize,
    /// Global variable table
    pub globals: HashMap<String, Value>,
    /// Open upvalues (sorted by stack address)
    pub open_upvalues: Vec<Rc<RefCell<crate::object::Upvalue>>>,
    /// Enable/disable execution tracing
    pub trace_execution: bool,
    /// Tracer for execution tracing
    pub tracer: Option<Tracer>,
}

/// Maximum stack size to prevent stack overflow
const STACK_MAX: usize = 256;
/// Maximum number of call frames
const FRAMES_MAX: usize = 64;

impl VM {
    /// Create a new virtual machine
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(STACK_MAX),
            stack_top: 0,
            frames: Vec::with_capacity(FRAMES_MAX),
            frame_count: 0,
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
            trace_execution: false,
            tracer: None,
        }
    }

    /// Reset the VM to initial state
    pub fn reset(&mut self) {
        self.stack.clear();
        self.stack_top = 0;
        self.frames.clear();
        self.frame_count = 0;
        self.globals.clear();
        self.open_upvalues.clear();
        if let Some(tracer) = &mut self.tracer {
            tracer.clear_traces();
        }
    }

    /// Enable execution tracing
    pub fn enable_trace(&mut self) {
        self.trace_execution = true;
        if self.tracer.is_none() {
            self.tracer = Some(Tracer::new(TraceConfig::execution_only()));
        }
        if let Some(tracer) = &mut self.tracer {
            tracer.enable_execution();
        }
    }

    /// Disable execution tracing
    pub fn disable_trace(&mut self) {
        self.trace_execution = false;
        if let Some(tracer) = &mut self.tracer {
            tracer.disable_execution();
        }
    }

    /// Set a custom tracer
    pub fn set_tracer(&mut self, tracer: Tracer) {
        self.tracer = Some(tracer);
        self.trace_execution = self.tracer.as_ref().map_or(false, |t| t.config.execution);
    }

    /// Get a reference to the tracer
    pub fn tracer(&self) -> Option<&Tracer> {
        self.tracer.as_ref()
    }

    /// Get a mutable reference to the tracer
    pub fn tracer_mut(&mut self) -> Option<&mut Tracer> {
        self.tracer.as_mut()
    }

    // Stack management methods

    /// Push a value onto the stack
    pub fn push(&mut self, value: Value) -> Result<(), RuntimeError> {
        if self.stack_top >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }

        if self.stack.len() <= self.stack_top {
            self.stack.resize(self.stack_top + 1, Value::Nil);
        }

        self.stack[self.stack_top] = value;
        self.stack_top += 1;
        Ok(())
    }

    /// Pop a value from the stack
    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        if self.stack_top == 0 {
            return Err(RuntimeError::StackUnderflow);
        }

        self.stack_top -= 1;
        Ok(self.stack[self.stack_top].clone())
    }

    /// Peek at the top value without popping
    pub fn peek(&self, distance: usize) -> Result<&Value, RuntimeError> {
        if distance >= self.stack_top {
            return Err(RuntimeError::StackUnderflow);
        }

        Ok(&self.stack[self.stack_top - 1 - distance])
    }

    /// Get the current stack depth
    pub fn stack_depth(&self) -> usize {
        self.stack_top
    }

    /// Print the current stack state (for debugging)
    pub fn print_stack(&self) {
        print!("          ");
        for i in 0..self.stack_top {
            print!("[ ");
            self.print_value(&self.stack[i]);
            print!(" ]");
        }
        println!();
    }

    /// Print a value (for debugging)
    fn print_value(&self, value: &Value) {
        match value {
            Value::Number(n) => print!("{}", n),
            Value::Boolean(b) => print!("{}", if *b { "#t" } else { "#f" }),
            Value::Nil => print!("nil"),
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        Object::String(s) => print!("\"{}\"", s),
                        Object::Character(c) => print!("#\\{}", c),
                        Object::Symbol(s) => print!("{}", s),
                        Object::Function(f) => print!("<fn {}>", f.name),
                        Object::Closure(_) => print!("<closure>"),
                        Object::Cons(_) => print!("<cons>"),
                        Object::Vector(_) => print!("<vector>"),
                        Object::Hashtable(_) => print!("<hashtable>"),
                        Object::Upvalue(_) => print!("<upvalue>"),
                    }
                } else {
                    print!("<object>");
                }
            }
        }
    }

    // Global variable management

    /// Define a global variable
    pub fn define_global(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }

    /// Get a global variable
    pub fn get_global(&self, name: &str) -> Result<Value, RuntimeError> {
        self.globals
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedVariable(name.to_string()))
    }

    /// Set a global variable (must already exist)
    pub fn set_global(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        if self.globals.contains_key(name) {
            self.globals.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedVariable(name.to_string()))
        }
    }

    // Instruction execution methods

    /// Execute a chunk of bytecode
    pub fn interpret(&mut self, chunk: &Chunk) -> Result<Value, RuntimeError> {
        // Create a main function to hold the chunk
        let main_function = Function {
            name: "main".to_string(),
            arity: 0,
            chunk: chunk.clone(),
            upvalue_count: 0,
        };

        // Create a closure for the main function
        let main_closure = crate::object::Closure::new(Rc::new(main_function));
        let main_closure_rc = Rc::new(main_closure);

        // Push the main function onto the stack (slot 0)
        self.push(Value::closure((*main_closure_rc).clone()))?;

        // Create the initial call frame with slots starting at 0
        self.push_frame(main_closure_rc, 0)?;

        // Run the main execution loop
        self.run()
    }

    /// Main execution loop
    fn run(&mut self) -> Result<Value, RuntimeError> {
        loop {
            // Prepare execution trace if tracing is enabled
            let mut execution_trace = if self.trace_execution {
                self.print_stack();
                let frame = &self.frames[self.frame_count - 1];
                let chunk = &frame.closure.function.chunk;
                let disassembler = crate::bytecode::Disassembler::new();
                disassembler.disassemble_instruction(chunk, frame.ip);

                // Create execution trace
                Some(self.create_execution_trace_before())
            } else {
                None
            };

            let instruction = self.read_byte()?;
            let opcode = OpCode::from_byte(instruction).ok_or_else(|| {
                RuntimeError::InvalidOperation(format!("Unknown opcode: {}", instruction))
            })?;

            // Record instruction and operands in trace
            if let Some(ref mut trace) = execution_trace {
                trace.instruction = opcode;
                // We'll read operands as we execute the instruction
            }

            let result = match opcode {
                OpCode::OP_CONSTANT => {
                    let constant_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(constant_index as u8);
                    }
                    let constant = self.read_constant(constant_index)?;
                    self.push(constant)?;
                    Ok(())
                }

                OpCode::OP_NIL => {
                    self.push(Value::Nil)?;
                    Ok(())
                }

                OpCode::OP_TRUE => {
                    self.push(Value::Boolean(true))?;
                    Ok(())
                }

                OpCode::OP_FALSE => {
                    self.push(Value::Boolean(false))?;
                    Ok(())
                }

                OpCode::OP_POP => {
                    self.pop()?;
                    Ok(())
                }

                OpCode::OP_GET_LOCAL => {
                    let slot = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(slot as u8);
                    }
                    let value = self.get_local(slot)?;
                    self.push(value)?;
                    Ok(())
                }

                OpCode::OP_SET_LOCAL => {
                    let slot = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(slot as u8);
                    }
                    let value = self.peek(0)?.clone(); // Don't pop yet in case of error
                    self.set_local(slot, value)?;
                    // Value stays on stack for assignment expressions
                    Ok(())
                }

                OpCode::OP_GET_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(constant_index as u8);
                    }
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.get_global(&name)?;
                    self.push(value)?;
                    Ok(())
                }

                OpCode::OP_DEFINE_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(constant_index as u8);
                    }
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.pop()?;
                    self.define_global(name, value);
                    Ok(())
                }

                OpCode::OP_SET_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(constant_index as u8);
                    }
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.peek(0)?.clone(); // Don't pop yet in case of error
                    self.set_global(&name, value)?;
                    // Only pop after successful assignment
                    self.pop()?;
                    Ok(())
                }

                OpCode::OP_GET_UPVALUE => {
                    let upvalue_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(upvalue_index as u8);
                    }
                    let value = self.get_upvalue(upvalue_index)?;
                    self.push(value)?;
                    Ok(())
                }

                OpCode::OP_SET_UPVALUE => {
                    let upvalue_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(upvalue_index as u8);
                    }
                    let value = self.peek(0)?.clone(); // Don't pop yet in case of error
                    self.set_upvalue(upvalue_index, value)?;
                    // Value stays on stack for assignment expressions
                    Ok(())
                }

                OpCode::OP_EQUAL => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Boolean(a.equal(&b)))?;
                    Ok(())
                }

                OpCode::OP_GREATER => {
                    self.binary_comparison(|a, b| a > b)?;
                    Ok(())
                }

                OpCode::OP_LESS => {
                    self.binary_comparison(|a, b| a < b)?;
                    Ok(())
                }

                OpCode::OP_ADD => {
                    self.binary_arithmetic(|a, b| a + b)?;
                    Ok(())
                }

                OpCode::OP_SUBTRACT => {
                    self.binary_arithmetic(|a, b| a - b)?;
                    Ok(())
                }

                OpCode::OP_MULTIPLY => {
                    self.binary_arithmetic(|a, b| a * b)?;
                    Ok(())
                }

                OpCode::OP_DIVIDE => {
                    self.binary_divide()?;
                    Ok(())
                }

                OpCode::OP_NOT => {
                    let value = self.pop()?;
                    self.push(Value::Boolean(value.is_falsy()))?;
                    Ok(())
                }

                OpCode::OP_NEGATE => {
                    let value = self.pop()?;
                    match value {
                        Value::Number(n) => self.push(Value::Number(-n))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "number".to_string(),
                                got: self.type_name(&value),
                            });
                        }
                    }
                    Ok(())
                }

                OpCode::OP_CALL => {
                    let arg_count = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(arg_count as u8);
                    }
                    self.call_value(arg_count)?;
                    Ok(())
                }

                OpCode::OP_CLOSE_UPVALUE => {
                    let slot = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(slot as u8);
                    }
                    let current_frame = self.current_frame().ok_or_else(|| {
                        RuntimeError::InvalidOperation("No active call frame".to_string())
                    })?;
                    let absolute_slot = current_frame.slots + slot;
                    self.close_upvalues(absolute_slot);
                    Ok(())
                }

                OpCode::OP_CLOSURE => {
                    let constant_index = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(constant_index as u8);
                    }
                    let function_value = self.read_constant(constant_index)?;

                    // Extract the function from the constant
                    let function = match &function_value {
                        Value::Object(obj) => {
                            if let Ok(obj_ref) = obj.try_borrow() {
                                if let Object::Function(func) = &*obj_ref {
                                    Rc::new(func.clone())
                                } else {
                                    return Err(RuntimeError::TypeError {
                                        expected: "function".to_string(),
                                        got: self.type_name(&function_value),
                                    });
                                }
                            } else {
                                return Err(RuntimeError::InvalidOperation(
                                    "Cannot borrow function object".to_string(),
                                ));
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "function".to_string(),
                                got: self.type_name(&function_value),
                            });
                        }
                    };

                    // Create the closure
                    let mut closure = Closure::new(function.clone());

                    // Read upvalue metadata and capture upvalues
                    for _ in 0..function.upvalue_count {
                        let is_local = self.read_byte()? != 0;
                        let index = self.read_byte()? as usize;

                        if let Some(ref mut trace) = execution_trace {
                            trace.operands.push(if is_local { 1 } else { 0 });
                            trace.operands.push(index as u8);
                        }

                        let upvalue = if is_local {
                            // Capture a local variable from the current frame
                            let current_frame = self.current_frame().ok_or_else(|| {
                                RuntimeError::InvalidOperation("No active call frame".to_string())
                            })?;
                            let stack_slot = current_frame.slots + index;
                            self.capture_upvalue(stack_slot)
                        } else {
                            // Capture an upvalue from the current closure
                            let current_frame = self.current_frame().ok_or_else(|| {
                                RuntimeError::InvalidOperation("No active call frame".to_string())
                            })?;
                            if index < current_frame.closure.upvalues.len() {
                                current_frame.closure.upvalues[index].clone()
                            } else {
                                return Err(RuntimeError::InvalidOperation(format!(
                                    "Upvalue index {} out of bounds",
                                    index
                                )));
                            }
                        };

                        closure.upvalues.push(upvalue);
                    }

                    // Push the closure onto the stack
                    let closure_value = Value::closure(closure);
                    self.push(closure_value)?;
                    Ok(())
                }

                OpCode::OP_RETURN => {
                    let result = self.pop()?;
                    let frame = self.pop_frame()?;

                    // Close any upvalues that are leaving scope
                    self.close_upvalues(frame.slots);

                    if self.frame_count == 0 {
                        // Complete tracing before returning
                        if let Some(trace) = execution_trace {
                            self.complete_execution_trace(trace);
                        }
                        // End of program
                        return Ok(result);
                    }

                    // Restore stack to frame boundary and push result
                    self.stack_top = frame.slots;
                    self.push(result)?;
                    Ok(())
                }

                OpCode::OP_CONS => {
                    let cdr = self.pop()?;
                    let car = self.pop()?;
                    let cons_value = Value::cons(car, cdr);
                    self.push(cons_value)?;
                    Ok(())
                }

                OpCode::OP_CAR => {
                    let value = self.pop()?;
                    match &value {
                        Value::Object(obj) => {
                            if let Ok(obj_ref) = obj.try_borrow() {
                                if let Object::Cons(cons) = &*obj_ref {
                                    self.push(cons.car.clone())?;
                                    Ok(())
                                } else {
                                    Err(RuntimeError::TypeError {
                                        expected: "cons cell".to_string(),
                                        got: self.type_name(&value),
                                    })
                                }
                            } else {
                                Err(RuntimeError::InvalidOperation(
                                    "Cannot borrow cons cell".to_string(),
                                ))
                            }
                        }
                        _ => Err(RuntimeError::TypeError {
                            expected: "cons cell".to_string(),
                            got: self.type_name(&value),
                        }),
                    }
                }

                OpCode::OP_CDR => {
                    let value = self.pop()?;
                    match &value {
                        Value::Object(obj) => {
                            if let Ok(obj_ref) = obj.try_borrow() {
                                if let Object::Cons(cons) = &*obj_ref {
                                    self.push(cons.cdr.clone())?;
                                    Ok(())
                                } else {
                                    Err(RuntimeError::TypeError {
                                        expected: "cons cell".to_string(),
                                        got: self.type_name(&value),
                                    })
                                }
                            } else {
                                Err(RuntimeError::InvalidOperation(
                                    "Cannot borrow cons cell".to_string(),
                                ))
                            }
                        }
                        _ => Err(RuntimeError::TypeError {
                            expected: "cons cell".to_string(),
                            got: self.type_name(&value),
                        }),
                    }
                }

                OpCode::OP_VECTOR => {
                    let element_count = self.read_byte()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push(element_count as u8);
                    }

                    // Check if we have enough elements on the stack
                    if self.stack_top < element_count {
                        return Err(RuntimeError::StackUnderflow);
                    }

                    // Pop elements from stack in reverse order (last pushed is first element)
                    let mut elements = Vec::with_capacity(element_count);
                    for _ in 0..element_count {
                        elements.push(self.pop()?);
                    }

                    // Reverse to get correct order (first pushed should be first element)
                    elements.reverse();

                    let vector_value = Value::vector(elements);
                    self.push(vector_value)?;
                    Ok(())
                }

                OpCode::OP_MAKE_HASHTABLE => {
                    // Create an empty hashtable
                    let hashtable = std::collections::HashMap::new();
                    let hashtable_value = Value::hashtable(hashtable);
                    self.push(hashtable_value)?;
                    Ok(())
                }

                OpCode::OP_JUMP => {
                    let jump_distance = self.read_short()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push((jump_distance >> 8) as u8);
                        trace.operands.push(jump_distance as u8);
                    }
                    let frame = &mut self.frames[self.frame_count - 1];
                    frame.ip += jump_distance;
                    Ok(())
                }

                OpCode::OP_JUMP_IF_FALSE => {
                    let jump_distance = self.read_short()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push((jump_distance >> 8) as u8);
                        trace.operands.push(jump_distance as u8);
                    }
                    let condition = self.peek(0)?;
                    if condition.is_falsy() {
                        let frame = &mut self.frames[self.frame_count - 1];
                        frame.ip += jump_distance;
                    }
                    Ok(())
                }

                OpCode::OP_LOOP => {
                    let jump_distance = self.read_short()? as usize;
                    if let Some(ref mut trace) = execution_trace {
                        trace.operands.push((jump_distance >> 8) as u8);
                        trace.operands.push(jump_distance as u8);
                    }
                    let frame = &mut self.frames[self.frame_count - 1];
                    if frame.ip >= jump_distance {
                        frame.ip -= jump_distance;
                    } else {
                        return Err(RuntimeError::InvalidOperation(
                            "Invalid loop jump".to_string(),
                        ));
                    }
                    Ok(())
                }

                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Unimplemented opcode: {:?}",
                    opcode
                ))),
            };

            // Complete execution trace if tracing is enabled
            if let Some(trace) = execution_trace {
                self.complete_execution_trace(trace);
            }

            // Handle any execution errors
            result?;
        }
    }

    // Helper methods for instruction execution

    /// Read a byte from the current instruction stream
    fn read_byte(&mut self) -> Result<u8, RuntimeError> {
        let frame = &mut self.frames[self.frame_count - 1];
        let chunk = &frame.closure.function.chunk;

        if frame.ip >= chunk.code.len() {
            return Err(RuntimeError::InvalidOperation(
                "Instruction pointer out of bounds".to_string(),
            ));
        }

        let byte = chunk.code[frame.ip];
        frame.ip += 1;
        Ok(byte)
    }

    /// Read a 2-byte short from the current instruction stream (big-endian)
    fn read_short(&mut self) -> Result<u16, RuntimeError> {
        let high = self.read_byte()? as u16;
        let low = self.read_byte()? as u16;
        Ok((high << 8) | low)
    }

    /// Read a constant from the constant pool
    fn read_constant(&self, index: usize) -> Result<Value, RuntimeError> {
        let frame = &self.frames[self.frame_count - 1];
        let chunk = &frame.closure.function.chunk;

        chunk.constants.get(index).cloned().ok_or_else(|| {
            RuntimeError::InvalidOperation(format!("Invalid constant index: {}", index))
        })
    }

    /// Read a string constant from the constant pool
    fn read_string_constant(&self, index: usize) -> Result<String, RuntimeError> {
        let constant = self.read_constant(index)?;
        match &constant {
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    if let Object::String(s) = &*obj_ref {
                        Ok(s.clone())
                    } else {
                        Err(RuntimeError::TypeError {
                            expected: "string".to_string(),
                            got: self.type_name(&constant),
                        })
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot borrow object".to_string(),
                    ))
                }
            }
            _ => Err(RuntimeError::TypeError {
                expected: "string".to_string(),
                got: self.type_name(&constant),
            }),
        }
    }

    /// Perform binary arithmetic operation
    fn binary_arithmetic<F>(&mut self, op: F) -> Result<(), RuntimeError>
    where
        F: Fn(f64, f64) -> f64,
    {
        let b = self.pop()?;
        let a = self.pop()?;

        match (a, b) {
            (Value::Number(a_num), Value::Number(b_num)) => {
                let result = op(a_num, b_num);
                self.push(Value::Number(result))?;
                Ok(())
            }
            (a_val, b_val) => Err(RuntimeError::TypeError {
                expected: "numbers".to_string(),
                got: format!("{} and {}", self.type_name(&a_val), self.type_name(&b_val)),
            }),
        }
    }

    /// Perform binary comparison operation
    fn binary_comparison<F>(&mut self, op: F) -> Result<(), RuntimeError>
    where
        F: Fn(f64, f64) -> bool,
    {
        let b = self.pop()?;
        let a = self.pop()?;

        match (a, b) {
            (Value::Number(a_num), Value::Number(b_num)) => {
                let result = op(a_num, b_num);
                self.push(Value::Boolean(result))?;
                Ok(())
            }
            (a_val, b_val) => Err(RuntimeError::TypeError {
                expected: "numbers".to_string(),
                got: format!("{} and {}", self.type_name(&a_val), self.type_name(&b_val)),
            }),
        }
    }

    /// Perform division with zero check
    fn binary_divide(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop()?;
        let a = self.pop()?;

        match (a, b) {
            (Value::Number(a_num), Value::Number(b_num)) => {
                if b_num == 0.0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                let result = a_num / b_num;
                self.push(Value::Number(result))?;
                Ok(())
            }
            (a_val, b_val) => Err(RuntimeError::TypeError {
                expected: "numbers".to_string(),
                got: format!("{} and {}", self.type_name(&a_val), self.type_name(&b_val)),
            }),
        }
    }

    // Call frame management methods

    /// Get the current call frame
    pub fn current_frame(&self) -> Option<&CallFrame> {
        if self.frame_count > 0 {
            Some(&self.frames[self.frame_count - 1])
        } else {
            None
        }
    }

    /// Get the current call frame mutably
    pub fn current_frame_mut(&mut self) -> Option<&mut CallFrame> {
        if self.frame_count > 0 {
            Some(&mut self.frames[self.frame_count - 1])
        } else {
            None
        }
    }

    /// Push a new call frame onto the frame stack
    pub fn push_frame(&mut self, closure: Rc<Closure>, slots: usize) -> Result<(), RuntimeError> {
        if self.frame_count >= FRAMES_MAX {
            return Err(RuntimeError::StackOverflow);
        }

        let frame = CallFrame::new(closure, slots);
        if self.frames.len() <= self.frame_count {
            self.frames.resize(self.frame_count + 1, frame.clone());
        }
        self.frames[self.frame_count] = frame;
        self.frame_count += 1;
        Ok(())
    }

    /// Pop the current call frame from the frame stack
    pub fn pop_frame(&mut self) -> Result<CallFrame, RuntimeError> {
        if self.frame_count == 0 {
            return Err(RuntimeError::InvalidOperation(
                "No frames to pop".to_string(),
            ));
        }

        self.frame_count -= 1;
        Ok(self.frames[self.frame_count].clone())
    }

    /// Get the current function being executed
    pub fn current_function(&self) -> Option<&Function> {
        self.current_frame().map(|frame| frame.function())
    }

    /// Get the current chunk being executed
    pub fn current_chunk(&self) -> Option<&Chunk> {
        self.current_frame().map(|frame| frame.chunk())
    }

    // Execution tracing helper methods

    /// Create an execution trace before instruction execution
    fn create_execution_trace_before(&self) -> ExecutionTrace {
        let frame = &self.frames[self.frame_count - 1];

        let frame_info = FrameInfo {
            function_name: frame.closure.function.name.clone(),
            instruction_pointer: frame.ip,
            local_variables: self.get_local_variables_for_trace(),
            stack_base: frame.slots,
        };

        let mut trace = ExecutionTrace::new(
            OpCode::OP_NIL, // Will be updated with actual instruction
            Vec::new(),     // Will be updated with actual operands
            frame_info,
        );

        // Capture stack state before execution
        trace.set_stack_before(self.format_stack_for_trace());

        // Capture upvalue states
        for (i, upvalue) in self.open_upvalues.iter().enumerate() {
            if let Ok(upvalue_ref) = upvalue.try_borrow() {
                let state = match &upvalue_ref.location {
                    crate::object::UpvalueLocation::Stack(slot) => UpvalueState {
                        index: i,
                        is_closed: false,
                        value: if *slot < self.stack.len() {
                            self.format_value_for_trace(&self.stack[*slot])
                        } else {
                            "INVALID".to_string()
                        },
                        stack_location: Some(*slot),
                    },
                    crate::object::UpvalueLocation::Closed(value) => UpvalueState {
                        index: i,
                        is_closed: true,
                        value: self.format_value_for_trace(value),
                        stack_location: None,
                    },
                };
                trace.add_upvalue_state(state);
            }
        }

        trace
    }

    /// Complete an execution trace after instruction execution
    fn complete_execution_trace(&mut self, mut trace: ExecutionTrace) {
        // Capture stack state after execution
        trace.set_stack_after(self.format_stack_for_trace());

        // Update local variables
        trace.current_frame.local_variables = self.get_local_variables_for_trace();

        // Trace the execution
        if let Some(tracer) = &mut self.tracer {
            tracer.trace_execution(trace);
        }
    }

    /// Format the current stack for tracing
    fn format_stack_for_trace(&self) -> Vec<String> {
        let mut stack_strings = Vec::new();
        for i in 0..self.stack_top {
            stack_strings.push(self.format_value_for_trace(&self.stack[i]));
        }
        stack_strings
    }

    /// Format a value for tracing output
    fn format_value_for_trace(&self, value: &Value) -> String {
        match value {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => {
                if *b {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            Value::Nil => "nil".to_string(),
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        Object::String(s) => format!("\"{}\"", s),
                        Object::Character(c) => format!("#\\{}", c),
                        Object::Symbol(s) => s.clone(),
                        Object::Function(f) => format!("<fn {}>", f.name),
                        Object::Closure(_) => "<closure>".to_string(),
                        Object::Cons(_) => "<cons>".to_string(),
                        Object::Vector(_) => "<vector>".to_string(),
                        Object::Hashtable(_) => "<hashtable>".to_string(),
                        Object::Upvalue(_) => "<upvalue>".to_string(),
                    }
                } else {
                    "<object>".to_string()
                }
            }
        }
    }

    /// Get local variables for the current frame for tracing
    fn get_local_variables_for_trace(&self) -> HashMap<String, String> {
        let mut locals = HashMap::new();

        if let Some(frame) = self.current_frame() {
            // For now, we'll just show slot indices since we don't track variable names in the VM
            // In a full implementation, we'd need debug information from the compiler
            for i in 0..frame.closure.function.arity {
                let slot = frame.slots + i;
                if slot < self.stack.len() {
                    locals.insert(
                        format!("arg_{}", i),
                        self.format_value_for_trace(&self.stack[slot]),
                    );
                }
            }
        }

        locals
    }

    // Local variable support methods

    /// Get a local variable from the current call frame
    fn get_local(&self, slot: usize) -> Result<Value, RuntimeError> {
        if let Some(frame) = self.current_frame() {
            let absolute_slot = frame.slots + slot;
            if absolute_slot < self.stack_top {
                Ok(self.stack[absolute_slot].clone())
            } else {
                Err(RuntimeError::InvalidOperation(format!(
                    "Local variable slot {} out of bounds",
                    slot
                )))
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "No active call frame".to_string(),
            ))
        }
    }

    /// Set a local variable in the current call frame
    fn set_local(&mut self, slot: usize, value: Value) -> Result<(), RuntimeError> {
        if let Some(frame) = self.current_frame() {
            let absolute_slot = frame.slots + slot;
            if absolute_slot < self.stack_top {
                self.stack[absolute_slot] = value;
                Ok(())
            } else {
                Err(RuntimeError::InvalidOperation(format!(
                    "Local variable slot {} out of bounds",
                    slot
                )))
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "No active call frame".to_string(),
            ))
        }
    }

    // Function call support methods

    /// Call a value with the given number of arguments
    fn call_value(&mut self, arg_count: usize) -> Result<(), RuntimeError> {
        let callee = self.peek(arg_count)?.clone();

        match &callee {
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        Object::Function(function) => {
                            // Create a closure from the function and call it
                            let closure = Closure::new(Rc::new(function.clone()));
                            self.call_closure(Rc::new(closure), arg_count)
                        }
                        Object::Closure(closure) => {
                            // Call the closure directly
                            let closure_rc = Rc::new(closure.clone());
                            self.call_closure(closure_rc, arg_count)
                        }
                        _ => Err(RuntimeError::TypeError {
                            expected: "function or closure".to_string(),
                            got: self.type_name(&callee),
                        }),
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot borrow object".to_string(),
                    ))
                }
            }
            _ => Err(RuntimeError::TypeError {
                expected: "function or closure".to_string(),
                got: self.type_name(&callee),
            }),
        }
    }

    /// Call a closure with the given number of arguments
    fn call_closure(&mut self, closure: Rc<Closure>, arg_count: usize) -> Result<(), RuntimeError> {
        // Check arity
        if !closure.function.check_arity(arg_count) {
            return Err(RuntimeError::ArityMismatch {
                expected: closure.function.arity,
                got: arg_count,
            });
        }

        // Calculate the slot where the function's locals will start
        // The arguments are the function's first local variables
        // Stack layout: [..., function, arg0, arg1, ..., argN-1]
        // We want locals to start at the position of arg0
        let slots = self.stack_top - arg_count;

        // Create and push the new call frame
        self.push_frame(closure, slots)?;

        Ok(())
    }

    // Upvalue management methods

    /// Capture an upvalue for the given stack slot
    /// Returns an existing upvalue if one already exists for this slot
    pub fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<crate::object::Upvalue>> {
        // Look for an existing upvalue for this stack slot
        for upvalue in &self.open_upvalues {
            if let Ok(upvalue_ref) = upvalue.try_borrow() {
                if let crate::object::UpvalueLocation::Stack(slot) = upvalue_ref.location {
                    if slot == stack_slot {
                        return upvalue.clone();
                    }
                }
            }
        }

        // Create a new upvalue if none exists
        let new_upvalue = Rc::new(RefCell::new(crate::object::Upvalue::new_open(stack_slot)));

        // Insert the upvalue in the correct position (sorted by stack address)
        let mut insert_index = 0;
        for (i, existing_upvalue) in self.open_upvalues.iter().enumerate() {
            if let Ok(existing_ref) = existing_upvalue.try_borrow() {
                if let crate::object::UpvalueLocation::Stack(existing_slot) = existing_ref.location
                {
                    if existing_slot > stack_slot {
                        break;
                    }
                    insert_index = i + 1;
                }
            }
        }

        self.open_upvalues.insert(insert_index, new_upvalue.clone());
        new_upvalue
    }

    /// Close upvalues that are at or above the given stack slot
    /// This is called when variables leave scope
    pub fn close_upvalues(&mut self, last_slot: usize) {
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let should_close = {
                if let Ok(upvalue_ref) = self.open_upvalues[i].try_borrow() {
                    if let crate::object::UpvalueLocation::Stack(slot) = upvalue_ref.location {
                        slot >= last_slot
                    } else {
                        false // Already closed
                    }
                } else {
                    false
                }
            };

            if should_close {
                // Get the value from the stack and close the upvalue
                let upvalue = self.open_upvalues.remove(i);
                if let Ok(mut upvalue_ref) = upvalue.try_borrow_mut() {
                    if let crate::object::UpvalueLocation::Stack(slot) = upvalue_ref.location {
                        if slot < self.stack.len() {
                            let value = self.stack[slot].clone();
                            upvalue_ref.close(value);
                        }
                    }
                }
            } else {
                i += 1;
            }
        }
    }

    /// Get an upvalue by index from the current closure
    fn get_upvalue(&self, index: usize) -> Result<Value, RuntimeError> {
        if let Some(frame) = self.current_frame() {
            if index < frame.closure.upvalues.len() {
                let upvalue = &frame.closure.upvalues[index];
                if let Ok(upvalue_ref) = upvalue.try_borrow() {
                    match &upvalue_ref.location {
                        crate::object::UpvalueLocation::Stack(slot) => {
                            // Open upvalue - read from stack
                            if *slot < self.stack.len() {
                                Ok(self.stack[*slot].clone())
                            } else {
                                Err(RuntimeError::InvalidOperation(format!(
                                    "Upvalue stack slot {} out of bounds",
                                    slot
                                )))
                            }
                        }
                        crate::object::UpvalueLocation::Closed(value) => {
                            // Closed upvalue - return the stored value
                            Ok(value.clone())
                        }
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot borrow upvalue".to_string(),
                    ))
                }
            } else {
                Err(RuntimeError::InvalidOperation(format!(
                    "Upvalue index {} out of bounds",
                    index
                )))
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "No active call frame".to_string(),
            ))
        }
    }

    /// Set an upvalue by index in the current closure
    fn set_upvalue(&mut self, index: usize, value: Value) -> Result<(), RuntimeError> {
        if let Some(frame) = self.current_frame() {
            if index < frame.closure.upvalues.len() {
                let upvalue = frame.closure.upvalues[index].clone();
                if let Ok(mut upvalue_ref) = upvalue.try_borrow_mut() {
                    match &mut upvalue_ref.location {
                        crate::object::UpvalueLocation::Stack(slot) => {
                            // Open upvalue - write to stack
                            if *slot < self.stack.len() {
                                self.stack[*slot] = value;
                                Ok(())
                            } else {
                                Err(RuntimeError::InvalidOperation(format!(
                                    "Upvalue stack slot {} out of bounds",
                                    slot
                                )))
                            }
                        }
                        crate::object::UpvalueLocation::Closed(stored_value) => {
                            // Closed upvalue - update the stored value
                            *stored_value = value;
                            Ok(())
                        }
                    }
                } else {
                    Err(RuntimeError::InvalidOperation(
                        "Cannot borrow upvalue mutably".to_string(),
                    ))
                }
            } else {
                Err(RuntimeError::InvalidOperation(format!(
                    "Upvalue index {} out of bounds",
                    index
                )))
            }
        } else {
            Err(RuntimeError::InvalidOperation(
                "No active call frame".to_string(),
            ))
        }
    }

    /// Get the type name of a value for error messages
    fn type_name(&self, value: &Value) -> String {
        match value {
            Value::Number(_) => "number".to_string(),
            Value::Boolean(_) => "boolean".to_string(),
            Value::Nil => "nil".to_string(),
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        Object::String(_) => "string".to_string(),
                        Object::Character(_) => "character".to_string(),
                        Object::Symbol(_) => "symbol".to_string(),
                        Object::Cons(_) => "cons".to_string(),
                        Object::Vector(_) => "vector".to_string(),
                        Object::Hashtable(_) => "hashtable".to_string(),
                        Object::Function(_) => "function".to_string(),
                        Object::Closure(_) => "closure".to_string(),
                        Object::Upvalue(_) => "upvalue".to_string(),
                    }
                } else {
                    "object".to_string()
                }
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{Chunk, OpCode};

    #[test]
    fn test_vm_creation() {
        let vm = VM::new();
        assert_eq!(vm.stack_depth(), 0);
        assert_eq!(vm.frame_count, 0);
        assert!(vm.globals.is_empty());
        assert!(!vm.trace_execution);
    }

    #[test]
    fn test_vm_reset() {
        let mut vm = VM::new();
        vm.push(Value::Number(42.0)).unwrap();
        vm.define_global("test".to_string(), Value::Boolean(true));
        vm.enable_trace();

        vm.reset();

        assert_eq!(vm.stack_depth(), 0);
        assert_eq!(vm.frame_count, 0);
        assert!(vm.globals.is_empty());
        // Note: trace_execution is not reset by reset()
    }

    #[test]
    fn test_stack_operations() {
        let mut vm = VM::new();

        // Test push and pop
        vm.push(Value::Number(42.0)).unwrap();
        vm.push(Value::Boolean(true)).unwrap();
        vm.push(Value::Nil).unwrap();

        assert_eq!(vm.stack_depth(), 3);

        let val3 = vm.pop().unwrap();
        let val2 = vm.pop().unwrap();
        let val1 = vm.pop().unwrap();

        assert_eq!(val3, Value::Nil);
        assert_eq!(val2, Value::Boolean(true));
        assert_eq!(val1, Value::Number(42.0));
        assert_eq!(vm.stack_depth(), 0);
    }

    #[test]
    fn test_stack_peek() {
        let mut vm = VM::new();
        vm.push(Value::Number(1.0)).unwrap();
        vm.push(Value::Number(2.0)).unwrap();
        vm.push(Value::Number(3.0)).unwrap();

        // Peek at different distances
        assert_eq!(vm.peek(0).unwrap(), &Value::Number(3.0)); // Top
        assert_eq!(vm.peek(1).unwrap(), &Value::Number(2.0)); // Second
        assert_eq!(vm.peek(2).unwrap(), &Value::Number(1.0)); // Bottom

        // Stack should be unchanged
        assert_eq!(vm.stack_depth(), 3);
    }

    #[test]
    fn test_stack_underflow() {
        let mut vm = VM::new();

        // Try to pop from empty stack
        let result = vm.pop();
        assert!(matches!(result, Err(RuntimeError::StackUnderflow)));

        // Try to peek beyond stack
        vm.push(Value::Number(42.0)).unwrap();
        let result = vm.peek(1);
        assert!(matches!(result, Err(RuntimeError::StackUnderflow)));
    }

    #[test]
    fn test_global_variables() {
        let mut vm = VM::new();

        // Define a global
        vm.define_global("x".to_string(), Value::Number(42.0));

        // Get the global
        let value = vm.get_global("x").unwrap();
        assert_eq!(value, Value::Number(42.0));

        // Set the global
        vm.set_global("x", Value::Boolean(true)).unwrap();
        let value = vm.get_global("x").unwrap();
        assert_eq!(value, Value::Boolean(true));

        // Try to get undefined global
        let result = vm.get_global("undefined");
        assert!(matches!(result, Err(RuntimeError::UndefinedVariable(_))));

        // Try to set undefined global
        let result = vm.set_global("undefined", Value::Nil);
        assert!(matches!(result, Err(RuntimeError::UndefinedVariable(_))));
    }

    #[test]
    fn test_basic_instruction_execution() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a simple program: push constants and return
        chunk.write_instruction(OpCode::OP_NIL, 1);
        chunk.write_instruction(OpCode::OP_TRUE, 1);
        chunk.write_instruction(OpCode::OP_FALSE, 1);
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_pop_instruction() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Push two values, pop one, return the other
        chunk.write_instruction(OpCode::OP_TRUE, 1);
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction(OpCode::OP_POP, 1); // Pop the number
        chunk.write_instruction(OpCode::OP_RETURN, 1); // Return the boolean

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Boolean(true));
    }

    #[test]
    fn test_global_variable_instructions() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Define a global variable
        chunk.write_constant(Value::Number(42.0), 1).unwrap(); // Value to store
        chunk.add_constant(Value::string("x".to_string())); // Variable name
        chunk.write_instruction_with_byte(OpCode::OP_DEFINE_GLOBAL, 1, 1);

        // Get the global variable
        chunk.write_instruction_with_byte(OpCode::OP_GET_GLOBAL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_set_global_instruction() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Define a global variable
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.add_constant(Value::string("x".to_string()));
        chunk.write_instruction_with_byte(OpCode::OP_DEFINE_GLOBAL, 1, 1);

        // Set the global to a new value
        chunk.write_constant(Value::Boolean(true), 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_SET_GLOBAL, 1, 1);

        // Get the global (should be the new value)
        chunk.write_instruction_with_byte(OpCode::OP_GET_GLOBAL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Boolean(true));
    }

    #[test]
    fn test_upvalue_capture() {
        let mut vm = VM::new();

        // Push some values onto the stack
        vm.push(Value::Number(1.0)).unwrap();
        vm.push(Value::Number(2.0)).unwrap();
        vm.push(Value::Number(3.0)).unwrap();

        // Capture upvalues for different stack slots
        let upvalue1 = vm.capture_upvalue(0);
        let _upvalue2 = vm.capture_upvalue(2);
        let _upvalue3 = vm.capture_upvalue(1);

        // Check that upvalues are created correctly
        assert_eq!(vm.open_upvalues.len(), 3);

        // Capturing the same slot again should return the same upvalue
        let upvalue1_again = vm.capture_upvalue(0);
        assert!(Rc::ptr_eq(&upvalue1, &upvalue1_again));
        assert_eq!(vm.open_upvalues.len(), 3); // No new upvalue created

        // Check that upvalues are sorted by stack address
        let slots: Vec<usize> = vm
            .open_upvalues
            .iter()
            .filter_map(|uv| {
                if let Ok(uv_ref) = uv.try_borrow() {
                    if let crate::object::UpvalueLocation::Stack(slot) = uv_ref.location {
                        Some(slot)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(slots, vec![0, 1, 2]);
    }

    #[test]
    fn test_upvalue_closing() {
        let mut vm = VM::new();

        // Push some values onto the stack
        vm.push(Value::Number(10.0)).unwrap();
        vm.push(Value::Number(20.0)).unwrap();
        vm.push(Value::Number(30.0)).unwrap();

        // Capture upvalues
        let upvalue0 = vm.capture_upvalue(0);
        let upvalue1 = vm.capture_upvalue(1);
        let upvalue2 = vm.capture_upvalue(2);

        assert_eq!(vm.open_upvalues.len(), 3);

        // Close upvalues at slot 1 and above
        vm.close_upvalues(1);

        // Only upvalue for slot 0 should remain open
        assert_eq!(vm.open_upvalues.len(), 1);

        // Check that upvalue0 is still open
        if let Ok(uv_ref) = upvalue0.try_borrow() {
            assert!(matches!(
                uv_ref.location,
                crate::object::UpvalueLocation::Stack(0)
            ));
        }

        // Check that upvalue1 and upvalue2 are closed with correct values
        if let Ok(uv_ref) = upvalue1.try_borrow() {
            assert!(
                matches!(uv_ref.location, crate::object::UpvalueLocation::Closed(Value::Number(n)) if n == 20.0)
            );
        }

        if let Ok(uv_ref) = upvalue2.try_borrow() {
            assert!(
                matches!(uv_ref.location, crate::object::UpvalueLocation::Closed(Value::Number(n)) if n == 30.0)
            );
        }
    }

    #[test]
    fn test_closure_creation() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a simple function with no upvalues
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction(OpCode::OP_NIL, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 0,
        };

        // Add the function as a constant
        let function_value = Value::function(function);
        chunk.write_constant(function_value, 1).unwrap();

        // Create a closure from the function
        chunk.write_instruction_with_byte(OpCode::OP_CLOSURE, 0, 1);
        // No upvalue metadata since upvalue_count is 0

        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();

        // Result should be a closure
        assert!(result.is_closure());
    }

    #[test]
    fn test_closure_with_upvalues() {
        let mut vm = VM::new();

        // Manually set up the VM state to simulate having a local variable
        vm.push(Value::Number(42.0)).unwrap(); // This will be at stack slot 0

        // Create a function that captures one upvalue
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction(OpCode::OP_NIL, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 1,
        };

        // Create a main function and closure to execute
        let main_function = Function {
            name: "main".to_string(),
            arity: 0,
            chunk: Chunk::new(),
            upvalue_count: 0,
        };

        let main_closure = Closure::new(Rc::new(main_function));
        let main_closure_rc = Rc::new(main_closure);

        // Set up the call frame with the local variable at slot 0
        vm.push_frame(main_closure_rc, 0).unwrap();

        // Now manually execute the OP_CLOSURE instruction
        let function_value = Value::function(function);
        vm.push(function_value).unwrap();

        // Simulate the OP_CLOSURE instruction execution
        let function_val = vm.pop().unwrap();
        let function_obj = match &function_val {
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    if let Object::Function(func) = &*obj_ref {
                        Rc::new(func.clone())
                    } else {
                        panic!("Expected function");
                    }
                } else {
                    panic!("Cannot borrow function");
                }
            }
            _ => panic!("Expected function object"),
        };

        // Create the closure
        let mut closure = Closure::new(function_obj.clone());

        // Capture upvalue (is_local=true, index=0)
        let upvalue = vm.capture_upvalue(0); // Capture stack slot 0
        closure.upvalues.push(upvalue);

        // Push the closure back
        let closure_value = Value::closure(closure);
        vm.push(closure_value).unwrap();

        // Check that an upvalue was created
        assert_eq!(vm.open_upvalues.len(), 1);

        // The closure should be on the stack
        let result = vm.pop().unwrap();
        assert!(result.is_closure());
    }

    #[test]
    fn test_upvalue_access_instructions() {
        let mut vm = VM::new();

        // Set up a closure with upvalues
        vm.push(Value::Number(42.0)).unwrap(); // Stack slot 0
        vm.push(Value::Number(100.0)).unwrap(); // Stack slot 1

        // Create a function that uses upvalues
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_UPVALUE, 0, 1); // Get first upvalue
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_UPVALUE, 1, 1); // Get second upvalue
        function_chunk.write_instruction(OpCode::OP_ADD, 1); // Add them
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 2,
        };

        // Create a closure with two upvalues
        let mut closure = Closure::new(Rc::new(function));
        let upvalue1 = vm.capture_upvalue(0); // Capture stack slot 0 (42.0)
        let upvalue2 = vm.capture_upvalue(1); // Capture stack slot 1 (100.0)
        closure.upvalues.push(upvalue1);
        closure.upvalues.push(upvalue2);

        // Set up a call frame for the closure
        vm.push_frame(Rc::new(closure), 2).unwrap(); // Start after the captured values

        let result = vm.run().unwrap();

        // Result should be 42.0 + 100.0 = 142.0
        assert_eq!(result, Value::Number(142.0));
    }

    #[test]
    fn test_upvalue_set_instruction() {
        let mut vm = VM::new();

        // Set up initial values
        vm.push(Value::Number(42.0)).unwrap(); // Stack slot 0

        // Create a function that modifies an upvalue
        let mut function_chunk = Chunk::new();
        function_chunk
            .write_constant(Value::Number(999.0), 1)
            .unwrap(); // New value
        function_chunk.write_instruction_with_byte(OpCode::OP_SET_UPVALUE, 0, 1); // Set first upvalue
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_UPVALUE, 0, 1); // Get the modified value
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 1,
        };

        // Create a closure with one upvalue
        let mut closure = Closure::new(Rc::new(function));
        let upvalue = vm.capture_upvalue(0); // Capture stack slot 0
        closure.upvalues.push(upvalue);

        // Set up a call frame for the closure
        vm.push_frame(Rc::new(closure), 1).unwrap(); // Start after the captured value

        let result = vm.run().unwrap();

        // Result should be the new value (999.0)
        assert_eq!(result, Value::Number(999.0));

        // The original stack slot should also be modified
        assert_eq!(vm.stack[0], Value::Number(999.0));
    }

    #[test]
    fn test_closed_upvalue_access() {
        let mut vm = VM::new();

        // Set up initial values
        vm.push(Value::Number(42.0)).unwrap(); // Stack slot 0

        // Create a closure with one upvalue
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_UPVALUE, 0, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 1,
        };

        let mut closure = Closure::new(Rc::new(function));
        let upvalue = vm.capture_upvalue(0);
        closure.upvalues.push(upvalue.clone());

        // Close the upvalue manually
        vm.close_upvalues(0);

        // Verify the upvalue is closed
        if let Ok(uv_ref) = upvalue.try_borrow() {
            assert!(uv_ref.is_closed());
        }

        // Set up a call frame and run
        vm.push_frame(Rc::new(closure), 1).unwrap();

        let result = vm.run().unwrap();

        // Should still return the original value even though it's closed
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_close_upvalue_instruction() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Set up some local variables
        chunk.write_constant(Value::Number(10.0), 1).unwrap(); // Local 0
        chunk.write_constant(Value::Number(20.0), 1).unwrap(); // Local 1
        chunk.write_constant(Value::Number(30.0), 1).unwrap(); // Local 2

        // Create a function that captures upvalues
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction(OpCode::OP_NIL, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function {
            name: "test_func".to_string(),
            arity: 0,
            chunk: function_chunk,
            upvalue_count: 2,
        };

        // Add function as constant
        chunk.write_constant(Value::function(function), 1).unwrap();

        // Create closure with upvalues
        chunk.write_instruction_with_byte(OpCode::OP_CLOSURE, 3, 1);
        chunk.write_byte(1, 1); // is_local = true
        chunk.write_byte(0, 1); // index = 0 (first local)
        chunk.write_byte(1, 1); // is_local = true  
        chunk.write_byte(1, 1); // index = 1 (second local)

        // Close upvalue at slot 1 (this should close the second upvalue)
        chunk.write_instruction_with_byte(OpCode::OP_CLOSE_UPVALUE, 1, 1);

        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();

        // Result should be the closure
        assert!(result.is_closure());

        // Since OP_RETURN closes all upvalues at frame.slots (which is 0),
        // all upvalues should be closed
        assert_eq!(vm.open_upvalues.len(), 0);
    }

    #[test]
    fn test_automatic_upvalue_closing_on_return() {
        let mut vm = VM::new();

        // Create a nested function scenario
        // Outer function creates locals, inner function captures them, then outer returns

        // Set up outer function locals
        vm.push(Value::Number(42.0)).unwrap(); // Slot 0
        vm.push(Value::Number(100.0)).unwrap(); // Slot 1

        // Create inner function that captures upvalues
        let mut inner_chunk = Chunk::new();
        inner_chunk.write_instruction_with_byte(OpCode::OP_GET_UPVALUE, 0, 1);
        inner_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let inner_function = Function {
            name: "inner".to_string(),
            arity: 0,
            chunk: inner_chunk,
            upvalue_count: 1,
        };

        // Create closure for inner function
        let mut inner_closure = Closure::new(Rc::new(inner_function));
        let upvalue = vm.capture_upvalue(0); // Capture slot 0
        inner_closure.upvalues.push(upvalue.clone());

        // Verify upvalue is initially open
        assert_eq!(vm.open_upvalues.len(), 1);
        if let Ok(uv_ref) = upvalue.try_borrow() {
            assert!(!uv_ref.is_closed());
        }

        // Create outer function frame
        let outer_function = Function {
            name: "outer".to_string(),
            arity: 0,
            chunk: Chunk::new(),
            upvalue_count: 0,
        };
        let outer_closure = Closure::new(Rc::new(outer_function));
        vm.push_frame(Rc::new(outer_closure), 0).unwrap();

        // Now simulate the outer function returning (which should close upvalues)
        vm.push(Value::Number(999.0)).unwrap(); // Return value
        let frame = vm.pop_frame().unwrap();
        vm.close_upvalues(frame.slots); // This should close upvalues at slot 0 and above

        // Verify upvalue is now closed
        if let Ok(uv_ref) = upvalue.try_borrow() {
            assert!(uv_ref.is_closed());
            if let crate::object::UpvalueLocation::Closed(value) = &uv_ref.location {
                assert_eq!(*value, Value::Number(42.0));
            }
        }

        // Open upvalues list should be empty
        assert_eq!(vm.open_upvalues.len(), 0);
    }

    #[test]
    fn test_type_name_function() {
        let vm = VM::new();

        assert_eq!(vm.type_name(&Value::Number(42.0)), "number");
        assert_eq!(vm.type_name(&Value::Boolean(true)), "boolean");
        assert_eq!(vm.type_name(&Value::Nil), "nil");
        assert_eq!(vm.type_name(&Value::string("test".to_string())), "string");
        assert_eq!(vm.type_name(&Value::character('a')), "character");
    }

    #[test]
    fn test_trace_enable_disable() {
        let mut vm = VM::new();
        assert!(!vm.trace_execution);

        vm.enable_trace();
        assert!(vm.trace_execution);

        vm.disable_trace();
        assert!(!vm.trace_execution);
    }

    #[test]
    fn test_call_frame_management() {
        let mut vm = VM::new();

        // Initially no frames
        assert_eq!(vm.frame_count, 0);
        assert!(vm.current_frame().is_none());
        assert!(vm.current_function().is_none());
        assert!(vm.current_chunk().is_none());

        // Create a test function and closure
        let function = Function::new("test".to_string(), 0);
        let closure = Closure::new(Rc::new(function));
        let closure_rc = Rc::new(closure);

        // Push a frame
        vm.push_frame(closure_rc.clone(), 0).unwrap();
        assert_eq!(vm.frame_count, 1);
        assert!(vm.current_frame().is_some());
        assert!(vm.current_function().is_some());
        assert!(vm.current_chunk().is_some());

        // Check frame properties
        let frame = vm.current_frame().unwrap();
        assert_eq!(frame.slots, 0);
        assert_eq!(frame.ip, 0);
        assert_eq!(frame.function().name, "test");
        assert_eq!(frame.function().arity, 0);
        assert!(frame.check_arity(0));
        assert!(!frame.check_arity(1));

        // Pop the frame
        let popped_frame = vm.pop_frame().unwrap();
        assert_eq!(vm.frame_count, 0);
        assert_eq!(popped_frame.function().name, "test");
        assert!(vm.current_frame().is_none());
    }

    #[test]
    fn test_call_frame_stack_overflow() {
        let mut vm = VM::new();
        let function = Function::new("test".to_string(), 0);
        let closure = Closure::new(Rc::new(function));
        let closure_rc = Rc::new(closure);

        // Fill up the frame stack
        for _ in 0..FRAMES_MAX {
            vm.push_frame(closure_rc.clone(), 0).unwrap();
        }

        // Next push should fail
        let result = vm.push_frame(closure_rc, 0);
        assert!(matches!(result, Err(RuntimeError::StackOverflow)));
    }

    #[test]
    fn test_call_frame_underflow() {
        let mut vm = VM::new();

        // Try to pop from empty frame stack
        let result = vm.pop_frame();
        assert!(matches!(result, Err(RuntimeError::InvalidOperation(_))));
    }

    #[test]
    fn test_function_methods() {
        let function = Function::new("test_func".to_string(), 2);

        assert_eq!(function.name(), "test_func");
        assert_eq!(function.arity(), 2);
        assert!(function.check_arity(2));
        assert!(!function.check_arity(1));
        assert!(!function.check_arity(3));

        // Test function with chunk
        let chunk = Chunk::new();
        let function_with_chunk = Function::with_chunk("test2".to_string(), 1, chunk);
        assert_eq!(function_with_chunk.name(), "test2");
        assert_eq!(function_with_chunk.arity(), 1);
    }

    #[test]
    fn test_simple_function_call() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a simple function that returns 42
        let mut function_chunk = Chunk::new();
        function_chunk
            .write_constant(Value::Number(42.0), 1)
            .unwrap();
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("test_func".to_string(), 0, function_chunk);
        let function_value = Value::function(function);

        // Main program: push function, call it with 0 args, return result
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 0, 1); // 0 arguments
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_function_call_with_arguments() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that adds its two arguments
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 0, 1); // First arg
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 1, 1); // Second arg
        function_chunk.write_instruction(OpCode::OP_ADD, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("add_func".to_string(), 2, function_chunk);
        let function_value = Value::function(function);

        // Main program: push function, push args, call function
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(3.0), 1).unwrap(); // First arg
        chunk.write_constant(Value::Number(4.0), 1).unwrap(); // Second arg
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 2, 1); // 2 arguments
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn test_function_call_arity_mismatch() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that expects 2 arguments
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction(OpCode::OP_NIL, 1);
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("test_func".to_string(), 2, function_chunk);
        let function_value = Value::function(function);

        // Try to call with wrong number of arguments
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(1.0), 1).unwrap(); // Only 1 arg, but function expects 2
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk);
        assert!(matches!(
            result,
            Err(RuntimeError::ArityMismatch {
                expected: 2,
                got: 1
            })
        ));
    }

    #[test]
    fn test_call_non_function() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Try to call a number (not a function)
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 0, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk);
        assert!(matches!(result, Err(RuntimeError::TypeError { .. })));
    }

    #[test]
    fn test_local_variable_access() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that returns its first argument
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 0, 1); // Get first arg
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("identity".to_string(), 1, function_chunk);
        let function_value = Value::function(function);

        // Main program: call function with argument 42
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_local_variable_set() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that modifies its first argument and returns it
        let mut function_chunk = Chunk::new();
        function_chunk
            .write_constant(Value::Number(100.0), 1)
            .unwrap(); // New value
        function_chunk.write_instruction_with_byte(OpCode::OP_SET_LOCAL, 0, 1); // Set first arg
        function_chunk.write_instruction(OpCode::OP_POP, 1); // Pop the assignment result
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 0, 1); // Get modified arg
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("modify".to_string(), 1, function_chunk);
        let function_value = Value::function(function);

        // Main program: call function with argument 42
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(100.0));
    }

    #[test]
    fn test_multiple_local_variables() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that swaps its two arguments and returns the first
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 1, 1); // Get second arg
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 0, 1); // Get first arg
        function_chunk.write_instruction_with_byte(OpCode::OP_SET_LOCAL, 1, 1); // Set second = first
        function_chunk.write_instruction(OpCode::OP_POP, 1); // Pop assignment result
        function_chunk.write_instruction_with_byte(OpCode::OP_SET_LOCAL, 0, 1); // Set first = original second
        function_chunk.write_instruction(OpCode::OP_POP, 1); // Pop assignment result
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 0, 1); // Return new first
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("swap_first".to_string(), 2, function_chunk);
        let function_value = Value::function(function);

        // Main program: call function with arguments 10, 20
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(10.0), 1).unwrap(); // First arg
        chunk.write_constant(Value::Number(20.0), 1).unwrap(); // Second arg
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 2, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(20.0)); // Should return the original second arg
    }

    #[test]
    fn test_local_variable_out_of_bounds() {
        let mut vm = VM::new();
        let mut chunk = Chunk::new();

        // Create a function that tries to access a non-existent local variable
        let mut function_chunk = Chunk::new();
        function_chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 5, 1); // Out of bounds
        function_chunk.write_instruction(OpCode::OP_RETURN, 1);

        let function = Function::with_chunk("bad_access".to_string(), 1, function_chunk);
        let function_value = Value::function(function);

        // Main program: call function with one argument
        chunk.write_constant(function_value, 1).unwrap();
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        chunk.write_instruction_with_byte(OpCode::OP_CALL, 1, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);

        let result = vm.interpret(&chunk);
        assert!(matches!(result, Err(RuntimeError::InvalidOperation(_))));
    }
}
#[test]
fn test_arithmetic_operations() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test addition: 3 + 4 = 7
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_constant(Value::Number(4.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_ADD, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(7.0));
}

#[test]
fn test_subtraction() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test subtraction: 10 - 3 = 7
    chunk.write_constant(Value::Number(10.0), 1).unwrap();
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_SUBTRACT, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(7.0));
}

#[test]
fn test_multiplication() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test multiplication: 6 * 7 = 42
    chunk.write_constant(Value::Number(6.0), 1).unwrap();
    chunk.write_constant(Value::Number(7.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_MULTIPLY, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn test_division() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test division: 15 / 3 = 5
    chunk.write_constant(Value::Number(15.0), 1).unwrap();
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_DIVIDE, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(5.0));
}

#[test]
fn test_division_by_zero() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test division by zero: 5 / 0
    chunk.write_constant(Value::Number(5.0), 1).unwrap();
    chunk.write_constant(Value::Number(0.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_DIVIDE, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk);
    assert!(matches!(result, Err(RuntimeError::DivisionByZero)));
}

#[test]
fn test_comparison_operations() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test greater than: 5 > 3 = true
    chunk.write_constant(Value::Number(5.0), 1).unwrap();
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_GREATER, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn test_less_than() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test less than: 3 < 5 = true
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_constant(Value::Number(5.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_LESS, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn test_equality() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test equality: 42 == 42 = true
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_EQUAL, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn test_equality_different_values() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test equality: 42 == 43 = false
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_constant(Value::Number(43.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_EQUAL, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn test_unary_not() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test NOT true = false
    chunk.write_instruction(OpCode::OP_TRUE, 1);
    chunk.write_instruction(OpCode::OP_NOT, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn test_unary_not_falsy() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test NOT false = true
    chunk.write_instruction(OpCode::OP_FALSE, 1);
    chunk.write_instruction(OpCode::OP_NOT, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn test_unary_not_truthy() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test NOT 42 = false (numbers are truthy)
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_NOT, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn test_unary_negate() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test negate: -42 = -42
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_NEGATE, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(-42.0));
}

#[test]
fn test_negate_type_error() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test negate on non-number: -true (should error)
    chunk.write_instruction(OpCode::OP_TRUE, 1);
    chunk.write_instruction(OpCode::OP_NEGATE, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk);
    assert!(matches!(result, Err(RuntimeError::TypeError { .. })));
}

#[test]
fn test_arithmetic_type_error() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test adding number and boolean: 42 + true (should error)
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_TRUE, 1);
    chunk.write_instruction(OpCode::OP_ADD, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk);
    assert!(matches!(result, Err(RuntimeError::TypeError { .. })));
}

#[test]
fn test_comparison_type_error() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test comparing number and boolean: 42 > true (should error)
    chunk.write_constant(Value::Number(42.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_TRUE, 1);
    chunk.write_instruction(OpCode::OP_GREATER, 1);
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk);
    assert!(matches!(result, Err(RuntimeError::TypeError { .. })));
}

#[test]
fn test_complex_expression() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Test complex expression: (3 + 4) * 2 - 1 = 13
    chunk.write_constant(Value::Number(3.0), 1).unwrap();
    chunk.write_constant(Value::Number(4.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_ADD, 1); // Stack: [7]
    chunk.write_constant(Value::Number(2.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_MULTIPLY, 1); // Stack: [14]
    chunk.write_constant(Value::Number(1.0), 1).unwrap();
    chunk.write_instruction(OpCode::OP_SUBTRACT, 1); // Stack: [13]
    chunk.write_instruction(OpCode::OP_RETURN, 1);

    let result = vm.interpret(&chunk).unwrap();
    assert_eq!(result, Value::Number(13.0));
}
