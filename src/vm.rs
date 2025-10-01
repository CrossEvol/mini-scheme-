use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::{Value, Object, Closure, Function};
use crate::bytecode::{OpCode, Chunk};

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
            RuntimeError::TypeError { expected, got } => write!(f, "Type error: expected {}, got {}", expected, got),
            RuntimeError::ArityMismatch { expected, got } => write!(f, "Arity mismatch: expected {} arguments, got {}", expected, got),
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
    pub ip: usize,              // Instruction pointer
    pub slots: usize,           // Base of stack frame for local variables
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
    }

    /// Enable execution tracing
    pub fn enable_trace(&mut self) {
        self.trace_execution = true;
    }

    /// Disable execution tracing
    pub fn disable_trace(&mut self) {
        self.trace_execution = false;
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
        self.globals.get(name)
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

        // Create the initial call frame
        let frame = CallFrame::new(main_closure_rc, 0);
        self.frames.push(frame);
        self.frame_count = 1;

        // Run the main execution loop
        self.run()
    }

    /// Main execution loop
    fn run(&mut self) -> Result<Value, RuntimeError> {
        loop {
            if self.trace_execution {
                self.print_stack();
                let frame = &self.frames[self.frame_count - 1];
                let chunk = &frame.closure.function.chunk;
                let disassembler = crate::bytecode::Disassembler::new();
                disassembler.disassemble_instruction(chunk, frame.ip);
            }

            let instruction = self.read_byte()?;
            let opcode = OpCode::from_byte(instruction)
                .ok_or_else(|| RuntimeError::InvalidOperation(format!("Unknown opcode: {}", instruction)))?;

            match opcode {
                OpCode::OP_CONSTANT => {
                    let constant_index = self.read_byte()? as usize;
                    let constant = self.read_constant(constant_index)?;
                    self.push(constant)?;
                }

                OpCode::OP_NIL => {
                    self.push(Value::Nil)?;
                }

                OpCode::OP_TRUE => {
                    self.push(Value::Boolean(true))?;
                }

                OpCode::OP_FALSE => {
                    self.push(Value::Boolean(false))?;
                }

                OpCode::OP_POP => {
                    self.pop()?;
                }

                OpCode::OP_GET_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.get_global(&name)?;
                    self.push(value)?;
                }

                OpCode::OP_DEFINE_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.pop()?;
                    self.define_global(name, value);
                }

                OpCode::OP_SET_GLOBAL => {
                    let constant_index = self.read_byte()? as usize;
                    let name = self.read_string_constant(constant_index)?;
                    let value = self.peek(0)?.clone(); // Don't pop yet in case of error
                    self.set_global(&name, value)?;
                    // Only pop after successful assignment
                    self.pop()?;
                }

                OpCode::OP_EQUAL => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Boolean(a.equal(&b)))?;
                }

                OpCode::OP_GREATER => {
                    self.binary_comparison(|a, b| a > b)?;
                }

                OpCode::OP_LESS => {
                    self.binary_comparison(|a, b| a < b)?;
                }

                OpCode::OP_ADD => {
                    self.binary_arithmetic(|a, b| a + b)?;
                }

                OpCode::OP_SUBTRACT => {
                    self.binary_arithmetic(|a, b| a - b)?;
                }

                OpCode::OP_MULTIPLY => {
                    self.binary_arithmetic(|a, b| a * b)?;
                }

                OpCode::OP_DIVIDE => {
                    self.binary_divide()?;
                }

                OpCode::OP_NOT => {
                    let value = self.pop()?;
                    self.push(Value::Boolean(value.is_falsy()))?;
                }

                OpCode::OP_NEGATE => {
                    let value = self.pop()?;
                    match value {
                        Value::Number(n) => self.push(Value::Number(-n))?,
                        _ => return Err(RuntimeError::TypeError {
                            expected: "number".to_string(),
                            got: self.type_name(&value),
                        }),
                    }
                }

                OpCode::OP_RETURN => {
                    let result = self.pop()?;
                    self.frame_count -= 1;
                    
                    if self.frame_count == 0 {
                        // End of program
                        return Ok(result);
                    }
                    
                    // Restore stack to frame boundary
                    let frame = &self.frames[self.frame_count];
                    self.stack_top = frame.slots;
                    self.push(result)?;
                }

                _ => {
                    return Err(RuntimeError::InvalidOperation(
                        format!("Unimplemented opcode: {:?}", opcode)
                    ));
                }
            }
        }
    }

    // Helper methods for instruction execution

    /// Read a byte from the current instruction stream
    fn read_byte(&mut self) -> Result<u8, RuntimeError> {
        let frame = &mut self.frames[self.frame_count - 1];
        let chunk = &frame.closure.function.chunk;
        
        if frame.ip >= chunk.code.len() {
            return Err(RuntimeError::InvalidOperation("Instruction pointer out of bounds".to_string()));
        }
        
        let byte = chunk.code[frame.ip];
        frame.ip += 1;
        Ok(byte)
    }

    /// Read a constant from the constant pool
    fn read_constant(&self, index: usize) -> Result<Value, RuntimeError> {
        let frame = &self.frames[self.frame_count - 1];
        let chunk = &frame.closure.function.chunk;
        
        chunk.constants.get(index)
            .cloned()
            .ok_or_else(|| RuntimeError::InvalidOperation(format!("Invalid constant index: {}", index)))
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
                    Err(RuntimeError::InvalidOperation("Cannot borrow object".to_string()))
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
}    #[
test]
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
        chunk.write_instruction(OpCode::OP_ADD, 1);           // Stack: [7]
        chunk.write_constant(Value::Number(2.0), 1).unwrap();
        chunk.write_instruction(OpCode::OP_MULTIPLY, 1);      // Stack: [14]
        chunk.write_constant(Value::Number(1.0), 1).unwrap();
        chunk.write_instruction(OpCode::OP_SUBTRACT, 1);      // Stack: [13]
        chunk.write_instruction(OpCode::OP_RETURN, 1);
        
        let result = vm.interpret(&chunk).unwrap();
        assert_eq!(result, Value::Number(13.0));
    }