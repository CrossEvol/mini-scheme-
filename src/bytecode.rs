use crate::object::Value;

/// Bytecode instruction opcodes for the MiniScheme virtual machine
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum OpCode {
    // Constants and literals
    OP_CONSTANT = 0,     // Load constant from constant pool
    OP_NIL = 1,          // Push nil value
    OP_TRUE = 2,         // Push true value
    OP_FALSE = 3,        // Push false value
    
    // Stack operations
    OP_POP = 4,          // Pop top value from stack
    
    // Variable operations
    OP_GET_LOCAL = 5,    // Get local variable by slot index
    OP_SET_LOCAL = 6,    // Set local variable by slot index
    OP_GET_GLOBAL = 7,   // Get global variable by name
    OP_DEFINE_GLOBAL = 8,// Define global variable
    OP_SET_GLOBAL = 9,   // Set global variable by name
    OP_GET_UPVALUE = 10, // Get upvalue by index
    OP_SET_UPVALUE = 11, // Set upvalue by index
    
    // Comparison operations
    OP_EQUAL = 12,       // Equality comparison
    OP_GREATER = 13,     // Greater than comparison
    OP_LESS = 14,        // Less than comparison
    
    // Arithmetic operations
    OP_ADD = 15,         // Addition
    OP_SUBTRACT = 16,    // Subtraction
    OP_MULTIPLY = 17,    // Multiplication
    OP_DIVIDE = 18,      // Division
    
    // Unary operations
    OP_NOT = 19,         // Logical not
    OP_NEGATE = 20,      // Arithmetic negation
    
    // I/O operations
    OP_PRINT = 21,       // Print value to stdout
    
    // Control flow
    OP_JUMP = 22,        // Unconditional jump
    OP_JUMP_IF_FALSE = 23,// Conditional jump
    OP_LOOP = 24,        // Loop back jump
    
    // Function operations
    OP_CALL = 25,        // Call function/closure
    OP_CLOSURE = 26,     // Create closure with upvalue capture
    OP_CLOSE_UPVALUE = 27,// Close upvalues when leaving scope
    OP_RETURN = 28,      // Return from function
    
    // Additional MiniScheme operations
    OP_CONS = 29,        // Create cons cell from two stack values
    OP_CAR = 30,         // Get car of cons cell
    OP_CDR = 31,         // Get cdr of cons cell
    OP_VECTOR = 32,      // Create vector from stack values
    OP_MAKE_HASHTABLE = 33, // Create hashtable
}

impl OpCode {
    /// Convert a byte to an OpCode
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        match byte {
            0 => Some(OpCode::OP_CONSTANT),
            1 => Some(OpCode::OP_NIL),
            2 => Some(OpCode::OP_TRUE),
            3 => Some(OpCode::OP_FALSE),
            4 => Some(OpCode::OP_POP),
            5 => Some(OpCode::OP_GET_LOCAL),
            6 => Some(OpCode::OP_SET_LOCAL),
            7 => Some(OpCode::OP_GET_GLOBAL),
            8 => Some(OpCode::OP_DEFINE_GLOBAL),
            9 => Some(OpCode::OP_SET_GLOBAL),
            10 => Some(OpCode::OP_GET_UPVALUE),
            11 => Some(OpCode::OP_SET_UPVALUE),
            12 => Some(OpCode::OP_EQUAL),
            13 => Some(OpCode::OP_GREATER),
            14 => Some(OpCode::OP_LESS),
            15 => Some(OpCode::OP_ADD),
            16 => Some(OpCode::OP_SUBTRACT),
            17 => Some(OpCode::OP_MULTIPLY),
            18 => Some(OpCode::OP_DIVIDE),
            19 => Some(OpCode::OP_NOT),
            20 => Some(OpCode::OP_NEGATE),
            21 => Some(OpCode::OP_PRINT),
            22 => Some(OpCode::OP_JUMP),
            23 => Some(OpCode::OP_JUMP_IF_FALSE),
            24 => Some(OpCode::OP_LOOP),
            25 => Some(OpCode::OP_CALL),
            26 => Some(OpCode::OP_CLOSURE),
            27 => Some(OpCode::OP_CLOSE_UPVALUE),
            28 => Some(OpCode::OP_RETURN),
            29 => Some(OpCode::OP_CONS),
            30 => Some(OpCode::OP_CAR),
            31 => Some(OpCode::OP_CDR),
            32 => Some(OpCode::OP_VECTOR),
            33 => Some(OpCode::OP_MAKE_HASHTABLE),
            _ => None,
        }
    }

    /// Convert OpCode to byte
    pub fn to_byte(self) -> u8 {
        self as u8
    }

    /// Get the instruction size in bytes (including operands)
    pub fn instruction_size(self) -> usize {
        match self {
            // Instructions with no operands (1 byte)
            OpCode::OP_NIL | OpCode::OP_TRUE | OpCode::OP_FALSE |
            OpCode::OP_POP | OpCode::OP_EQUAL | OpCode::OP_GREATER |
            OpCode::OP_LESS | OpCode::OP_ADD | OpCode::OP_SUBTRACT |
            OpCode::OP_MULTIPLY | OpCode::OP_DIVIDE | OpCode::OP_NOT |
            OpCode::OP_NEGATE | OpCode::OP_PRINT | OpCode::OP_RETURN |
            OpCode::OP_CONS | OpCode::OP_CAR | OpCode::OP_CDR |
            OpCode::OP_MAKE_HASHTABLE => 1,

            // Instructions with 1-byte operand (2 bytes total)
            OpCode::OP_CONSTANT | OpCode::OP_GET_LOCAL | OpCode::OP_SET_LOCAL |
            OpCode::OP_GET_GLOBAL | OpCode::OP_DEFINE_GLOBAL | OpCode::OP_SET_GLOBAL |
            OpCode::OP_GET_UPVALUE | OpCode::OP_SET_UPVALUE | OpCode::OP_CALL |
            OpCode::OP_CLOSE_UPVALUE | OpCode::OP_VECTOR => 2,

            // Instructions with 2-byte operand (3 bytes total)
            OpCode::OP_JUMP | OpCode::OP_JUMP_IF_FALSE | OpCode::OP_LOOP => 3,

            // Variable-size instruction (closure has variable operands)
            OpCode::OP_CLOSURE => 2, // Base size, actual size depends on upvalue count
        }
    }

    /// Check if this instruction has operands
    pub fn has_operands(self) -> bool {
        self.instruction_size() > 1
    }

    /// Get the number of operand bytes for this instruction
    pub fn operand_count(self) -> usize {
        self.instruction_size() - 1
    }

    /// Get a human-readable name for the instruction
    pub fn name(self) -> &'static str {
        match self {
            OpCode::OP_CONSTANT => "OP_CONSTANT",
            OpCode::OP_NIL => "OP_NIL",
            OpCode::OP_TRUE => "OP_TRUE",
            OpCode::OP_FALSE => "OP_FALSE",
            OpCode::OP_POP => "OP_POP",
            OpCode::OP_GET_LOCAL => "OP_GET_LOCAL",
            OpCode::OP_SET_LOCAL => "OP_SET_LOCAL",
            OpCode::OP_GET_GLOBAL => "OP_GET_GLOBAL",
            OpCode::OP_DEFINE_GLOBAL => "OP_DEFINE_GLOBAL",
            OpCode::OP_SET_GLOBAL => "OP_SET_GLOBAL",
            OpCode::OP_GET_UPVALUE => "OP_GET_UPVALUE",
            OpCode::OP_SET_UPVALUE => "OP_SET_UPVALUE",
            OpCode::OP_EQUAL => "OP_EQUAL",
            OpCode::OP_GREATER => "OP_GREATER",
            OpCode::OP_LESS => "OP_LESS",
            OpCode::OP_ADD => "OP_ADD",
            OpCode::OP_SUBTRACT => "OP_SUBTRACT",
            OpCode::OP_MULTIPLY => "OP_MULTIPLY",
            OpCode::OP_DIVIDE => "OP_DIVIDE",
            OpCode::OP_NOT => "OP_NOT",
            OpCode::OP_NEGATE => "OP_NEGATE",
            OpCode::OP_PRINT => "OP_PRINT",
            OpCode::OP_JUMP => "OP_JUMP",
            OpCode::OP_JUMP_IF_FALSE => "OP_JUMP_IF_FALSE",
            OpCode::OP_LOOP => "OP_LOOP",
            OpCode::OP_CALL => "OP_CALL",
            OpCode::OP_CLOSURE => "OP_CLOSURE",
            OpCode::OP_CLOSE_UPVALUE => "OP_CLOSE_UPVALUE",
            OpCode::OP_RETURN => "OP_RETURN",
            OpCode::OP_CONS => "OP_CONS",
            OpCode::OP_CAR => "OP_CAR",
            OpCode::OP_CDR => "OP_CDR",
            OpCode::OP_VECTOR => "OP_VECTOR",
            OpCode::OP_MAKE_HASHTABLE => "OP_MAKE_HASHTABLE",
        }
    }
}

/// Bytecode chunk containing instructions, constants, and debug information
#[derive(Debug, Clone)]
pub struct Chunk {
    /// Bytecode instructions and operands
    pub code: Vec<u8>,
    /// Constant pool for literal values
    pub constants: Vec<Value>,
    /// Line number information for debugging (parallel to code)
    pub lines: Vec<usize>,
}

impl Chunk {
    /// Create a new empty chunk
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    /// Write a byte to the chunk
    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Write an instruction to the chunk
    pub fn write_instruction(&mut self, opcode: OpCode, line: usize) {
        self.write_byte(opcode.to_byte(), line);
    }

    /// Write an instruction with a 1-byte operand
    pub fn write_instruction_with_byte(&mut self, opcode: OpCode, operand: u8, line: usize) {
        self.write_instruction(opcode, line);
        self.write_byte(operand, line);
    }

    /// Write an instruction with a 2-byte operand (big-endian)
    pub fn write_instruction_with_short(&mut self, opcode: OpCode, operand: u16, line: usize) {
        self.write_instruction(opcode, line);
        self.write_byte((operand >> 8) as u8, line);  // High byte
        self.write_byte(operand as u8, line);         // Low byte
    }

    /// Add a constant to the constant pool and return its index
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Write a constant instruction
    pub fn write_constant(&mut self, value: Value, line: usize) -> Result<(), String> {
        let constant_index = self.add_constant(value);
        if constant_index > 255 {
            return Err("Too many constants in one chunk".to_string());
        }
        self.write_instruction_with_byte(OpCode::OP_CONSTANT, constant_index as u8, line);
        Ok(())
    }

    /// Get the current instruction count
    pub fn count(&self) -> usize {
        self.code.len()
    }

    /// Get a byte at the given offset
    pub fn get_byte(&self, offset: usize) -> Option<u8> {
        self.code.get(offset).copied()
    }

    /// Get a 2-byte operand at the given offset (big-endian)
    pub fn get_short(&self, offset: usize) -> Option<u16> {
        if offset + 1 < self.code.len() {
            let high = self.code[offset] as u16;
            let low = self.code[offset + 1] as u16;
            Some((high << 8) | low)
        } else {
            None
        }
    }

    /// Get the line number for a given instruction offset
    pub fn get_line(&self, offset: usize) -> Option<usize> {
        self.lines.get(offset).copied()
    }

    /// Patch a 2-byte jump instruction at the given offset
    pub fn patch_jump(&mut self, offset: usize) -> Result<(), String> {
        let jump_distance = self.code.len() - offset - 3; // -3 for the 3-byte jump instruction
        
        if jump_distance > u16::MAX as usize {
            return Err("Jump distance too large".to_string());
        }

        let jump_distance = jump_distance as u16;
        self.code[offset + 1] = (jump_distance >> 8) as u8;  // High byte
        self.code[offset + 2] = jump_distance as u8;         // Low byte
        
        Ok(())
    }

    /// Emit a loop instruction that jumps back to the given offset
    pub fn emit_loop(&mut self, loop_start: usize, line: usize) -> Result<(), String> {
        let offset = self.code.len() + 3; // +3 for the loop instruction itself
        let jump_distance = offset - loop_start;
        
        if jump_distance > u16::MAX as usize {
            return Err("Loop body too large".to_string());
        }

        self.write_instruction_with_short(OpCode::OP_LOOP, jump_distance as u16, line);
        Ok(())
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

/// Disassembler for debugging bytecode chunks
pub struct Disassembler;

impl Disassembler {
    /// Create a new disassembler
    pub fn new() -> Self {
        Disassembler
    }

    /// Disassemble an entire chunk
    pub fn disassemble_chunk(&self, chunk: &Chunk, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < chunk.code.len() {
            offset = self.disassemble_instruction(chunk, offset);
        }
    }

    /// Disassemble a single instruction and return the next offset
    pub fn disassemble_instruction(&self, chunk: &Chunk, offset: usize) -> usize {
        print!("{:04} ", offset);

        // Print line number
        if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
            print!("   | ");
        } else {
            print!("{:4} ", chunk.get_line(offset).unwrap_or(0));
        }

        let instruction_byte = chunk.get_byte(offset).unwrap_or(0);
        let opcode = OpCode::from_byte(instruction_byte);

        match opcode {
            Some(op) => self.disassemble_opcode(chunk, op, offset),
            None => {
                println!("Unknown opcode {}", instruction_byte);
                offset + 1
            }
        }
    }

    /// Disassemble a specific opcode
    fn disassemble_opcode(&self, chunk: &Chunk, opcode: OpCode, offset: usize) -> usize {
        match opcode {
            // Instructions with no operands
            OpCode::OP_NIL | OpCode::OP_TRUE | OpCode::OP_FALSE |
            OpCode::OP_POP | OpCode::OP_EQUAL | OpCode::OP_GREATER |
            OpCode::OP_LESS | OpCode::OP_ADD | OpCode::OP_SUBTRACT |
            OpCode::OP_MULTIPLY | OpCode::OP_DIVIDE | OpCode::OP_NOT |
            OpCode::OP_NEGATE | OpCode::OP_PRINT | OpCode::OP_RETURN |
            OpCode::OP_CONS | OpCode::OP_CAR | OpCode::OP_CDR |
            OpCode::OP_MAKE_HASHTABLE => {
                self.simple_instruction(opcode.name(), offset)
            }

            // Instructions with constant operand
            OpCode::OP_CONSTANT => {
                self.constant_instruction("OP_CONSTANT", chunk, offset)
            }

            // Instructions with byte operand
            OpCode::OP_GET_LOCAL | OpCode::OP_SET_LOCAL |
            OpCode::OP_GET_UPVALUE | OpCode::OP_SET_UPVALUE |
            OpCode::OP_CALL | OpCode::OP_CLOSE_UPVALUE |
            OpCode::OP_VECTOR => {
                self.byte_instruction(opcode.name(), chunk, offset)
            }

            // Instructions with global name operand
            OpCode::OP_GET_GLOBAL | OpCode::OP_DEFINE_GLOBAL |
            OpCode::OP_SET_GLOBAL => {
                self.constant_instruction(opcode.name(), chunk, offset)
            }

            // Jump instructions with 2-byte operand
            OpCode::OP_JUMP | OpCode::OP_JUMP_IF_FALSE => {
                self.jump_instruction(opcode.name(), 1, chunk, offset)
            }

            // Loop instruction (jumps backward)
            OpCode::OP_LOOP => {
                self.jump_instruction(opcode.name(), -1, chunk, offset)
            }

            // Closure instruction (variable operands)
            OpCode::OP_CLOSURE => {
                self.closure_instruction("OP_CLOSURE", chunk, offset)
            }
        }
    }

    /// Disassemble a simple instruction with no operands
    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    /// Disassemble an instruction with a constant operand
    fn constant_instruction(&self, name: &str, chunk: &Chunk, offset: usize) -> usize {
        let constant_index = chunk.get_byte(offset + 1).unwrap_or(0);
        print!("{:<16} {:4} '", name, constant_index);
        
        if let Some(constant) = chunk.constants.get(constant_index as usize) {
            self.print_value(constant);
        } else {
            print!("INVALID_CONSTANT");
        }
        
        println!("'");
        offset + 2
    }

    /// Disassemble an instruction with a byte operand
    fn byte_instruction(&self, name: &str, chunk: &Chunk, offset: usize) -> usize {
        let slot = chunk.get_byte(offset + 1).unwrap_or(0);
        println!("{:<16} {:4}", name, slot);
        offset + 2
    }

    /// Disassemble a jump instruction
    fn jump_instruction(&self, name: &str, sign: i32, chunk: &Chunk, offset: usize) -> usize {
        let jump = chunk.get_short(offset + 1).unwrap_or(0) as i32;
        let target = offset as i32 + 3 + sign * jump;
        println!("{:<16} {:4} -> {}", name, offset, target);
        offset + 3
    }

    /// Disassemble a closure instruction with variable operands
    fn closure_instruction(&self, name: &str, chunk: &Chunk, offset: usize) -> usize {
        let mut current_offset = offset + 1;
        let constant_index = chunk.get_byte(current_offset).unwrap_or(0);
        current_offset += 1;

        print!("{:<16} {:4} ", name, constant_index);
        
        if let Some(constant) = chunk.constants.get(constant_index as usize) {
            self.print_value(constant);
        } else {
            print!("INVALID_CONSTANT");
        }
        
        println!();

        // Print upvalue information
        if let Some(Value::Object(obj)) = chunk.constants.get(constant_index as usize) {
            if let Ok(obj_ref) = obj.try_borrow() {
                if let crate::object::Object::Function(function) = &*obj_ref {
                    for _ in 0..function.upvalue_count {
                        let is_local = chunk.get_byte(current_offset).unwrap_or(0);
                        let index = chunk.get_byte(current_offset + 1).unwrap_or(0);
                        current_offset += 2;
                        
                        println!("{:04}      |                     {} {}",
                                current_offset - 2,
                                if is_local != 0 { "local" } else { "upvalue" },
                                index);
                    }
                }
            }
        }

        current_offset
    }

    /// Print a value for disassembly output
    fn print_value(&self, value: &Value) {
        match value {
            Value::Number(n) => print!("{}", n),
            Value::Boolean(b) => print!("{}", if *b { "#t" } else { "#f" }),
            Value::Nil => print!("nil"),
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        crate::object::Object::String(s) => print!("\"{}\"", s),
                        crate::object::Object::Character(c) => print!("#\\{}", c),
                        crate::object::Object::Function(f) => print!("<fn {}>", f.name),
                        crate::object::Object::Closure(_) => print!("<closure>"),
                        crate::object::Object::Cons(_) => print!("<cons>"),
                        crate::object::Object::Vector(_) => print!("<vector>"),
                        crate::object::Object::Hashtable(_) => print!("<hashtable>"),
                        crate::object::Object::Upvalue(_) => print!("<upvalue>"),
                    }
                } else {
                    print!("<object>");
                }
            }
        }
    }

    /// Disassemble a range of instructions
    pub fn disassemble_range(&self, chunk: &Chunk, start: usize, end: usize) {
        let mut offset = start;
        while offset < end && offset < chunk.code.len() {
            offset = self.disassemble_instruction(chunk, offset);
        }
    }

    /// Get a formatted string representation of an instruction
    pub fn instruction_to_string(&self, chunk: &Chunk, offset: usize) -> String {
        let instruction_byte = chunk.get_byte(offset).unwrap_or(0);
        let opcode = OpCode::from_byte(instruction_byte);

        match opcode {
            Some(op) => {
                match op {
                    OpCode::OP_CONSTANT => {
                        let constant_index = chunk.get_byte(offset + 1).unwrap_or(0);
                        if let Some(constant) = chunk.constants.get(constant_index as usize) {
                            format!("OP_CONSTANT {} ({})", constant_index, self.value_to_string(constant))
                        } else {
                            format!("OP_CONSTANT {} (INVALID)", constant_index)
                        }
                    }
                    OpCode::OP_GET_LOCAL | OpCode::OP_SET_LOCAL |
                    OpCode::OP_GET_UPVALUE | OpCode::OP_SET_UPVALUE |
                    OpCode::OP_CALL | OpCode::OP_CLOSE_UPVALUE |
                    OpCode::OP_VECTOR => {
                        let operand = chunk.get_byte(offset + 1).unwrap_or(0);
                        format!("{} {}", op.name(), operand)
                    }
                    OpCode::OP_JUMP | OpCode::OP_JUMP_IF_FALSE | OpCode::OP_LOOP => {
                        let jump = chunk.get_short(offset + 1).unwrap_or(0);
                        format!("{} {}", op.name(), jump)
                    }
                    _ => op.name().to_string()
                }
            }
            None => format!("UNKNOWN {}", instruction_byte)
        }
    }

    /// Convert a value to a string representation
    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => if *b { "#t".to_string() } else { "#f".to_string() },
            Value::Nil => "nil".to_string(),
            Value::Object(obj) => {
                if let Ok(obj_ref) = obj.try_borrow() {
                    match &*obj_ref {
                        crate::object::Object::String(s) => format!("\"{}\"", s),
                        crate::object::Object::Character(c) => format!("#\\{}", c),
                        crate::object::Object::Function(f) => format!("<fn {}>", f.name),
                        crate::object::Object::Closure(_) => "<closure>".to_string(),
                        crate::object::Object::Cons(_) => "<cons>".to_string(),
                        crate::object::Object::Vector(_) => "<vector>".to_string(),
                        crate::object::Object::Hashtable(_) => "<hashtable>".to_string(),
                        crate::object::Object::Upvalue(_) => "<upvalue>".to_string(),
                    }
                } else {
                    "<object>".to_string()
                }
            }
        }
    }
}

impl Default for Disassembler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode_conversion() {
        // Test byte to opcode conversion
        assert_eq!(OpCode::from_byte(0), Some(OpCode::OP_CONSTANT));
        assert_eq!(OpCode::from_byte(1), Some(OpCode::OP_NIL));
        assert_eq!(OpCode::from_byte(33), Some(OpCode::OP_MAKE_HASHTABLE));
        assert_eq!(OpCode::from_byte(255), None);

        // Test opcode to byte conversion
        assert_eq!(OpCode::OP_CONSTANT.to_byte(), 0);
        assert_eq!(OpCode::OP_NIL.to_byte(), 1);
        assert_eq!(OpCode::OP_MAKE_HASHTABLE.to_byte(), 33);
    }

    #[test]
    fn test_instruction_sizes() {
        // Test instructions with no operands
        assert_eq!(OpCode::OP_NIL.instruction_size(), 1);
        assert_eq!(OpCode::OP_ADD.instruction_size(), 1);
        assert_eq!(OpCode::OP_RETURN.instruction_size(), 1);

        // Test instructions with 1-byte operand
        assert_eq!(OpCode::OP_CONSTANT.instruction_size(), 2);
        assert_eq!(OpCode::OP_GET_LOCAL.instruction_size(), 2);
        assert_eq!(OpCode::OP_CALL.instruction_size(), 2);

        // Test instructions with 2-byte operand
        assert_eq!(OpCode::OP_JUMP.instruction_size(), 3);
        assert_eq!(OpCode::OP_JUMP_IF_FALSE.instruction_size(), 3);
        assert_eq!(OpCode::OP_LOOP.instruction_size(), 3);
    }

    #[test]
    fn test_operand_info() {
        // Test operand detection
        assert!(!OpCode::OP_NIL.has_operands());
        assert!(OpCode::OP_CONSTANT.has_operands());
        assert!(OpCode::OP_JUMP.has_operands());

        // Test operand count
        assert_eq!(OpCode::OP_NIL.operand_count(), 0);
        assert_eq!(OpCode::OP_CONSTANT.operand_count(), 1);
        assert_eq!(OpCode::OP_JUMP.operand_count(), 2);
    }

    #[test]
    fn test_instruction_names() {
        assert_eq!(OpCode::OP_CONSTANT.name(), "OP_CONSTANT");
        assert_eq!(OpCode::OP_ADD.name(), "OP_ADD");
        assert_eq!(OpCode::OP_JUMP_IF_FALSE.name(), "OP_JUMP_IF_FALSE");
    }

    #[test]
    fn test_chunk_creation() {
        let chunk = Chunk::new();
        assert_eq!(chunk.count(), 0);
        assert!(chunk.code.is_empty());
        assert!(chunk.constants.is_empty());
        assert!(chunk.lines.is_empty());
    }

    #[test]
    fn test_chunk_write_byte() {
        let mut chunk = Chunk::new();
        chunk.write_byte(42, 1);
        
        assert_eq!(chunk.count(), 1);
        assert_eq!(chunk.get_byte(0), Some(42));
        assert_eq!(chunk.get_line(0), Some(1));
    }

    #[test]
    fn test_chunk_write_instruction() {
        let mut chunk = Chunk::new();
        chunk.write_instruction(OpCode::OP_NIL, 1);
        
        assert_eq!(chunk.count(), 1);
        assert_eq!(chunk.get_byte(0), Some(OpCode::OP_NIL.to_byte()));
        assert_eq!(chunk.get_line(0), Some(1));
    }

    #[test]
    fn test_chunk_write_instruction_with_byte() {
        let mut chunk = Chunk::new();
        chunk.write_instruction_with_byte(OpCode::OP_CONSTANT, 5, 1);
        
        assert_eq!(chunk.count(), 2);
        assert_eq!(chunk.get_byte(0), Some(OpCode::OP_CONSTANT.to_byte()));
        assert_eq!(chunk.get_byte(1), Some(5));
        assert_eq!(chunk.get_line(0), Some(1));
        assert_eq!(chunk.get_line(1), Some(1));
    }

    #[test]
    fn test_chunk_write_instruction_with_short() {
        let mut chunk = Chunk::new();
        chunk.write_instruction_with_short(OpCode::OP_JUMP, 0x1234, 1);
        
        assert_eq!(chunk.count(), 3);
        assert_eq!(chunk.get_byte(0), Some(OpCode::OP_JUMP.to_byte()));
        assert_eq!(chunk.get_short(1), Some(0x1234));
        assert_eq!(chunk.get_line(0), Some(1));
    }

    #[test]
    fn test_chunk_constants() {
        let mut chunk = Chunk::new();
        
        // Add constants
        let index1 = chunk.add_constant(Value::Number(42.0));
        let index2 = chunk.add_constant(Value::Boolean(true));
        
        assert_eq!(index1, 0);
        assert_eq!(index2, 1);
        assert_eq!(chunk.constants.len(), 2);
        
        // Write constant instruction
        chunk.write_constant(Value::string("hello".to_string()), 1).unwrap();
        assert_eq!(chunk.constants.len(), 3);
        assert_eq!(chunk.count(), 2); // OP_CONSTANT + index
    }

    #[test]
    fn test_chunk_jump_patching() {
        let mut chunk = Chunk::new();
        
        // Write a jump instruction with placeholder
        chunk.write_instruction_with_short(OpCode::OP_JUMP, 0, 1);
        let jump_offset = chunk.count() - 3;
        
        // Add some more instructions
        chunk.write_instruction(OpCode::OP_NIL, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);
        
        // Patch the jump
        chunk.patch_jump(jump_offset).unwrap();
        
        // The jump should now point to the instruction after the added ones
        let jump_distance = chunk.get_short(jump_offset + 1).unwrap();
        assert_eq!(jump_distance, 2); // 2 instructions added after jump
    }

    #[test]
    fn test_chunk_loop_emission() {
        let mut chunk = Chunk::new();
        
        // Mark loop start
        let loop_start = chunk.count();
        
        // Add some loop body instructions
        chunk.write_instruction(OpCode::OP_NIL, 1);
        chunk.write_instruction(OpCode::OP_POP, 1);
        
        // Emit loop instruction
        chunk.emit_loop(loop_start, 1).unwrap();
        
        // Check that loop instruction was added
        let loop_instruction_offset = chunk.count() - 3;
        assert_eq!(chunk.get_byte(loop_instruction_offset), Some(OpCode::OP_LOOP.to_byte()));
        
        // Check jump distance
        let jump_distance = chunk.get_short(loop_instruction_offset + 1).unwrap();
        assert_eq!(jump_distance, 5); // 2 body instructions + 3 for loop instruction
    }

    #[test]
    fn test_chunk_bounds_checking() {
        let chunk = Chunk::new();
        
        // Test out-of-bounds access
        assert_eq!(chunk.get_byte(0), None);
        assert_eq!(chunk.get_short(0), None);
        assert_eq!(chunk.get_line(0), None);
    }

    #[test]
    fn test_disassembler_creation() {
        let disassembler = Disassembler::new();
        let default_disassembler = Disassembler::default();
        
        // Just test that they can be created
        assert_eq!(std::mem::size_of_val(&disassembler), std::mem::size_of_val(&default_disassembler));
    }

    #[test]
    fn test_disassembler_simple_instructions() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add some simple instructions
        chunk.write_instruction(OpCode::OP_NIL, 1);
        chunk.write_instruction(OpCode::OP_TRUE, 1);
        chunk.write_instruction(OpCode::OP_FALSE, 1);
        chunk.write_instruction(OpCode::OP_RETURN, 1);
        
        // Test instruction string representation
        assert_eq!(disassembler.instruction_to_string(&chunk, 0), "OP_NIL");
        assert_eq!(disassembler.instruction_to_string(&chunk, 1), "OP_TRUE");
        assert_eq!(disassembler.instruction_to_string(&chunk, 2), "OP_FALSE");
        assert_eq!(disassembler.instruction_to_string(&chunk, 3), "OP_RETURN");
    }

    #[test]
    fn test_disassembler_constant_instruction() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add a constant instruction
        chunk.write_constant(Value::Number(42.0), 1).unwrap();
        
        let instruction_str = disassembler.instruction_to_string(&chunk, 0);
        assert!(instruction_str.contains("OP_CONSTANT"));
        assert!(instruction_str.contains("42"));
    }

    #[test]
    fn test_disassembler_byte_instruction() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add a byte instruction
        chunk.write_instruction_with_byte(OpCode::OP_GET_LOCAL, 5, 1);
        
        let instruction_str = disassembler.instruction_to_string(&chunk, 0);
        assert_eq!(instruction_str, "OP_GET_LOCAL 5");
    }

    #[test]
    fn test_disassembler_jump_instruction() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add a jump instruction
        chunk.write_instruction_with_short(OpCode::OP_JUMP, 100, 1);
        
        let instruction_str = disassembler.instruction_to_string(&chunk, 0);
        assert_eq!(instruction_str, "OP_JUMP 100");
    }

    #[test]
    fn test_disassembler_value_to_string() {
        let disassembler = Disassembler::new();
        
        // Test different value types
        assert_eq!(disassembler.value_to_string(&Value::Number(3.14)), "3.14");
        assert_eq!(disassembler.value_to_string(&Value::Boolean(true)), "#t");
        assert_eq!(disassembler.value_to_string(&Value::Boolean(false)), "#f");
        assert_eq!(disassembler.value_to_string(&Value::Nil), "nil");
        
        // Test string value
        let string_val = Value::string("hello".to_string());
        assert_eq!(disassembler.value_to_string(&string_val), "\"hello\"");
        
        // Test character value
        let char_val = Value::character('x');
        assert_eq!(disassembler.value_to_string(&char_val), "#\\x");
    }

    #[test]
    fn test_disassembler_unknown_opcode() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add an invalid opcode
        chunk.write_byte(255, 1);
        
        let instruction_str = disassembler.instruction_to_string(&chunk, 0);
        assert_eq!(instruction_str, "UNKNOWN 255");
    }

    #[test]
    fn test_disassemble_instruction_offset() {
        let disassembler = Disassembler::new();
        let mut chunk = Chunk::new();
        
        // Add various instructions
        chunk.write_instruction(OpCode::OP_NIL, 1);                    // offset 0, size 1
        chunk.write_instruction_with_byte(OpCode::OP_CONSTANT, 0, 1);  // offset 1, size 2
        chunk.write_instruction_with_short(OpCode::OP_JUMP, 10, 1);    // offset 3, size 3
        
        // Test that disassemble_instruction returns correct next offsets
        assert_eq!(disassembler.disassemble_instruction(&chunk, 0), 1);
        assert_eq!(disassembler.disassemble_instruction(&chunk, 1), 3);
        assert_eq!(disassembler.disassemble_instruction(&chunk, 3), 6);
    }
}