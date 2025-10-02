use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use crate::bytecode::Chunk;

/// The fundamental Value type that represents all Scheme values
#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(Rc<RefCell<Object>>),
}

/// Object types for heap-allocated Scheme values
#[derive(Debug)]
pub enum Object {
    String(String),
    Character(char),
    Symbol(String),
    Cons(Cons),
    Vector(Vec<Value>),
    Hashtable(HashMap<String, Value>),
    Function(Function),
    Closure(Closure),
    Upvalue(Upvalue),
}

/// Cons cell - the fundamental building block for Scheme lists
#[derive(Debug, Clone)]
pub struct Cons {
    pub car: Value,
    pub cdr: Value,
}

/// Function object containing bytecode and metadata
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
    pub upvalue_count: usize,
}

/// Closure object that captures upvalues from its environment
#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

/// Upvalue for capturing variables from enclosing scopes
#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: UpvalueLocation,
}

/// Location of an upvalue - either on the stack (open) or closed on the heap
#[derive(Debug, Clone)]
pub enum UpvalueLocation {
    Stack(usize),      // Points to stack slot (open upvalue)
    Closed(Value),     // Contains the closed value (closed upvalue)
}



impl Cons {
    /// Create a new cons cell
    pub fn new(car: Value, cdr: Value) -> Self {
        Cons { car, cdr }
    }
}

impl Function {
    /// Create a new function object
    pub fn new(name: String, arity: usize) -> Self {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }

    /// Create a new function with a specific chunk
    pub fn with_chunk(name: String, arity: usize, chunk: Chunk) -> Self {
        Function {
            name,
            arity,
            chunk,
            upvalue_count: 0,
        }
    }

    /// Check if this function can accept the given number of arguments
    pub fn check_arity(&self, arg_count: usize) -> bool {
        self.arity == arg_count
    }

    /// Get the function's name for debugging
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the function's arity
    pub fn arity(&self) -> usize {
        self.arity
    }
}

impl Closure {
    /// Create a new closure from a function
    pub fn new(function: Rc<Function>) -> Self {
        let upvalue_count = function.upvalue_count;
        Closure {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
        }
    }
}

impl Upvalue {
    /// Create a new open upvalue pointing to a stack slot
    pub fn new_open(stack_slot: usize) -> Self {
        Upvalue {
            location: UpvalueLocation::Stack(stack_slot),
        }
    }

    /// Create a new closed upvalue containing a value
    pub fn new_closed(value: Value) -> Self {
        Upvalue {
            location: UpvalueLocation::Closed(value),
        }
    }

    /// Check if this upvalue is closed
    pub fn is_closed(&self) -> bool {
        matches!(self.location, UpvalueLocation::Closed(_))
    }

    /// Close this upvalue with the given value
    pub fn close(&mut self, value: Value) {
        self.location = UpvalueLocation::Closed(value);
    }
}

impl Value {
    /// Create a new string value
    pub fn string(s: String) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::String(s))))
    }

    /// Create a new character value
    pub fn character(c: char) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Character(c))))
    }

    /// Create a new symbol value
    pub fn symbol(name: String) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Symbol(name))))
    }

    /// Create a new cons cell value
    pub fn cons(car: Value, cdr: Value) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Cons(Cons::new(car, cdr)))))
    }

    /// Create a new vector value
    pub fn vector(elements: Vec<Value>) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Vector(elements))))
    }

    /// Create a new hashtable value
    pub fn hashtable(map: HashMap<String, Value>) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Hashtable(map))))
    }

    /// Create a new function value
    pub fn function(func: Function) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Function(func))))
    }

    /// Create a new closure value
    pub fn closure(closure: Closure) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Closure(closure))))
    }

    // Type checking methods
    
    /// Check if this value is a number
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    /// Check if this value is a boolean
    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    /// Check if this value is nil
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Check if this value is a string
    pub fn is_string(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::String(_)),
            _ => false,
        }
    }

    /// Check if this value is a character
    pub fn is_character(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Character(_)),
            _ => false,
        }
    }

    /// Check if this value is a symbol
    pub fn is_symbol(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Symbol(_)),
            _ => false,
        }
    }

    /// Check if this value is a cons cell (pair)
    pub fn is_cons(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Cons(_)),
            _ => false,
        }
    }

    /// Check if this value is a vector
    pub fn is_vector(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Vector(_)),
            _ => false,
        }
    }

    /// Check if this value is a hashtable
    pub fn is_hashtable(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Hashtable(_)),
            _ => false,
        }
    }

    /// Check if this value is a function
    pub fn is_function(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Function(_)),
            _ => false,
        }
    }

    /// Check if this value is a closure
    pub fn is_closure(&self) -> bool {
        match self {
            Value::Object(obj) => matches!(*obj.borrow(), Object::Closure(_)),
            _ => false,
        }
    }

    /// Check if this value is callable (function or closure)
    pub fn is_callable(&self) -> bool {
        self.is_function() || self.is_closure()
    }

    // Safe value extraction methods

    /// Extract number value, returns None if not a number
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    /// Extract boolean value, returns None if not a boolean
    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    /// Extract string value, returns None if not a string
    pub fn as_string(&self) -> Option<String> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::String(s) => Some(s.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract character value, returns None if not a character
    pub fn as_character(&self) -> Option<char> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Character(c) => Some(*c),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract symbol value, returns None if not a symbol
    pub fn as_symbol(&self) -> Option<String> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Symbol(s) => Some(s.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract cons cell, returns None if not a cons
    pub fn as_cons(&self) -> Option<Cons> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Cons(cons) => Some(cons.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract vector, returns None if not a vector
    pub fn as_vector(&self) -> Option<Vec<Value>> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Vector(vec) => Some(vec.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract hashtable, returns None if not a hashtable
    pub fn as_hashtable(&self) -> Option<HashMap<String, Value>> {
        match self {
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Hashtable(map) => Some(map.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Check if this value is truthy (everything except #f is truthy in Scheme)
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(false) => false,
            _ => true,
        }
    }

    /// Check if this value is falsy (only #f is falsy in Scheme)
    pub fn is_falsy(&self) -> bool {
        !self.is_truthy()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Object(a), Value::Object(b)) => {
                // For objects, we compare by reference (eq? semantics)
                Rc::ptr_eq(a, b)
            }
            _ => false,
        }
    }
}

impl Eq for Value {}

/// Display implementation for Value to show Scheme-like output
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                // Display integers without decimal point, floats with decimal
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Nil => write!(f, "()"),
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::String(s) => write!(f, "\"{}\"", s),
                    Object::Character(c) => write!(f, "#\\{}", c),
                    Object::Symbol(s) => write!(f, "{}", s),
                    Object::Cons(cons) => {
                        write!(f, "(")?;
                        self.display_list(f, cons)?;
                        write!(f, ")")
                    }
                    Object::Vector(vec) => {
                        write!(f, "#(")?;
                        for (i, val) in vec.iter().enumerate() {
                            if i > 0 { write!(f, " ")?; }
                            write!(f, "{}", val)?;
                        }
                        write!(f, ")")
                    }
                    Object::Hashtable(_) => write!(f, "#<hashtable>"),
                    Object::Function(func) => write!(f, "#<function:{}>", func.name),
                    Object::Closure(closure) => write!(f, "#<closure:{}>", closure.function.name),
                    Object::Upvalue(_) => write!(f, "#<upvalue>"),
                }
            }
        }
    }
}

impl Value {
    /// Helper method to display lists properly
    fn display_list(&self, f: &mut fmt::Formatter<'_>, cons: &Cons) -> fmt::Result {
        write!(f, "{}", cons.car)?;
        
        match &cons.cdr {
            Value::Nil => Ok(()),
            Value::Object(obj) => {
                match &*obj.borrow() {
                    Object::Cons(next_cons) => {
                        write!(f, " ")?;
                        self.display_list(f, next_cons)
                    }
                    _ => {
                        write!(f, " . {}", cons.cdr)
                    }
                }
            }
            _ => {
                write!(f, " . {}", cons.cdr)
            }
        }
    }
}

/// Implement equality comparison for values
impl Value {
    /// Scheme eq? predicate - reference equality for objects, value equality for immediates
    pub fn eq(&self, other: &Value) -> bool {
        self == other
    }

    /// Scheme equal? predicate - structural equality
    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Object(a), Value::Object(b)) => {
                match (&*a.borrow(), &*b.borrow()) {
                    (Object::String(s1), Object::String(s2)) => s1 == s2,
                    (Object::Character(c1), Object::Character(c2)) => c1 == c2,
                    (Object::Symbol(s1), Object::Symbol(s2)) => s1 == s2,
                    (Object::Cons(cons1), Object::Cons(cons2)) => {
                        cons1.car.equal(&cons2.car) && cons1.cdr.equal(&cons2.cdr)
                    }
                    (Object::Vector(v1), Object::Vector(v2)) => {
                        v1.len() == v2.len() && 
                        v1.iter().zip(v2.iter()).all(|(a, b)| a.equal(b))
                    }
                    _ => Rc::ptr_eq(a, b), // For other objects, fall back to reference equality
                }
            }
            _ => false,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_creation() {
        // Test immediate values
        let num = Value::Number(42.0);
        let bool_true = Value::Boolean(true);
        let bool_false = Value::Boolean(false);
        let nil = Value::Nil;

        assert!(num.is_number());
        assert!(bool_true.is_boolean());
        assert!(bool_false.is_boolean());
        assert!(nil.is_nil());

        // Test object values
        let string_val = Value::string("hello".to_string());
        let char_val = Value::character('a');
        let cons_val = Value::cons(Value::Number(1.0), Value::Nil);
        let vec_val = Value::vector(vec![Value::Number(1.0), Value::Number(2.0)]);
        let map = HashMap::new();
        let hash_val = Value::hashtable(map);

        assert!(string_val.is_string());
        assert!(char_val.is_character());
        assert!(cons_val.is_cons());
        assert!(vec_val.is_vector());
        assert!(hash_val.is_hashtable());
    }

    #[test]
    fn test_value_extraction() {
        // Test number extraction
        let num = Value::Number(42.5);
        assert_eq!(num.as_number(), Some(42.5));
        assert_eq!(Value::Nil.as_number(), None);

        // Test boolean extraction
        let bool_val = Value::Boolean(true);
        assert_eq!(bool_val.as_boolean(), Some(true));
        assert_eq!(Value::Nil.as_boolean(), None);

        // Test string extraction
        let string_val = Value::string("hello".to_string());
        assert_eq!(string_val.as_string(), Some("hello".to_string()));
        assert_eq!(Value::Nil.as_string(), None);

        // Test character extraction
        let char_val = Value::character('x');
        assert_eq!(char_val.as_character(), Some('x'));
        assert_eq!(Value::Nil.as_character(), None);
    }

    #[test]
    fn test_cons_operations() {
        let car = Value::Number(1.0);
        let cdr = Value::Number(2.0);
        let cons_val = Value::cons(car.clone(), cdr.clone());

        assert!(cons_val.is_cons());
        
        let extracted_cons = cons_val.as_cons().unwrap();
        assert_eq!(extracted_cons.car, car);
        assert_eq!(extracted_cons.cdr, cdr);
    }

    #[test]
    fn test_vector_operations() {
        let elements = vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)];
        let vec_val = Value::vector(elements.clone());

        assert!(vec_val.is_vector());
        assert_eq!(vec_val.as_vector(), Some(elements));
    }

    #[test]
    fn test_hashtable_operations() {
        let mut map = HashMap::new();
        map.insert("key1".to_string(), Value::Number(1.0));
        map.insert("key2".to_string(), Value::string("value".to_string()));
        
        let hash_val = Value::hashtable(map.clone());
        assert!(hash_val.is_hashtable());
        assert_eq!(hash_val.as_hashtable(), Some(map));
    }

    #[test]
    fn test_truthiness() {
        // Only #f is falsy in Scheme
        assert!(Value::Boolean(false).is_falsy());
        assert!(!Value::Boolean(false).is_truthy());

        // Everything else is truthy
        assert!(Value::Boolean(true).is_truthy());
        assert!(Value::Number(0.0).is_truthy());
        assert!(Value::Nil.is_truthy());
        assert!(Value::string("".to_string()).is_truthy());
    }

    #[test]
    fn test_equality() {
        // Test eq? (reference equality for objects)
        let num1 = Value::Number(42.0);
        let num2 = Value::Number(42.0);
        assert!(num1.eq(&num2));

        let str1 = Value::string("hello".to_string());
        let str2 = Value::string("hello".to_string());
        assert!(!str1.eq(&str2)); // Different object references

        // Test equal? (structural equality)
        assert!(num1.equal(&num2));
        assert!(str1.equal(&str2)); // Same content

        // Test cons equality
        let cons1 = Value::cons(Value::Number(1.0), Value::Number(2.0));
        let cons2 = Value::cons(Value::Number(1.0), Value::Number(2.0));
        assert!(cons1.equal(&cons2));

        // Test vector equality
        let vec1 = Value::vector(vec![Value::Number(1.0), Value::Number(2.0)]);
        let vec2 = Value::vector(vec![Value::Number(1.0), Value::Number(2.0)]);
        assert!(vec1.equal(&vec2));
    }

    #[test]
    fn test_function_and_closure() {
        let func = Function::new("test".to_string(), 2);
        let func_val = Value::function(func);
        assert!(func_val.is_function());
        assert!(func_val.is_callable());

        let closure = Closure::new(Rc::new(Function::new("test".to_string(), 1)));
        let closure_val = Value::closure(closure);
        assert!(closure_val.is_closure());
        assert!(closure_val.is_callable());
    }

    #[test]
    fn test_upvalue_operations() {
        // Test open upvalue
        let mut upvalue = Upvalue::new_open(5);
        assert!(!upvalue.is_closed());

        // Test closing upvalue
        upvalue.close(Value::Number(42.0));
        assert!(upvalue.is_closed());

        // Test closed upvalue creation
        let closed_upvalue = Upvalue::new_closed(Value::string("test".to_string()));
        assert!(closed_upvalue.is_closed());
    }
}