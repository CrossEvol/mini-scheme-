# Implementation Plan

- [x] 1. Remove MultipleValues type and update Value enum
  - Remove `MultipleValues(Vec<Value>)` variant from `Value` enum in `src/object.rs`
  - Remove all associated methods: `multiple_values()`, `is_multiple_values()`, `as_multiple_values()`
  - Update `Display` and `PartialEq` implementations to remove MultipleValues handling
  - Clean up any existing MultipleValues usage in VM and compiler
  - _Requirements: 3.5, 3.6_

- [ ] 2. Add new bytecode instructions for multiple values
  - [ ] 2.1 Add OP_RETURN_VALUES and OP_CALL_WITH_VALUES to OpCode enum
    - Add `OP_RETURN_VALUES = 46` to handle multiple return values
    - Add `OP_CALL_WITH_VALUES = 47` to coordinate producer/consumer calls
    - Update `from_byte()`, `to_byte()`, and `name()` methods
    - _Requirements: 3.1, 3.2_

  - [ ] 2.2 Update instruction size and operand handling
    - Set `OP_RETURN_VALUES` instruction size to 2 bytes (opcode + value count)
    - Set `OP_CALL_WITH_VALUES` instruction size to 1 byte (no operands)
    - Update `instruction_size()`, `has_operands()`, and `operand_count()` methods
    - _Requirements: 3.1, 3.2_

  - [ ] 2.3 Update disassembler for new instructions
    - Add disassembly support for `OP_RETURN_VALUES` with value count display
    - Add disassembly support for `OP_CALL_WITH_VALUES`
    - Update `disassemble_opcode()` and `instruction_to_string()` methods
    - _Requirements: 3.1, 3.2_

- [ ] 3. Extend VM with multiple values state tracking
  - [ ] 3.1 Add return count tracking to VM struct
    - Add `last_return_count: Option<usize>` field to track multiple return values
    - Add `in_multiple_value_context: bool` field for context validation
    - Initialize fields in `VM::new()` method
    - _Requirements: 3.3, 3.4_

  - [ ] 3.2 Implement OP_RETURN_VALUES instruction handler
    - Read value count operand from bytecode
    - Validate stack has sufficient values
    - Set `last_return_count` metadata
    - Return from current function keeping values on stack
    - _Requirements: 3.3, 1.1, 1.2, 1.3, 1.4_

  - [ ] 3.3 Implement OP_CALL_WITH_VALUES instruction handler
    - Pop consumer and producer closures from stack
    - Validate producer takes 0 arguments and consumer arity matches
    - Call producer function and capture return count
    - Call consumer with produced values as arguments
    - Reset return count metadata after consumer call
    - _Requirements: 3.4, 2.1, 2.2, 2.3, 2.4, 2.5_

  - [ ] 3.4 Add context validation for single-value contexts
    - Implement `validate_value_context()` method
    - Check for multiple values in arithmetic operations
    - Report appropriate errors for invalid contexts
    - _Requirements: 4.1, 4.2, 4.4_

- [ ] 4. Update built-in values function implementation
  - [ ] 4.1 Modify values function to use OP_RETURN_VALUES
    - Change `builtin_values()` to generate `OP_RETURN_VALUES` instruction
    - Remove creation of `MultipleValues` composite type
    - Handle zero, one, and multiple argument cases
    - _Requirements: 1.1, 1.2, 1.3, 1.4_

  - [ ]* 4.2 Write unit tests for values function
    - Test `(values)` returns zero values
    - Test `(values 1)` returns single value
    - Test `(values 1 2 3)` returns multiple values
    - Test values in different contexts
    - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [ ] 5. Implement call-with-values compilation
  - [ ] 5.1 Add call-with-values compilation to compiler
    - Implement `compile_call_with_values()` method in `Compiler`
    - Compile producer and consumer expressions
    - Generate `OP_CALL_WITH_VALUES` instruction
    - _Requirements: 3.2, 2.1, 2.2, 2.3, 2.4, 2.5_

  - [ ]* 5.2 Write unit tests for call-with-values compilation
    - Test bytecode generation for simple call-with-values expressions
    - Test producer and consumer lambda compilation
    - Test error cases for invalid producer/consumer types
    - _Requirements: 3.2, 5.1, 5.2, 5.3_

- [ ] 6. Implement let-values to call-with-values transformation
  - [ ] 6.1 Add let-values compilation transformation
    - Implement `compile_let_values()` method in `Compiler`
    - Transform let-values bindings to producer/consumer lambdas
    - Compile transformed call-with-values expression
    - Handle multiple bindings by nesting transformations
    - _Requirements: 3.2, 2.1, 2.2, 2.3, 2.4, 2.5_

  - [ ]* 6.2 Write unit tests for let-values transformation
    - Test simple let-values to call-with-values transformation
    - Test multiple variable bindings
    - Test nested let-values expressions
    - _Requirements: 3.2_

- [ ] 7. Add comprehensive error handling
  - [ ] 7.1 Define new runtime error types
    - Add `MultipleValuesInSingleContext` error variant
    - Add `ZeroValuesInSingleContext` error variant  
    - Add `ProducerArityMismatch` and `ConsumerArityMismatch` variants
    - Update error display messages
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

  - [ ] 7.2 Implement context validation in VM operations
    - Add validation to arithmetic operations (`OP_ADD`, `OP_SUBTRACT`, etc.)
    - Add validation to assignment operations
    - Add validation to function call contexts
    - _Requirements: 4.1, 4.2, 4.4, 5.1, 5.2, 5.3_

  - [ ]* 7.3 Write unit tests for error handling
    - Test multiple values in single-value arithmetic context
    - Test zero values in single-value context
    - Test producer/consumer arity mismatches
    - Test error message formatting
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

- [ ] 8. Integration testing and cleanup
  - [ ] 8.1 Create comprehensive integration tests
    - Test complete call-with-values expressions from parsing to execution
    - Test let-values expressions end-to-end
    - Test multiple values in various contexts
    - Test interaction with existing language features
    - _Requirements: 1.1, 1.2, 1.3, 1.4, 2.1, 2.2, 2.3, 2.4, 2.5_

  - [ ] 8.2 Update example files and documentation
    - Verify existing `example/core/call_with_values.scm` works correctly
    - Verify existing `example/operations/values.scm` works correctly
    - Add additional example files for edge cases
    - _Requirements: 4.5_

  - [ ] 8.3 Clean up any remaining MultipleValues references
    - Search codebase for any remaining MultipleValues usage
    - Update any missed references in comments or documentation
    - Ensure all tests pass with new implementation
    - _Requirements: 3.5, 3.6_

- [ ]* 9. Performance and optimization testing
  - [ ]* 9.1 Benchmark multiple values performance
    - Compare performance with previous MultipleValues implementation
    - Test stack usage and memory efficiency
    - Profile VM instruction execution overhead
    - _Requirements: 3.5, 3.6_

  - [ ]* 9.2 Optimize hot paths if needed
    - Optimize OP_RETURN_VALUES instruction execution
    - Optimize OP_CALL_WITH_VALUES coordination
    - Minimize context validation overhead
    - _Requirements: 3.3, 3.4_