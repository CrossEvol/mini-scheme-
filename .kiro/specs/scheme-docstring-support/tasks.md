# Implementation Plan

- [ ] 1. Extend AST structures to support docstrings
  - Add `docstring: Option<String>` field to `DefineExpr` struct in `src/ast.rs`
  - Add `docstring: Option<String>` field to `LambdaExpr` struct in `src/ast.rs`
  - Update struct constructors to handle the new docstring field
  - _Requirements: 4.1, 4.2, 4.4_

- [ ]* 1.1 Write unit tests for AST docstring extensions
  - Create tests for `DefineExpr` and `LambdaExpr` with and without docstrings
  - Test AST node creation and field access
  - _Requirements: 4.1, 4.2_

- [ ] 2. Implement docstring parsing logic in parser
  - Add helper method `is_string_literal()` to check if current token is a string
  - Add helper method `parse_docstring_and_body()` to extract docstrings and parse function bodies
  - Implement logic to detect string literals immediately after parameter lists
  - _Requirements: 1.1, 1.2, 3.1, 3.2_

- [ ] 2.1 Update parse_define method for docstring support
  - Modify function definition parsing to detect and extract docstrings
  - Handle `(define (name params...) "docstring" body...)` syntax
  - Ensure backward compatibility with functions without docstrings
  - _Requirements: 1.1, 1.2, 5.1, 5.2_

- [ ] 2.2 Update parse_lambda method for docstring support
  - Modify lambda parsing to detect and extract docstrings
  - Handle `(lambda (params...) "docstring" body...)` syntax
  - Maintain existing lambda parsing behavior for functions without docstrings
  - _Requirements: 1.4, 5.1, 5.2_

- [ ]* 2.3 Write unit tests for docstring parsing
  - Test parsing functions with docstrings in both define and lambda forms
  - Test parsing functions without docstrings (backward compatibility)
  - Test edge cases like empty docstrings and multiple strings
  - _Requirements: 1.1, 1.2, 1.4, 5.1_

- [ ] 3. Add comprehensive error handling for docstring syntax
  - Implement error detection for functions with only docstrings but no body
  - Add error handling for malformed docstring syntax
  - Create clear error messages with suggestions for correct syntax
  - _Requirements: 6.1, 6.2, 6.3, 6.4_

- [ ]* 3.1 Write unit tests for docstring error handling
  - Test error cases like missing function body after docstring
  - Test malformed docstring syntax error reporting
  - Verify error messages provide helpful suggestions
  - _Requirements: 6.1, 6.2, 6.3, 6.4_

- [ ] 4. Update compiler to handle docstring fields
  - Modify `compile_define()` method to ignore docstring field during compilation
  - Modify `compile_lambda()` method to ignore docstring field during compilation
  - Ensure existing compilation behavior is unchanged for function bodies
  - _Requirements: 2.1, 2.2, 2.3, 5.3_

- [ ]* 4.1 Write unit tests for compiler docstring handling
  - Test that docstrings are ignored during compilation
  - Verify bytecode generation is identical for functions with and without docstrings
  - Test that function execution behavior is unchanged
  - _Requirements: 2.1, 2.2, 2.3_

- [ ] 5. Create integration tests with example files
  - Create test files with documented functions using both define and lambda forms
  - Test parsing and compilation of complete programs with docstrings
  - Verify execution results are identical with and without docstrings
  - _Requirements: 5.1, 5.2, 5.3_

- [ ] 5.1 Test backward compatibility with existing code
  - Run existing test suite to ensure no regressions
  - Test that existing Scheme files parse and execute correctly
  - Verify AST serialization/deserialization compatibility
  - _Requirements: 5.1, 5.2, 5.3_

- [ ] 6. Add support for docstring examples from specification
  - Test parsing of `(define (fn1) "simple function" "Hello, world")`
  - Test parsing of `(define (fn2 a b) "simple function" (+ a b))`
  - Verify these examples compile and execute correctly
  - _Requirements: 1.1, 1.2, 2.1, 2.2_