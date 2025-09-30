# Syntax Elements

## Core Syntax
- `define` - Function and variable definition
- `lambda` - Anonymous function creation
- `if` - Conditional expression
- `cond` - Multi-way conditional expression
- `let` - Local binding
- `let*` - Sequential local binding
- `let loop` - Named let for iteration
- `let-values` - Binding values from multiple-value returns
- `call-with-values` - Calling procedures with multiple values

## Data Types
- Number (e.g., `42`)
- String (e.g., `"hello"`)
- Character (e.g., `#\a`)
- Boolean (e.g., `#t`, `#f`)
- List (e.g., `'(1 2 3)`, `'()`)
- Vector (e.g., `#(1 2 3)`)
- Hashtable (using `make-hashtable`, `hashtable-set!`, etc.)
- Null (represented as `'null`)

## Procedures and Operations
- `import` - Importing libraries
- `for-each` - Iterating over a list
- `car` - Get first element of a list
- `cdr` - Get rest of a list
- `error` - Raise an error
- `values` - Return multiple values
- `display` - Output value
- Predicates (question-mark functions):
  - `hashtable?` - Check if object is a hashtable
  - `string?` - Check if object is a string
  - `number?` - Check if object is a number
  - `boolean?` - Check if object is a boolean
  - `char?` - Check if object is a character
  - `char-numeric?` - Check if character is numeric
  - `char-whitespace?` - Check if character is whitespace
  - `null?` - Check if list is empty
  - `pair?` - Check if object is a pair
  - `eq?` - Check if objects are identical
  - `char=?` - Check if characters are equal
  - `string=?` - Check if strings are equal
- Type conversion:
  - `string->number`
  - `list->string`
  - `list->vector`
  - `vector->list`