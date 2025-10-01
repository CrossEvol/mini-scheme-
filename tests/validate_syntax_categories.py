#!/usr/bin/env python3
"""
Validate that all syntax categories from the requirements are properly handled.
"""

import subprocess
import sys
import os

def test_expression(expr, description):
    """Test a single expression and return success status."""
    print(f"Testing {description}: {expr}")

    try:

        result = subprocess.run(
            ['cargo', 'run','--bin','mini_scheme'],
            input=f"{expr}\nquit\n",
            capture_output=True,
            text=True,
            timeout=5,
        )
        
        if result.returncode == 0 and "AST:" in result.stdout:
            print("  ✓ SUCCESS")
            return True
        else:
            print(f"  ✗ FAILED: {result.stdout}")
            return False
    except Exception as e:
        print(f"  ✗ ERROR: {e}")
        return False

def main():
    """Test all major syntax categories."""
    print("Validating MiniScheme Syntax Categories")
    print("=" * 50)
    
    test_cases = [
        # Literals (Requirement 1)
        ("42", "integer number"),
        ("3.14", "floating-point number"),
        ('"hello world"', "string literal"),
        ('#\\a', "character literal"),
        ('#\\space', "named character"),
        ('#t', "boolean true"),
        ('#f', "boolean false"),
        
        # Identifiers and Keywords (Requirement 2)
        ('hello', "simple identifier"),
        ('hello-world', "hyphenated identifier"),
        ('string->number', "identifier with arrow"),
        ('number?', "predicate identifier"),
        ('set!', "identifier with exclamation"),
        
        # Special Forms (Requirement 4)
        ('(define x 42)', "variable definition"),
        ('(lambda (x) x)', "lambda expression"),
        ('(if #t 1 2)', "if expression"),
        ('(cond (#t 1))', "cond expression"),
        ('(let ((x 1)) x)', "let expression"),
        ('(let* ((x 1)) x)', "let* expression"),
        ('(begin 1 2)', "begin expression"),
        
        # Quotation (Requirement 5)
        ("'x", "quote shorthand"),
        ('(quote x)', "quote form"),
        ('`x', "quasiquote shorthand"),
        (',x', "unquote shorthand"),
        (',@x', "unquote-splicing shorthand"),
        
        # Data Structures (Requirement 5)
        ("'(1 2 3)", "quoted list"),
        ('#(1 2 3)', "vector literal"),
        ('(list 1 2 3)', "list constructor"),
        
        # Function Calls
        ('(+ 1 2)', "function call"),
        ('(car (list 1 2))', "nested function call"),
        
        # Built-in Procedures (Requirement 2)
        ('(car (list 1 2))', "car procedure"),
        ('(cdr (list 1 2))', "cdr procedure"),
        ('(cons 1 2)', "cons procedure"),
        ('(display "hello")', "display procedure"),
        ('(number? 42)', "number? predicate"),
        ('(string? "hello")', "string? predicate"),
        ('(null? (list))', "null? predicate"),
    ]
    
    success_count = 0
    total_count = len(test_cases)
    
    for expr, description in test_cases:
        if test_expression(expr, description):
            success_count += 1
        print()
    
    print("=" * 50)
    print(f"RESULTS: {success_count}/{total_count} tests passed")
    
    if success_count == total_count:
        print("🎉 All syntax categories validated successfully!")
        return True
    else:
        print(f"❌ {total_count - success_count} tests failed")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)