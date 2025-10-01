#!/usr/bin/env python3
"""
Comprehensive test script to validate AST structure matches expected Scheme semantics.
"""

import subprocess
import sys
from pathlib import Path
import re

def run_mini_scheme_ast_only(file_path):
    """Run the mini_scheme binary on a file and return just the AST output."""
    try:
        result = subprocess.run(
            ['cargo', 'run','--bin','mini_scheme', '--', str(file_path)],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if result.returncode != 0:
            return None, result.stderr
        
        # Extract AST section from output
        lines = result.stdout.split('\n')
        ast_lines = []
        in_ast_section = False
        
        for line in lines:
            if line.strip() == "AST:":
                in_ast_section = True
                continue
            elif in_ast_section and line.strip():
                ast_lines.append(line.strip())
        
        return '\n'.join(ast_lines), None
    except Exception as e:
        return None, str(e)

def validate_ast_structure():
    """Validate AST structure for key syntax categories."""
    
    test_cases = [
        # Core syntax
        {
            'name': 'Variable definition',
            'file': 'example/core/define_var.scm',
            'expected_patterns': [
                r'Define\(DefineExpr.*name: "x".*value: Number\(42\.0\)',
            ]
        },
        {
            'name': 'Lambda expression',
            'file': 'example/core/lambda.scm',
            'expected_patterns': [
                r'Lambda\(LambdaExpr.*params: \["x"\].*body:.*Call\(Variable\("\+"\)',
            ]
        },
        {
            'name': 'If expression',
            'file': 'example/core/if.scm',
            'expected_patterns': [
                r'If\(IfExpr.*condition:.*Call\(Variable\(">".*then_expr:.*String\("yes".*else_expr:.*String\("no"',
            ]
        },
        {
            'name': 'Let expression',
            'file': 'example/core/let.scm',
            'expected_patterns': [
                r'Let\(LetExpr.*bindings:.*\("x", Number\(5\.0\).*\("y", Number\(3\.0\).*body:.*Call\(Variable\("\+"',
            ]
        },
        {
            'name': 'Cond expression',
            'file': 'example/core/cond.scm',
            'expected_patterns': [
                r'Cond\(CondExpr.*clauses:',
            ]
        },
        
        # Data types
        {
            'name': 'Number literal',
            'file': 'example/data_types/number.scm',
            'expected_patterns': [
                r'Number\(42\.0\)',
            ]
        },
        {
            'name': 'String literal',
            'file': 'example/data_types/string.scm',
            'expected_patterns': [
                r'String\("hello"\)',
            ]
        },
        {
            'name': 'Boolean literal',
            'file': 'example/data_types/boolean.scm',
            'expected_patterns': [
                r'Boolean\(true\)',
            ]
        },
        {
            'name': 'Character literal',
            'file': 'example/data_types/char.scm',
            'expected_patterns': [
                r'Character\(\'a\'\)',
            ]
        },
        
        # Operations
        {
            'name': 'Car operation',
            'file': 'example/operations/car.scm',
            'expected_patterns': [
                r'Call\(Variable\("car"\)',
                r'Quote\(',
            ]
        },
        {
            'name': 'Predicate operation',
            'file': 'example/operations/predicates/number_q.scm',
            'expected_patterns': [
                r'Call\(Variable\("number\?"\)',
                r'Number\(42\.0\)',
            ]
        },
    ]
    
    print("VALIDATING AST STRUCTURE")
    print("=" * 60)
    
    success_count = 0
    total_count = len(test_cases)
    
    for test_case in test_cases:
        print(f"\nTesting: {test_case['name']}")
        print(f"File: {test_case['file']}")
        
        # Read file content
        try:
            with open(test_case['file'], 'r') as f:
                content = f.read().strip()
            print(f"Content: {content}")
        except Exception as e:
            print(f"❌ Error reading file: {e}")
            continue
        
        # Get AST
        ast_output, error = run_mini_scheme_ast_only(test_case['file'])
        if error:
            print(f"❌ Error getting AST: {error}")
            continue
        
        if not ast_output:
            print("❌ No AST output received")
            continue
        
        print(f"AST: {ast_output}")
        
        # Validate patterns
        all_patterns_match = True
        for pattern in test_case['expected_patterns']:
            if not re.search(pattern, ast_output, re.DOTALL):
                print(f"❌ Pattern not found: {pattern}")
                all_patterns_match = False
            else:
                print(f"✓ Pattern matched: {pattern}")
        
        if all_patterns_match:
            print("✓ All patterns matched - AST structure correct")
            success_count += 1
        else:
            print("❌ Some patterns failed - AST structure incorrect")
    
    print("\n" + "=" * 60)
    print("AST VALIDATION SUMMARY:")
    print(f"✓ Successful: {success_count}/{total_count}")
    
    return success_count == total_count

def test_syntax_coverage():
    """Test that all syntax categories from SYNTAX_CATEGORIES.md are covered."""
    print("\n" + "=" * 60)
    print("TESTING SYNTAX COVERAGE")
    print("=" * 60)
    
    # Read syntax categories
    try:
        with open('example/SYNTAX_CATEGORIES.md', 'r') as f:
            _ = f.read()
    except Exception as e:
        print(f"❌ Error reading SYNTAX_CATEGORIES.md: {e}")
        return False
    
    # Extract syntax elements
    core_syntax = [
        'define', 'lambda', 'if', 'cond', 'let', 'let*', 'let loop', 
        'let-values', 'call-with-values'
    ]
    
    _ = [
        'Number', 'String', 'Character', 'Boolean', 'List', 'Vector'
    ]
    
    operations = [
        'import', 'for-each', 'car', 'cdr', 'error', 'values', 'display',
        'hashtable?', 'string?', 'number?', 'boolean?', 'char?',
        'char-numeric?', 'char-whitespace?', 'null?', 'pair?', 'eq?',
        'char=?', 'string=?', 'string->number', 'list->string',
        'list->vector', 'vector->list'
    ]
    
    # Find all .scm files
    scm_files = list(Path('example').rglob('*.scm'))
    
    # Check coverage
    covered_syntax = set()
    
    for scm_file in scm_files:
        try:
            with open(scm_file, 'r') as f:
                content = f.read()
            
            # Check for syntax elements in file content
            for syntax in core_syntax + operations:
                if syntax in content:
                    covered_syntax.add(syntax)
        except Exception as e:
            print(f"Warning: Could not read {scm_file}: {e}")
    
    print(f"Found {len(scm_files)} example files")
    print(f"Covered {len(covered_syntax)} syntax elements")
    
    # Report coverage
    all_syntax = set(core_syntax + operations)
    missing_syntax = all_syntax - covered_syntax
    
    if missing_syntax:
        print(f"❌ Missing syntax coverage: {sorted(missing_syntax)}")
        return False
    else:
        print("✓ All syntax elements have example coverage")
        return True

if __name__ == "__main__":
    print("MiniScheme AST Structure Validation")
    print("=" * 60)
    
    # Build the project first
    print("Building project...")
    build_result = subprocess.run(['cargo', 'build'], capture_output=True)
    if build_result.returncode != 0:
        print("❌ Build failed!")
        print(build_result.stderr.decode())
        sys.exit(1)
    print("✓ Build successful")
    
    # Validate AST structure
    ast_valid = validate_ast_structure()
    
    # Test syntax coverage
    coverage_complete = test_syntax_coverage()
    
    if ast_valid and coverage_complete:
        print("\n🎉 All AST validation tests passed!")
        sys.exit(0)
    else:
        print("\n❌ Some AST validation tests failed")
        sys.exit(1)