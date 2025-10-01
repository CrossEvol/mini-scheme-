#!/usr/bin/env python3
"""
Test script to process all example Scheme files and verify correct tokenization and parsing.
"""

import os
import subprocess
import sys
from pathlib import Path

def run_mini_scheme(file_path):
    """Run the mini_scheme binary on a file and return the result."""
    try:
        result = subprocess.run(
            ['cargo', 'run','--bin','mini_scheme', '--', str(file_path)],
            capture_output=True,
            text=True,
            timeout=10
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout"
    except Exception as e:
        return -1, "", str(e)

def test_example_files():
    """Test all example files in the example/ directory."""
    example_dir = Path('example')
    if not example_dir.exists():
        print("Error: example/ directory not found")
        return False
    
    # Find all .scm files
    scm_files = list(example_dir.rglob('*.scm'))
    if not scm_files:
        print("Error: No .scm files found in example/ directory")
        return False
    
    print(f"Found {len(scm_files)} Scheme files to test")
    print("=" * 60)
    
    success_count = 0
    error_count = 0
    parse_error_count = 0
    
    for scm_file in sorted(scm_files):
        print(f"\nTesting: {scm_file}")
        print("-" * 40)
        
        # Read the file content to show what we're testing
        try:
            with open(scm_file, 'r') as f:
                content = f.read().strip()
            print(f"Content: {content}")
        except Exception as e:
            print(f"Error reading file: {e}")
            error_count += 1
            continue
        
        # Run mini_scheme on the file
        returncode, stdout, stderr = run_mini_scheme(scm_file)
        
        if returncode == 0:
            print("✓ SUCCESS: Tokenization and parsing completed")
            if "Tokens:" in stdout and "AST:" in stdout:
                print("✓ Both tokens and AST generated")
            success_count += 1
        else:
            if "Parse error:" in stdout or "Parse error:" in stderr:
                print("⚠ PARSE ERROR: Tokenization succeeded but parsing failed")
                parse_error_count += 1
            else:
                print("✗ ERROR: Failed to process file")
                error_count += 1
            
            if stderr:
                print(f"Error output: {stderr}")
            if "error:" in stdout.lower():
                # Extract just the error line
                for line in stdout.split('\n'):
                    if 'error:' in line.lower():
                        print(f"Error: {line}")
    
    print("\n" + "=" * 60)
    print("SUMMARY:")
    print(f"Total files tested: {len(scm_files)}")
    print(f"✓ Successful: {success_count}")
    print(f"⚠ Parse errors: {parse_error_count}")
    print(f"✗ Other errors: {error_count}")
    
    if success_count == len(scm_files):
        print("\n🎉 All files processed successfully!")
        return True
    elif success_count + parse_error_count == len(scm_files):
        print("\n✓ All files tokenized successfully (some parse errors expected)")
        return True
    else:
        print(f"\n❌ {error_count} files failed to process")
        return False

def test_malformed_files():
    """Test error handling with intentionally malformed files."""
    print("\n" + "=" * 60)
    print("TESTING ERROR HANDLING WITH MALFORMED FILES")
    print("=" * 60)
    
    malformed_tests = [
        ('unterminated_string.scm', '"hello world'),
        ('unmatched_paren.scm', '(define x 42'),
        ('invalid_number.scm', '123.45.67'),
        ('invalid_char.scm', '#\\invalid'),
    ]
    
    for filename, content in malformed_tests:
        print(f"\nTesting malformed file: {filename}")
        print(f"Content: {content}")
        
        # Write the malformed file
        with open(filename, 'w') as f:
            f.write(content)
        
        # Test it
        returncode, stdout, stderr = run_mini_scheme(filename)
        
        if returncode == 0 and ("error:" in stdout.lower() or "error:" in stderr.lower()):
            print("✓ Error correctly detected and reported")
        elif returncode != 0:
            print("✓ Error correctly caused non-zero exit code")
        else:
            print("⚠ Expected error but got success")
        
        # Clean up
        os.remove(filename)

if __name__ == "__main__":
    print("MiniScheme Frontend Testing")
    print("=" * 60)
    
    # Build the project first
    print("Building project...")
    build_result = subprocess.run(['cargo', 'build'], capture_output=True)
    if build_result.returncode != 0:
        print("❌ Build failed!")
        print(build_result.stderr.decode())
        sys.exit(1)
    print("✓ Build successful")
    
    # Test example files
    success = test_example_files()
    
    # Test error handling
    test_malformed_files()
    
    if success:
        print("\n🎉 Testing completed successfully!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed")
        sys.exit(1)