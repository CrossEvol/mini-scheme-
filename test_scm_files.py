#!/usr/bin/env python3

import os
import subprocess
import sys
from pathlib import Path

def find_scm_files(directory):
    """Recursively find all .scm files in the given directory."""
    scm_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.scm'):
                scm_files.append(os.path.join(root, file))
    return scm_files

def run_scheme_script(file_path):
    """Run a scheme script using 'scheme --script' command."""
    try:
        result = subprocess.run(
            ['scheme', '--script', file_path],
            capture_output=True,
            text=True,
            timeout=30  # 30 second timeout to prevent hanging
        )
        return result.returncode == 0
    except subprocess.TimeoutExpired:
        return False
    except FileNotFoundError:
        print("Error: 'scheme' command not found. Please make sure the scheme interpreter is installed and in your PATH.")
        sys.exit(1)
    except Exception:
        return False

def main():
    example_dir = 'example'
    scm_files = find_scm_files(example_dir)
    
    total_files = len(scm_files)
    passed_count = 0
    failed_files = []
    
    print(f"Testing {total_files} .scm files in {example_dir}/")
    
    for i, file_path in enumerate(scm_files, 1):
        print(f"Testing ({i}/{total_files}): {file_path}")
        if run_scheme_script(file_path):
            passed_count += 1
            print(f"  PASS")
        else:
            failed_files.append(file_path)
            print(f"  FAIL")
    
    print("\n--- Test Results ---")
    print(f"Total: {total_files}")
    print(f"Passed: {passed_count}")
    print(f"Failed: {len(failed_files)}")
    
    if failed_files:
        print("\nFailed files:")
        for file_path in failed_files:
            print(f"  - {file_path}")
    
    return len(failed_files) == 0

if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)