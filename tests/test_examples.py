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
            if file.endswith(".scm"):
                scm_files.append(os.path.join(root, file))
    return scm_files


def extract_expected_output(file_path):
    """Extract expected output from ;=> comment in the file."""
    try:
        with open(file_path, "r") as f:
            content = f.read()

        # Look for ;=> comment and collect multiline expected output
        lines = content.split("\n")
        expected_lines = []
        found_marker = False

        for line in lines:
            stripped = line.strip()
            if stripped.startswith(";=>"):
                found_marker = True
                expected_part = stripped[3:].strip()  # Remove ';=>' and whitespace
                if expected_part:
                    expected_lines.append(expected_part)
            elif (
                found_marker
                and stripped.startswith(";")
                and not stripped.startswith(";=>")
            ):
                # Continue collecting multiline expected output
                expected_part = stripped[1:].strip()  # Remove ';' and whitespace
                if expected_part:
                    expected_lines.append(expected_part)
            elif found_marker and not stripped.startswith(";"):
                # End of expected output block
                break

        if found_marker:
            if expected_lines:
                return "\n".join(expected_lines)
            else:
                return ""  # Empty expected output
        return None
    except Exception:
        return None


def run_mini_scheme(file_path):
    """Run the mini_scheme binary on a file and return the result."""
    try:
        # Redirect stderr to suppress compilation warnings
        with open(os.devnull, 'w') as devnull:
            result = subprocess.run(
                ["cargo", "run", "--bin", "mini_scheme", "--", str(file_path)],
                stdout=subprocess.PIPE,
                stderr=devnull,
                text=True,
                timeout=30,
            )

        if result.returncode == 0:
            output = result.stdout.strip() if result.stdout else ""
            return True, output
        else:
            # For errors, we need to capture stderr differently since we redirected it
            # Let's run again with stderr captured for error cases
            error_result = subprocess.run(
                ["cargo", "run", "--bin", "mini_scheme", "--", str(file_path)],
                capture_output=True,
                text=True,
                timeout=30,
            )
            
            if error_result.stderr:
                # Look for actual error messages in stderr
                lines = error_result.stderr.split('\n')
                for line in lines:
                    if 'Error in' in line or 'error:' in line.lower():
                        return False, line.strip()
                return False, "Execution failed"
            else:
                return False, "Unknown error"
    except subprocess.TimeoutExpired:
        return False, "Timeout"
    except FileNotFoundError:
        print(
            "Error: 'cargo' command not found. Please make sure Rust and Cargo are installed and in your PATH."
        )
        sys.exit(1)
    except Exception as e:
        return False, str(e)


def test_scheme_file(file_path):
    """Test a single scheme file against its expected output."""
    expected = extract_expected_output(file_path)
    success, actual = run_mini_scheme(file_path)

    if not success:
        return False, f"Execution failed: {actual}"

    if expected is None:
        return False, "No expected output found (missing ;=> comment)"

    # Handle empty expected output (files that should produce no output)
    if expected == "":
        if actual == "":
            return True, "No output (as expected)"
        else:
            return False, f"Expected no output, got: {actual}"

    # Compare actual vs expected
    if actual == expected:
        return True, f"Output matches: {actual}"
    else:
        return False, f"Expected: {expected}, Got: {actual}"


def main():
    example_dir = "example"
    scm_files = find_scm_files(example_dir)

    total_files = len(scm_files)
    passed_count = 0
    failed_tests = []

    print(f"Testing {total_files} .scm files in {example_dir}/")

    for i, file_path in enumerate(scm_files, 1):
        print(f"Testing ({i}/{total_files}): {file_path}")
        success, message = test_scheme_file(file_path)

        if success:
            passed_count += 1
            print(f"  PASS: {message}")
        else:
            failed_tests.append((file_path, message))
            print(f"  FAIL: {message}")

    print("\n--- Test Results ---")
    print(f"Total: {total_files}")
    print(f"Passed: {passed_count}")
    print(f"Failed: {len(failed_tests)}")

    if failed_tests:
        print("\nFailed tests:")
        for file_path, message in failed_tests:
            print(f"  - {file_path}: {message}")

    return len(failed_tests) == 0


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
