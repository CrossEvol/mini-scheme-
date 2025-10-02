// Module declarations
mod ast;
mod bytecode;
mod compiler;
mod error;
mod lexer;
mod object;
mod parser;
mod token;
mod trace;
mod vm;

// Re-exports for convenience
pub use ast::Expr;
pub use compiler::{CompileError, Compiler};
pub use error::{LexError, ParseError};
pub use lexer::Lexer;
pub use object::Value;
pub use parser::Parser;
pub use token::{Token, TokenInfo};
pub use vm::{RuntimeError, VM};

use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;
use trace::{TraceConfig, Tracer};
use error::{ErrorContext, ErrorReporter, MiniSchemeError};

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse command line arguments
    let mut filename = None;
    let mut trace_compilation = false;
    let mut trace_execution = false;
    let mut show_tokens = false;
    let mut show_ast = false;
    let mut show_bytecode = false;
    let mut execute = true;
    let mut repl_mode = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--help" | "-h" => {
                print_usage(&args[0]);
                return;
            }
            "--trace-compilation" | "-tc" => {
                trace_compilation = true;
            }
            "--trace-execution" | "-te" => {
                trace_execution = true;
            }
            "--trace-all" | "-ta" => {
                trace_compilation = true;
                trace_execution = true;
            }
            "--tokens" => {
                show_tokens = true;
            }
            "--ast" => {
                show_ast = true;
            }
            "--bytecode" => {
                show_bytecode = true;
            }
            "--no-execute" => {
                execute = false;
            }
            "--all" => {
                show_tokens = true;
                show_ast = true;
                show_bytecode = true;
                execute = true;
            }
            "--repl" => {
                repl_mode = true;
            }
            arg if !arg.starts_with('-') => {
                if filename.is_none() {
                    filename = Some(arg.to_string());
                } else {
                    eprintln!("Error: Multiple files specified");
                    print_usage(&args[0]);
                    process::exit(1);
                }
            }
            _ => {
                eprintln!("Error: Unknown option '{}'", args[i]);
                print_usage(&args[0]);
                process::exit(1);
            }
        }
        i += 1;
    }

    // Create trace configuration
    let mut trace_config = TraceConfig::default();
    trace_config.compilation = trace_compilation;
    trace_config.execution = trace_execution;
    trace_config.stack_operations = trace_execution;
    trace_config.upvalue_operations = trace_execution;
    trace_config.function_calls = trace_execution;
    trace_config.variable_access = trace_execution;

    if repl_mode || filename.is_none() {
        // Start REPL
        println!("MiniScheme Bytecode Compiler REPL");
        println!("Type expressions to compile and execute them, or 'quit' to exit.");
        println!("Commands: :tokens, :ast, :bytecode, :run, :all, :trace-on, :trace-off");
        repl_with_config(trace_config);
    } else {
        // Process file
        let filename = filename.unwrap();
        process_file_with_config(&filename, trace_config, show_tokens, show_ast, show_bytecode, execute);
    }
}

fn print_usage(program_name: &str) {
    eprintln!("MiniScheme Bytecode Compiler");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  {} [OPTIONS] [FILE]", program_name);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -h, --help              Show this help message");
    eprintln!("  --repl                  Start interactive REPL (default if no file)");
    eprintln!("  --tokens                Show tokenization output");
    eprintln!("  --ast                   Show AST output");
    eprintln!("  --bytecode              Show bytecode disassembly");
    eprintln!("  --no-execute            Compile only, don't execute");
    eprintln!("  --all                   Show all phases and execute");
    eprintln!("  -tc, --trace-compilation Enable compilation tracing");
    eprintln!("  -te, --trace-execution  Enable execution tracing");
    eprintln!("  -ta, --trace-all        Enable all tracing");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  {} program.scm          Compile and run program.scm", program_name);
    eprintln!("  {} --all program.scm    Show all phases for program.scm", program_name);
    eprintln!("  {} --trace-all prog.scm Trace compilation and execution", program_name);
    eprintln!("  {}                      Start REPL", program_name);
}

fn process_file_with_config(
    filename: &str, 
    trace_config: TraceConfig,
    show_tokens: bool,
    show_ast: bool, 
    show_bytecode: bool,
    execute: bool
) {
    println!("Processing file: {}", filename);

    // Read the input file
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            let error_reporter = ErrorReporter::new();
            let mini_scheme_error = MiniSchemeError::from(err);
            let context = ErrorContext::new(Some(filename.to_string()), 1, 1)
                .with_context("reading source file".to_string());
            error_reporter.report_error(&mini_scheme_error, Some(&context));
            process::exit(1);
        }
    };

    if let Err(error) = process_input_with_config_result(&input, Some(filename), trace_config, show_tokens, show_ast, show_bytecode, execute) {
        let error_reporter = ErrorReporter::new();
        error_reporter.report_error(&error, None);
        process::exit(1);
    }
}

fn process_input_with_config(
    input: &str,
    trace_config: TraceConfig,
    show_tokens: bool,
    show_ast: bool,
    show_bytecode: bool,
    execute: bool,
) {
    if let Err(error) = process_input_with_config_result(input, None, trace_config, show_tokens, show_ast, show_bytecode, execute) {
        let error_reporter = ErrorReporter::new();
        error_reporter.report_error(&error, None);
    }
}

fn process_input_with_config_result(
    input: &str,
    filename: Option<&str>,
    trace_config: TraceConfig,
    show_tokens: bool,
    show_ast: bool,
    show_bytecode: bool,
    execute: bool,
) -> Result<(), MiniSchemeError> {
    let input_lines: Vec<&str> = input.lines().collect();
    let error_reporter = ErrorReporter::new();

    // Create lexer and tokenize
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|err| {
        let context = ErrorContext::new(filename.map(|s| s.to_string()), err.line(), err.column())
            .with_source_line(input_lines.get(err.line().saturating_sub(1)).unwrap_or(&"").to_string())
            .with_context("tokenizing source code".to_string());
        error_reporter.report_error(&MiniSchemeError::from(err.clone()), Some(&context));
        err
    })?;

    if show_tokens {
        println!("=== TOKENS ===");
        for token in &tokens {
            println!("  {:?}", token);
        }
        println!();
    }

    // Create parser and parse
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|err| {
        let (line, column) = if let (Some(l), Some(c)) = (err.line(), err.column()) {
            (l, c)
        } else {
            (input_lines.len(), 1)
        };
        
        let context = ErrorContext::new(filename.map(|s| s.to_string()), line, column)
            .with_source_line(input_lines.get(line.saturating_sub(1)).unwrap_or(&"").to_string())
            .with_context("parsing source code".to_string());
        error_reporter.report_error(&MiniSchemeError::from(err.clone()), Some(&context));
        err
    })?;

    if show_ast {
        println!("=== AST ===");
        for expr in &ast {
            println!("  {:?}", expr);
        }
        println!();
    }

    // Compile to bytecode using the new compilation pipeline
    let function = if trace_config.compilation {
        let tracer = Tracer::new(trace_config.clone());
        Compiler::compile_ast_with_trace(&ast, tracer).map_err(|err| {
            let context = ErrorContext::new(filename.map(|s| s.to_string()), 1, 1)
                .with_context("compiling AST to bytecode".to_string());
            error_reporter.report_error(&MiniSchemeError::from(err.clone()), Some(&context));
            err
        })?
    } else {
        Compiler::compile_ast(&ast).map_err(|err| {
            let context = ErrorContext::new(filename.map(|s| s.to_string()), 1, 1)
                .with_context("compiling AST to bytecode".to_string());
            error_reporter.report_error(&MiniSchemeError::from(err.clone()), Some(&context));
            err
        })?
    };

    if show_bytecode {
        println!("=== BYTECODE ===");
        let disassembler = bytecode::Disassembler::new();
        disassembler.disassemble_chunk(&function.chunk, "script");
        println!();
    }

    // Execute if requested
    if execute {
        if show_tokens || show_ast || show_bytecode {
            println!("=== EXECUTION ===");
        }

        let mut vm = VM::new();
        
        // Set up tracing if requested
        if trace_config.execution {
            let tracer = Tracer::new(trace_config);
            vm.set_tracer(tracer);
        }

        vm.interpret(&function.chunk).map_err(|err| {
            let context = ErrorContext::new(filename.map(|s| s.to_string()), 1, 1)
                .with_context("executing bytecode".to_string());
            error_reporter.report_error(&MiniSchemeError::from(err.clone()), Some(&context));
            err
        }).map(|result| {
            println!("Result: {:?}", result);
        })?;
    }

    if !show_tokens && !show_ast && !show_bytecode && !execute {
        println!("Compilation successful!");
    }

    Ok(())
}

fn repl_with_config(mut trace_config: TraceConfig) {
    let mut show_tokens = false;
    let mut show_ast = false;
    let mut show_bytecode = false;
    let mut execute = true;

    loop {
        print!("mini-scheme> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => {
                // EOF reached
                println!("Goodbye!");
                break;
            }
            Ok(_) => {
                let input = input.trim();

                if input.is_empty() {
                    continue;
                }

                match input {
                    "quit" | "exit" | ":quit" | ":exit" => {
                        println!("Goodbye!");
                        break;
                    }
                    ":help" => {
                        print_repl_help();
                        continue;
                    }
                    ":tokens" => {
                        show_tokens = true;
                        show_ast = false;
                        show_bytecode = false;
                        execute = false;
                        println!("Mode: showing tokens only");
                        continue;
                    }
                    ":ast" => {
                        show_tokens = false;
                        show_ast = true;
                        show_bytecode = false;
                        execute = false;
                        println!("Mode: showing AST only");
                        continue;
                    }
                    ":bytecode" => {
                        show_tokens = false;
                        show_ast = false;
                        show_bytecode = true;
                        execute = false;
                        println!("Mode: showing bytecode only");
                        continue;
                    }
                    ":run" => {
                        show_tokens = false;
                        show_ast = false;
                        show_bytecode = false;
                        execute = true;
                        println!("Mode: execute only (default)");
                        continue;
                    }
                    ":all" => {
                        show_tokens = true;
                        show_ast = true;
                        show_bytecode = true;
                        execute = true;
                        println!("Mode: showing all phases");
                        continue;
                    }
                    ":none" => {
                        show_tokens = false;
                        show_ast = false;
                        show_bytecode = false;
                        execute = false;
                        println!("Mode: compile only (no output)");
                        continue;
                    }
                    ":trace-on" => {
                        trace_config.compilation = true;
                        trace_config.execution = true;
                        trace_config.stack_operations = true;
                        trace_config.upvalue_operations = true;
                        trace_config.function_calls = true;
                        trace_config.variable_access = true;
                        println!("Tracing enabled");
                        continue;
                    }
                    ":trace-off" => {
                        trace_config.compilation = false;
                        trace_config.execution = false;
                        trace_config.stack_operations = false;
                        trace_config.upvalue_operations = false;
                        trace_config.function_calls = false;
                        trace_config.variable_access = false;
                        println!("Tracing disabled");
                        continue;
                    }
                    ":trace-compilation" => {
                        trace_config.compilation = !trace_config.compilation;
                        println!("Compilation tracing: {}", if trace_config.compilation { "on" } else { "off" });
                        continue;
                    }
                    ":trace-execution" => {
                        trace_config.execution = !trace_config.execution;
                        trace_config.stack_operations = trace_config.execution;
                        trace_config.upvalue_operations = trace_config.execution;
                        trace_config.function_calls = trace_config.execution;
                        trace_config.variable_access = trace_config.execution;
                        println!("Execution tracing: {}", if trace_config.execution { "on" } else { "off" });
                        continue;
                    }
                    _ => {
                        process_input_with_config(input, trace_config.clone(), show_tokens, show_ast, show_bytecode, execute);
                    }
                }
            }
            Err(err) => {
                eprintln!("Error reading input: {}", err);
                break;
            }
        }
    }
}

fn print_repl_help() {
    println!("REPL Commands:");
    println!("  :help               Show this help message");
    println!("  :tokens             Show tokens only");
    println!("  :ast                Show AST only");
    println!("  :bytecode           Show bytecode only");
    println!("  :run                Execute only (default)");
    println!("  :all                Show all phases (tokens, AST, bytecode, and execute)");
    println!("  :none               Compile only, no output");
    println!("  :trace-on           Enable all tracing");
    println!("  :trace-off          Disable all tracing");
    println!("  :trace-compilation  Toggle compilation tracing");
    println!("  :trace-execution    Toggle execution tracing");
    println!("  quit                Exit the REPL");
    println!();
    println!("Enter any Scheme expression to compile and execute it.");
}
