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

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => {
            // No arguments - start REPL
            println!("MiniScheme Frontend REPL");
            println!("Type expressions to parse them, or 'quit' to exit.");
            println!(
                "Commands: :tokens (show tokens), :ast (show AST), :bytecode (show bytecode), :run (execute), :all (show all)"
            );
            repl();
        }
        2 => {
            // One argument - check if it's a help flag or process file
            let arg = &args[1];
            match arg.as_str() {
                "--help" | "-h" | "help" => {
                    print_usage(&args[0]);
                }
                _ => {
                    process_file(arg);
                }
            }
        }
        _ => {
            print_usage(&args[0]);
            process::exit(1);
        }
    }
}

fn print_usage(program_name: &str) {
    eprintln!("Usage:");
    eprintln!("  {} <scheme-file>    Process a Scheme file", program_name);
    eprintln!("  {}                  Start interactive REPL", program_name);
}

fn process_file(filename: &str) {
    println!("Processing file: {}", filename);

    // Read the input file
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    process_input(&input, true, true, true, true);
}

fn process_input(
    input: &str,
    show_tokens: bool,
    show_ast: bool,
    show_bytecode: bool,
    execute: bool,
) {
    // Create lexer and tokenize
    let mut lexer = Lexer::new(input);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexical error: {}", err);
            return;
        }
    };

    if show_tokens {
        println!("Tokens:");
        for token in &tokens {
            println!("  {:?}", token);
        }
    }

    // Create parser and parse
    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            return;
        }
    };

    if show_ast {
        if show_tokens {
            println!();
        }
        println!("AST:");
        for expr in &ast {
            println!("  {:?}", expr);
        }
    }

    // Compile to bytecode
    let mut compiler = Compiler::new_script();
    for expr in &ast {
        if let Err(err) = compiler.compile_expr(expr) {
            eprintln!("Compile error: {}", err);
            return;
        }
    }

    let function = compiler.end_compiler();

    if show_bytecode {
        if show_tokens || show_ast {
            println!();
        }
        println!("Bytecode:");
        let disassembler = bytecode::Disassembler::new();
        disassembler.disassemble_chunk(&function.chunk, "script");
    }

    // Execute if requested
    if execute {
        if show_tokens || show_ast || show_bytecode {
            println!();
        }

        let mut vm = VM::new();
        match vm.interpret(&function.chunk) {
            Ok(result) => {
                println!("Result: {:?}", result);
            }
            Err(err) => {
                eprintln!("Runtime error: {}", err);
            }
        }
    }

    if !show_tokens && !show_ast && !show_bytecode && !execute {
        println!("Compilation successful!");
    }
}

fn repl() {
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
                    _ => {
                        process_input(input, show_tokens, show_ast, show_bytecode, execute);
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
    println!("  :help      Show this help message");
    println!("  :tokens    Show tokens only");
    println!("  :ast       Show AST only");
    println!("  :bytecode  Show bytecode only");
    println!("  :run       Execute only (default)");
    println!("  :all       Show all phases (tokens, AST, bytecode, and execute)");
    println!("  :none      Compile only, no output");
    println!("  quit       Exit the REPL");
    println!();
    println!("Enter any Scheme expression to compile and execute it.");
}
