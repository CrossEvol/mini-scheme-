use mini_scheme::{Lexer, Parser, Compiler};
use mini_scheme::bytecode::Disassembler;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        1 => {
            // No arguments - start REPL
            println!("MiniScheme Compiler REPL");
            println!("Type expressions to compile them to bytecode, or 'quit' to exit.");
            println!("Commands: :trace (enable compilation tracing), :notrace (disable tracing)");
            compiler_repl();
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
    eprintln!("MiniScheme Compiler - Bytecode Generation Tool");
    eprintln!("Usage:");
    eprintln!("  {} <scheme-file>    Compile a Scheme file to bytecode", program_name);
    eprintln!("  {}                  Start interactive compiler REPL", program_name);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --help, -h         Show this help message");
}

fn process_file(filename: &str) {
    println!("Compiling file: {}", filename);
    
    // Read the input file
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    
    compile_input(&input, false);
}

fn compile_input(input: &str, trace_enabled: bool) {
    // Create lexer and tokenize
    let mut lexer = Lexer::new(input);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexical error: {}", err);
            return;
        }
    };
    
    // Create parser and parse
    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            return;
        }
    };
    
    // Create compiler and compile to bytecode
    let mut compiler = Compiler::new_script();
    if trace_enabled {
        compiler.enable_trace();
    }
    
    // Compile each expression in the AST
    for expr in &ast {
        if let Err(err) = compiler.compile_expr(expr) {
            eprintln!("Compilation error: {}", err);
            return;
        }
    }
    
    // Finish compilation
    let function = compiler.end_compiler();
    
    // Display the compiled bytecode
    println!("Compilation successful! Generated {} bytes of bytecode.", function.chunk.count());
    println!();
    
    let disassembler = Disassembler::new();
    disassembler.disassemble_chunk_with_functions(&function.chunk, "script");
}

fn compiler_repl() {
    let mut trace_enabled = false;
    
    loop {
        print!("compiler> ");
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
                        print_compiler_help();
                        continue;
                    }
                    ":trace" => {
                        trace_enabled = true;
                        println!("Mode: compilation tracing enabled");
                        continue;
                    }
                    ":notrace" => {
                        trace_enabled = false;
                        println!("Mode: compilation tracing disabled");
                        continue;
                    }
                    _ => {
                        compile_input(input, trace_enabled);
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

fn print_compiler_help() {
    println!("Compiler REPL Commands:");
    println!("  :help      Show this help message");
    println!("  :trace     Enable compilation tracing");
    println!("  :notrace   Disable compilation tracing (default)");
    println!("  quit       Exit the compiler REPL");
    println!();
    println!("Enter any Scheme expression to compile it to bytecode.");
    println!();
    println!("Examples:");
    println!("  42");
    println!("  (define x 42)");
    println!("  (lambda (x) (+ x 1))");
    println!("  (+ 1 2 3)");
}