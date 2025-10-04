use mini_scheme::Lexer;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        1 => {
            // No arguments - start REPL
            println!("MiniScheme Lexer REPL");
            println!("Type expressions to tokenize them, or 'quit' to exit.");
            println!("Commands: :verbose (show detailed token info), :simple (show simple tokens)");
            lexer_repl();
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
    eprintln!("MiniScheme Lexer - Tokenization Tool");
    eprintln!("Usage:");
    eprintln!("  {} <scheme-file>    Tokenize a Scheme file", program_name);
    eprintln!("  {}                  Start interactive lexer REPL", program_name);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --help, -h         Show this help message");
}

fn process_file(filename: &str) {
    println!("Tokenizing file: {}", filename);
    
    // Read the input file
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    
    tokenize_input(&input, true);
}

fn tokenize_input(input: &str, verbose: bool) {
    // Create lexer and tokenize
    let mut lexer = Lexer::new(input);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexical error: {}", err);
            return;
        }
    };
    
    if verbose {
        println!("Tokens ({} total):", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!("  {}: {:?}", i + 1, token);
        }
    } else {
        println!("Tokens:");
        for token in &tokens {
            println!("  {:?}", token.token_type);
        }
    }
    
    println!("\nTokenization successful! {} tokens generated.", tokens.len());
}

fn lexer_repl() {
    let mut verbose = true;
    
    loop {
        print!("lexer> ");
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
                        print_lexer_help();
                        continue;
                    }
                    ":verbose" => {
                        verbose = true;
                        println!("Mode: verbose token info (with line/column details)");
                        continue;
                    }
                    ":simple" => {
                        verbose = false;
                        println!("Mode: simple token list");
                        continue;
                    }
                    _ => {
                        tokenize_input(input, verbose);
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

fn print_lexer_help() {
    println!("Lexer REPL Commands:");
    println!("  :help      Show this help message");
    println!("  :verbose   Show detailed token info with line/column (default)");
    println!("  :simple    Show simple token list");
    println!("  quit       Exit the lexer REPL");
    println!();
    println!("Enter any Scheme expression to tokenize it.");
    println!();
    println!("Examples:");
    println!("  42");
    println!("  \"hello world\"");
    println!("  (define x 42)");
    println!("  (lambda (x) (+ x 1))");
}