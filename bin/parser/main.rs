use mini_scheme::{Lexer, Parser, Expr};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        1 => {
            // No arguments - start REPL
            println!("MiniScheme Parser REPL");
            println!("Type expressions to parse them, or 'quit' to exit.");
            println!("Commands: :pretty (pretty print AST), :debug (debug AST), :tokens (show tokens too)");
            parser_repl();
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
    eprintln!("MiniScheme Parser - AST Generation Tool");
    eprintln!("Usage:");
    eprintln!("  {} <scheme-file>    Parse a Scheme file", program_name);
    eprintln!("  {}                  Start interactive parser REPL", program_name);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --help, -h         Show this help message");
}

fn process_file(filename: &str) {
    println!("Parsing file: {}", filename);
    
    // Read the input file
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    
    parse_input(&input, false, true, false);
}

fn parse_input(input: &str, show_tokens: bool, show_ast: bool, pretty_print: bool) {
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
        println!("Tokens ({} total):", tokens.len());
        for token in &tokens {
            println!("  {:?}", token);
        }
        println!();
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
        println!("AST ({} expressions):", ast.len());
        if pretty_print {
            for (i, expr) in ast.iter().enumerate() {
                println!("  {}. {}", i + 1, format_expr_pretty(expr, 2));
            }
        } else {
            for (i, expr) in ast.iter().enumerate() {
                println!("  {}. {:?}", i + 1, expr);
            }
        }
    }
    
    if !show_tokens && !show_ast {
        println!("Parse successful! {} expressions generated.", ast.len());
    }
}

fn format_expr_pretty(expr: &Expr, indent: usize) -> String {
    match expr {
        Expr::Number(n) => format!("Number({})", n),
        Expr::String(s) => format!("String(\"{}\")", s),
        Expr::Character(c) => format!("Character('{}')", c),
        Expr::Boolean(b) => format!("Boolean({})", b),
        Expr::Variable(name) => format!("Variable(\"{}\")", name),
        Expr::Quote(inner) => {
            format!("Quote(\n{}{}\n{})", 
                " ".repeat(indent + 2), 
                format_expr_pretty(inner, indent + 2),
                " ".repeat(indent))
        },
        Expr::Call(func, args) => {
            let mut result = format!("Call(\n{}func: {},", 
                " ".repeat(indent + 2),
                format_expr_pretty(func, indent + 2));
            if !args.is_empty() {
                result.push_str(&format!("\n{}args: [", " ".repeat(indent + 2)));
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { result.push_str(","); }
                    result.push_str(&format!("\n{}{}", 
                        " ".repeat(indent + 4),
                        format_expr_pretty(arg, indent + 4)));
                }
                result.push_str(&format!("\n{}]", " ".repeat(indent + 2)));
            }
            result.push_str(&format!("\n{})", " ".repeat(indent)));
            result
        },
        Expr::Define(def) => {
            format!("Define(\n{}name: \"{}\",\n{}value: {}\n{})",
                " ".repeat(indent + 2), def.name,
                " ".repeat(indent + 2), format_expr_pretty(&def.value, indent + 2),
                " ".repeat(indent))
        },
        Expr::Lambda(lambda) => {
            format!("Lambda(\n{}params: {:?},\n{}body: [\n{}{}\n{}]\n{})",
                " ".repeat(indent + 2), lambda.params,
                " ".repeat(indent + 2),
                " ".repeat(indent + 4),
                lambda.body.iter()
                    .map(|e| format_expr_pretty(e, indent + 4))
                    .collect::<Vec<_>>()
                    .join(&format!(",\n{}", " ".repeat(indent + 4))),
                " ".repeat(indent + 2),
                " ".repeat(indent))
        },
        Expr::If(if_expr) => {
            format!("If(\n{}condition: {},\n{}then: {},\n{}else: {}\n{})",
                " ".repeat(indent + 2), format_expr_pretty(&if_expr.condition, indent + 2),
                " ".repeat(indent + 2), format_expr_pretty(&if_expr.then_expr, indent + 2),
                " ".repeat(indent + 2), format_expr_pretty(&if_expr.else_expr, indent + 2),
                " ".repeat(indent))
        },
        Expr::Let(let_expr) => {
            let bindings = let_expr.bindings.iter()
                .map(|(name, value)| format!("(\"{}\", {})", name, format_expr_pretty(value, indent + 4)))
                .collect::<Vec<_>>()
                .join(&format!(",\n{}", " ".repeat(indent + 4)));
            format!("Let(\n{}bindings: [\n{}{}\n{}],\n{}body: [\n{}{}\n{}]\n{})",
                " ".repeat(indent + 2),
                " ".repeat(indent + 4), bindings, " ".repeat(indent + 2),
                " ".repeat(indent + 2),
                " ".repeat(indent + 4),
                let_expr.body.iter()
                    .map(|e| format_expr_pretty(e, indent + 4))
                    .collect::<Vec<_>>()
                    .join(&format!(",\n{}", " ".repeat(indent + 4))),
                " ".repeat(indent + 2),
                " ".repeat(indent))
        },
        _ => format!("{:?}", expr), // Fallback for other expression types
    }
}

fn parser_repl() {
    let mut show_tokens = false;
    let mut pretty_print = false;
    
    loop {
        print!("parser> ");
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
                        print_parser_help();
                        continue;
                    }
                    ":pretty" => {
                        pretty_print = true;
                        println!("Mode: pretty print AST");
                        continue;
                    }
                    ":debug" => {
                        pretty_print = false;
                        println!("Mode: debug AST format");
                        continue;
                    }
                    ":tokens" => {
                        show_tokens = !show_tokens;
                        println!("Mode: {} tokens", if show_tokens { "show" } else { "hide" });
                        continue;
                    }
                    _ => {
                        parse_input(input, show_tokens, true, pretty_print);
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

fn print_parser_help() {
    println!("Parser REPL Commands:");
    println!("  :help      Show this help message");
    println!("  :pretty    Use pretty-printed AST format");
    println!("  :debug     Use debug AST format (default)");
    println!("  :tokens    Toggle showing tokens before AST");
    println!("  quit       Exit the parser REPL");
    println!();
    println!("Enter any Scheme expression to parse it.");
    println!();
    println!("Examples:");
    println!("  42");
    println!("  (define x 42)");
    println!("  (lambda (x) (+ x 1))");
    println!("  (let ((x 5) (y 3)) (+ x y))");
}