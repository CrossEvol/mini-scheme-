# Build the parser binary                                                  
.PHONY: parser                                                             
parser:                                                                    
	cargo build --bin parser                                               
                                                                           
# Build the compiler binary                                                
.PHONY: compiler                                                           
compiler:                                                                  
	cargo build --bin compiler                                             
                                                                           
# Build the main mini_scheme binary                                        
.PHONY: mini_scheme                                                        
mini_scheme:                                                               
	cargo build --bin mini_scheme                                          
                                                                           
# Run tests                                                                
.PHONY: test                                                               
test: test-examples test-scm test-ast test-syntax                          
                                                                           
# Test example files with the frontend                                     
.PHONY: test-examples                                                      
test-examples:                                                             
	python3 tests/test_examples.py                                         
                                                                           
# Test .scm files using external scheme interpreter                        
.PHONY: test-scm                                                           
test-scm:                                                                  
	python3 tests/test_scm_files.py                                        
                                                                           
# Validate AST structure                                                   
.PHONY: test-ast                                                           
test-ast:                                                                  
	python3 tests/validate_ast_structure.py                                
                                                                           
# Validate syntax categories                                               
.PHONY: test-syntax                                                        
test-syntax:                                                               
	python3 tests/validate_syntax_categories.py                            
                                                                           
# Run all tests (including Rust unit tests)                                
.PHONY: test-all                                                           
test-all: build                                                            
	cargo test                                                             
	$(MAKE) test                                                           
                                                                           
# Clean build artifacts                                                    
.PHONY: clean                                                              
clean:                                                                     
	cargo clean                                                            
                                                                           
# Format code                                                              
.PHONY: format                                                             
format:                                                                    
	cargo fmt                                                              
                                                                           
# Run linting                                                              
.PHONY: lint                                                               
lint:                                                                      
	cargo clippy                                                           
                                                                           
# Run the lexer REPL                                                       
.PHONY: lexer-repl                                                         
lexer-repl:                                                                
	cargo run --bin lexer                                                  
                                                                           
# Run the parser REPL                                                      
.PHONY: parser-repl                                                        
parser-repl:                                                               
	cargo run --bin parser                                                 
                                                                           
# Run the compiler REPL                                                    
.PHONY: compiler-repl                                                      
compiler-repl:                                                             
	cargo run --bin compiler                                               
                                                                           
# Run the main REPL                                                        
.PHONY: repl                                                               
repl:                                                                      
	cargo run --bin mini_scheme                                            
                                                                           
# Run lexer on a file                                                      
.PHONY: lex                                                                
lex:                                                                       
	@echo "Usage: make lex FILE=path/to/file.scm"                          
	@if [ -n "$(FILE)" ]; then \                                           
	    cargo run --bin lexer -- $(FILE); \                                
	else \                                                                 
	    echo "Please specify a file: make lex FILE=path/to/file.scm"; \    
	fi                                                                     
                                                                           
# Run parser on a file                                                     
.PHONY: parse                                                              
parse:                                                                     
	@echo "Usage: make parse FILE=path/to/file.scm"                        
	@if [ -n "$(FILE)" ]; then \                                           
	    cargo run --bin parser -- $(FILE); \                               
	else \                                                                 
	    echo "Please specify a file: make parse FILE=path/to/file.scm"; \  
	fi                                                                     
                                                                           
# Run compiler on a file
.PHONY: compile
compile:
	@echo "Usage: make compile FILE=path/to/file.scm"
	@if [ -n "$(FILE)" ]; then \
	    cargo run --bin compiler -- $(FILE); \
	else \
	    echo "Please specify a file: make compile FILE=path/to/file.scm"; \
	fi                                                                     
                                                                           
# Run main application on a file                                           
.PHONY: run                                                                
run:                                                                       
	@echo "Usage: make run FILE=path/to/file.scm"                          
	@if [ -n "$(FILE)" ]; then \                                           
	    cargo run --bin mini_scheme -- $(FILE); \                          
	else \                                                                 
	    echo "Please specify a file: make run FILE=path/to/file.scm"; \    
	fi                                                                     
	                                                                       
# Show help                                                                
.PHONY: help                                                               
help:                                                                      
	@echo "MiniScheme Makefile targets:"                                   
	@echo ""                                                               
	@echo "Build targets:"                                                 
	@echo "  all           - Build all binaries (default)"                 
	@echo "  build         - Build all binaries"                           
	@echo "  lexer         - Build lexer binary"                           
	@echo "  parser        - Build parser binary"                          
	@echo "  compiler      - Build compiler binary"                        
	@echo "  mini_scheme   - Build main application"                       
	@echo ""                                                               
	@echo "Test targets:"                                                  
	@echo "  test          - Run all Python-based tests"                   
	@echo "  test-examples - Test example files with frontend"             
	@echo "  test-scm      - Test .scm files with external interpreter"    
	@echo "  test-ast      - Validate AST structure"                       
	@echo "  test-syntax   - Validate syntax categories"                   
	@echo "  test-all      - Run both Rust unit tests and Python tests"    
	@echo ""                                                               
	@echo "Run targets:"                                                   
	@echo "  repl          - Run main REPL"                                
	@echo "  lexer-repl    - Run lexer REPL"                               
	@echo "  parser-repl   - Run parser REPL"                              
	@echo "  compiler-repl - Run compiler REPL"                            
	@echo "  run FILE=...  - Run main application on a file"               
	@echo "  lex FILE=...  - Run lexer on a file"                          
	@echo "  parse FILE=... - Run parser on a file"                        
	@echo "  compile FILE=... - Run compiler on a file"                        
	@echo ""                                                               
	@echo "Utility targets:"                                               
	@echo "  clean         - Clean build artifacts"                        
	@echo "  format        - Format code with rustfmt"                     
	@echo "  lint          - Run linter with clippy"                       
	@echo "  help          - Show this help message" 