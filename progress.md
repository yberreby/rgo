# Progress

## To do

### Short-medium term

- lexer: rune, float, complex/imaginary and raw string literals
- parser: escape sequence interpretation in string literals
- parser+ast: top-level declarations, function declarations

#### lexer/parser: automic semicolon insertion

"The formal grammar uses semicolons ";" as terminators in a number of
productions. Go programs may omit most of these semicolons using the following
two rules:

  When the input is broken into tokens, a semicolon is automatically inserted
  into the token stream immediately after a line's final token if that token is
    - an identifier
    - an integer, floating-point, imaginary, rune, or string literal
    - one of the keywords break, continue, fallthrough, or return
    - one of the operators and delimiters ++, --, ), ], or }

To allow complex statements to occupy a single line, a semicolon may be omitted
before a closing ")" or "}"."

### Long-term

- testing: integration tests for various programs (around 10 programs would be
  nice)
- runtime integration
- standard library (written in Go), import via submodules from the main Go
  project
- linking/dependency resolution
- LLVM codegen
- LLVM optimization passes
- LLVM LTO (Link-Time Optimization)
- Parallel compilation
- advanced warnings
- `rgo.toml` file to keep track of Git dependencies using branches, tags and
  commit hashes
