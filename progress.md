# Progress

## To do

### Short-medium term

- lexer: rune, float, complex/imaginary and raw string literals
- parser: escape sequence interpretation in string literals
- parser+ast: top-level declarations, function declarations
- lexer/parser: automic semicolon insertion

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
