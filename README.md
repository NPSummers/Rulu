# Rulu

Rulu is a tiny Rust-like language that transpiles to Lua.

## Usage

- Print transpiled Lua: `lua Rulu/rulu.lua examples/hello.rulu --print`
- Write to a file and run: `lua Rulu/rulu.lua examples/hello.rulu --out out.lua --run`
- Modules example: `lua Rulu/rulu.lua examples/module_main.rulu --print`

## Structure

- `Rulu/lib/lexer.lua`: tokenizes source
- `Rulu/lib/parser.lua`: builds AST
- `Rulu/lib/emitter.lua`: emits Lua
- `Rulu/rulu.lua`: CLI driver
- `examples/`: sample `.rulu` programs

## Attribution

Made by 0x1000007e
