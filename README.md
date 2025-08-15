# Rulu

Rulu is a tiny Rust-like language that transpiles to Lua.

## What's new

- Immutable-by-default variables: reassignments require `let mut name = ...`.
- Small prelude available in all programs: `println(...)` and `range(a, b, step)`.
- Modules can export constants: `pub const NAME = expr;` becomes `M.NAME` in Lua when inside a module.
- Ranges: `for i in a..=b {}` for inclusive end; `step` is supported: `for i in 0..10 step 2 {}`.
- Modules: Added calling module functions from other rulu files

## Usage

- Print transpiled Lua: `lua Rulu/rulu.lua examples/hello.rulu --print`
- Write to a file and run: `lua Rulu/rulu.lua examples/hello.rulu --out out.lua --run`
- Modules example: `lua Rulu/rulu.lua examples/module_main.rulu --print`

### Bundled exe usage

- `rulu.exe --version` prints version
- `rulu.exe <input.rulu>` transpiles and runs (no `--print/--out/--run` in bundled mode)

## Bundling to a single file and Windows EXE

- Build a single-file `dist/rulu.lua` (no external requires):
  - `lua tools/bundle.lua`
- Create a standalone `rulu.exe` (Windows):
  1. Download srlua + glue (from PUC Lua) or use a prebuilt release.
  2. Build/obtain `srlua.exe` and `srglue.exe` for your Lua version.
  3. Run: `srglue srlua.exe dist/rulu.lua rulu.exe`
  4. Now `rulu.exe` runs without a separate Lua installation.

### One-command build on Windows

- Run: `powershell -ExecutionPolicy Bypass -File tools\build-win-exe.ps1`
  - This bundles to `dist\rulu.lua` and glues it with downloaded srlua to produce `rulu.exe`.
  - Downloads binaries from `noahp/srlua-mingw` when available [link](https://github.com/noahp/srlua-mingw).

## Structure

- `Rulu/lib/lexer.lua`: tokenizes source
- `Rulu/lib/parser.lua`: builds AST
- `Rulu/lib/emitter.lua`: emits Lua
- `Rulu/rulu.lua`: CLI driver
- `examples/`: sample `.rulu` programs

## Attribution

Made by 0x1000007e
