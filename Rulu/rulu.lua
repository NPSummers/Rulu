-- Made by 0x1000007e

-- rulu CLI: transpile Rust-like "rulu" to Lua

local function read_file(path)
  local f, err = io.open(path, "rb")
  if not f then return nil, err end
  local content = f:read("*a")
  f:close()
  return content
end

local function write_file(path, content)
  local f, err = io.open(path, "wb")
  if not f then return nil, err end
  f:write(content)
  f:close()
  return true
end

local function parse_args(argv)
  local args = { input = nil, out = nil, run = false, print = false }
  local i = 1
  while i <= #argv do
    local a = argv[i]
    if a == "--out" or a == "-o" then
      args.out = argv[i + 1]
      i = i + 1
    elseif a == "--run" then
      args.run = true
    elseif a == "--print" or a == "-p" then
      args.print = true
    elseif a == "--help" or a == "-h" then
      args.help = true
    else
      args.input = a
    end
    i = i + 1
  end
  return args
end

local function usage()
  print([[rulu - Rust-like language that transpiles to Lua
Usage:
  lua Rulu/rulu.lua <input.rulu> [--out out.lua] [--run] [--print]
  lua Rulu/rulu.lua --help
]])
end

local function main()
  local args = parse_args(arg)
  if args.help or (not args.input and not args.print) then
    usage()
    return 0
  end

  local Lexer = require("Rulu.lib.lexer")
  local Parser = require("Rulu.lib.parser")
  local Emitter = require("Rulu.lib.emitter")

  local source
  if args.input then
    local err
    source, err = read_file(args.input)
    if not source then
      io.stderr:write("Failed to read input: " .. tostring(err) .. "\n")
      return 1
    end
  else
    source = io.read("*a") or ""
  end

  local ok, tokens_or_err = pcall(function()
    local lexer = Lexer.new(source)
    return lexer:tokenize()
  end)
  if not ok then
    io.stderr:write("Lex error: " .. tostring(tokens_or_err) .. "\n")
    return 1
  end

  local tokens = tokens_or_err

  local ok2, ast_or_err = pcall(function()
    local parser = Parser.new(tokens)
    return parser:parse_program()
  end)
  if not ok2 then
    io.stderr:write("Parse error: " .. tostring(ast_or_err) .. "\n")
    return 1
  end

  local ast = ast_or_err

  local emitter = Emitter.new()
  local lua_code = emitter:emit_program(ast)

  if args.out then
    local okw, errw = write_file(args.out, lua_code)
    if not okw then
      io.stderr:write("Write error: " .. tostring(errw) .. "\n")
      return 1
    end
    print("Wrote " .. args.out)
  end

  if args.print or not args.out then
    io.write(lua_code)
  end

  if args.run then
    local loader = loadstring or load
    local chunk, errc = loader(lua_code, args.out or args.input or "rulu_chunk")
    if not chunk then
      io.stderr:write("Load error: " .. tostring(errc) .. "\n")
      return 1
    end
    local okr, errr = pcall(function()
      local exports = chunk()
      if type(exports) == "table" then
        -- module: don't auto-run main
      else
        if type(_G.main) == "function" then _G.main() end
      end
    end)
    if not okr then
      io.stderr:write("Runtime error: " .. tostring(errr) .. "\n")
      return 1
    end
  end

  return 0
end

return main()


