-- Made by 0x1000007e

-- rulu CLI: transpile Rust-like "rulu" to Lua

local VERSION = "0.1.0"
local BUNDLED = _G.__RULU_BUNDLED or false

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
  local args = { input = nil, out = nil, run = false, print = false, version = false }
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
    elseif a == "--version" or a == "-v" then
      args.version = true
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
  local extra = BUNDLED and "Usage (bundled exe):\n  rulu.exe <input.rulu>\n  rulu.exe --version\n" or "Usage:\n  lua Rulu/rulu.lua <input.rulu> [--out out.lua] [--run] [--print]\n  lua Rulu/rulu.lua --help\n"
  print(([[rulu - Rust-like language that transpiles to Lua
Usage:
%s]]):format(extra))
end

local function main()
  local args = parse_args(arg)
  if args.version then
    print("rulu " .. VERSION)
    return 0
  end
  if args.help or (not args.input and not args.print) then
    usage()
    return 0
  end

  local Lexer = require("Rulu.lib.lexer")
  local Parser = require("Rulu.lib.parser")
  local Checker = require("Rulu.lib.checker")
  local Emitter = require("Rulu.lib.emitter")

  local function dirname(path)
    if not path then return "." end
    local dir = path:gsub("\\", "/")
    local idx = dir:match("^.*()/")
    if idx then
      return dir:sub(1, idx - 1)
    else
      return "."
    end
  end

  local function install_rulu_searcher(base_dir)
    local searchers = package.searchers or package.loaders
    local function searcher(modname)
      local rel = (modname or ""):gsub("%.", "/")
      local candidates = {
        base_dir .. "/" .. rel .. ".lua",
        base_dir .. "/" .. rel .. ".rulu",
        rel .. ".lua",
        rel .. ".rulu",
      }
      for _, p in ipairs(candidates) do
        local f = io.open(p, "rb")
        if f then
          local content = f:read("*a"); f:close()
          if p:sub(-5) == ".rulu" then
            local ok_lex, tokens_or_err = pcall(function()
              local lx = Lexer.new(content)
              return lx:tokenize()
            end)
            if not ok_lex then return nil, tokens_or_err end
            local ok_par, ast_or_err = pcall(function()
              local ps = Parser.new(tokens_or_err)
              return ps:parse_program()
            end)
            if not ok_par then return nil, ast_or_err end
            local ok_chk, chk_err = pcall(function() return Checker.check(ast_or_err) end)
            if not ok_chk then return nil, chk_err end
            local em = Emitter.new()
            local lua_code = em:emit_program(ast_or_err)
            local loader = (loadstring or load)(lua_code, p)
            if not loader then return nil, "Failed to load generated Lua from " .. p end
            return loader, p
          else
            local loader = (loadstring or load)(content, p)
            if not loader then return nil, "Failed to load Lua file " .. p end
            return loader, p
          end
        end
      end
      return nil, "rulu searcher: module not found '" .. tostring(modname) .. "'"
    end
    table.insert(searchers, 1, searcher)
  end

  if BUNDLED then
    if args.out or args.print or args.run then
      io.stderr:write("In bundled mode, --out/--print/--run are not supported. Just pass an input file.\n")
      return 1
    end
  end

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

  local okc, errc = pcall(function()
    return Checker.check(ast)
  end)
  if not okc then
    io.stderr:write("Check error: " .. tostring(errc) .. "\n")
    return 1
  end

  local emitter = Emitter.new()
  local lua_code = emitter:emit_program(ast)

  if not BUNDLED and args.out then
    local okw, errw = write_file(args.out, lua_code)
    if not okw then
      io.stderr:write("Write error: " .. tostring(errw) .. "\n")
      return 1
    end
    print("Wrote " .. args.out)
  end

  if not BUNDLED and args.print then
    io.write(lua_code)
  end

  if (not BUNDLED and args.run) or (BUNDLED and args.input) then
    install_rulu_searcher(dirname(args.input))
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


