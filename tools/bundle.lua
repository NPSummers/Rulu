-- Made by 0x1000007e

-- Bundle Rulu into a single Lua file with package.preload entries
-- Usage: lua tools/bundle.lua

local modules = {
  ["Rulu.lib.lexer"] = "Rulu/lib/lexer.lua",
  ["Rulu.lib.parser"] = "Rulu/lib/parser.lua",
  ["Rulu.lib.emitter"] = "Rulu/lib/emitter.lua",
  ["Rulu.lib.checker"] = "Rulu/lib/checker.lua",
  ["Rulu.rulu"] = "Rulu/rulu.lua",
}

local function read_file(path)
  local f, err = io.open(path, "rb")
  if not f then error("Failed to read '" .. path .. "': " .. tostring(err)) end
  local s = f:read("*a")
  f:close()
  return s
end

local function ensure_dir(path)
  -- simple Windows/Unix compatible mkdir -p
  local sep = package.config:sub(1,1)
  local parts = {}
  for part in string.gmatch(path, "[^" .. sep .. "]+") do table.insert(parts, part) end
  local cur = ""
  for i, p in ipairs(parts) do
    cur = (i == 1) and p or (cur .. sep .. p)
    local ok = os.rename(cur, cur)
    if not ok then
      os.execute((sep == "\\" and ("mkdir \""..cur.."\"") or ("mkdir -p \""..cur.."\"")))
    end
  end
end

local function escape_long_brackets(s)
  -- choose a long bracket level that doesn't appear in s
  for eqs = 0, 5 do
    local open = "[" .. string.rep("=", eqs) .. "["
    local close = "]" .. string.rep("=", eqs) .. "]"
    if not s:find(open, 1, true) and not s:find(close, 1, true) then
      return eqs
    end
  end
  return 0
end

local function build_bundle()
  ensure_dir("dist")
  local out = {}
  table.insert(out, "-- rulu single-file bundle\n")
  table.insert(out, "local preload = package.preload\n")
  table.insert(out, "_G.__RULU_BUNDLED = true\n")
  for name, path in pairs(modules) do
    local src = read_file(path)
    local eqs = escape_long_brackets(src)
    local open = "[" .. string.rep("=", eqs) .. "["
    local close = "]" .. string.rep("=", eqs) .. "]"
    table.insert(out, string.format(
      "preload[%q] = function(... ) local _src = %s%s%s local _load = loadstring or load local _fn, _err = _load(_src, '@'..%q) if not _fn then error(_err) end return _fn(...) end\n",
      name, open, src, close, name))
  end
  table.insert(out, "local code = require('Rulu.rulu') or 0\n")
  table.insert(out, "if type(code) == 'number' then os.exit(code) end\n")
  local content = table.concat(out)
  local f = assert(io.open("dist/rulu.lua", "wb"))
  f:write(content)
  f:close()
  print("Wrote dist/rulu.lua")
end

build_bundle()


