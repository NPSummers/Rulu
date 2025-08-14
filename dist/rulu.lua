-- rulu single-file bundle
local preload = package.preload
_G.__RULU_BUNDLED = true
preload["Rulu.lib.lexer"] = function(... ) local _src = [[-- Made by 0x1000007e

-- rulu: Lexer
-- Converts source text into a stream of tokens

local Lexer = {}
Lexer.__index = Lexer

local KEYWORDS = {
  ["fn"] = true,
  ["let"] = true,
  ["mut"] = true,
  ["if"] = true,
  ["else"] = true,
  ["return"] = true,
  ["while"] = true,
  ["loop"] = true,
  ["break"] = true,
  ["continue"] = true,
  ["const"] = true,
  ["crate"] = true,
  ["enum"] = true,
  ["extern"] = true,
  ["for"] = true,
  ["impl"] = true,
  ["in"] = true,
  ["match"] = true,
  ["mod"] = true,
  ["move"] = true,
  ["pub"] = true,
  ["ref"] = true,
  ["self"] = true,
  ["Self"] = true,
  ["static"] = true,
  ["struct"] = true,
  ["trait"] = true,
  ["type"] = true,
  ["unsafe"] = true,
  ["use"] = true,
  ["where"] = true,
  ["as"] = true,
  ["true"] = true,
  ["false"] = true,
  ["step"] = true,
}

local function is_alpha(c)
  return c:match("[A-Za-z_]") ~= nil
end

local function is_alnum(c)
  return c:match("[A-Za-z0-9_]") ~= nil
end

local function is_digit(c)
  return c:match("[0-9]") ~= nil
end

local function make_token(type_, value, line, col)
  return { type = type_, value = value, line = line, col = col }
end

function Lexer.new(input)
  return setmetatable({
    input = input or "",
    i = 1,
    line = 1,
    col = 1,
    tokens = {},
  }, Lexer)
end

function Lexer:peek(n)
  n = n or 0
  local idx = self.i + n
  if idx > #self.input then return "" end
  return self.input:sub(idx, idx)
end

function Lexer:advance(n)
  n = n or 1
  for _ = 1, n do
    local c = self:peek(0)
    if c == "\n" then
      self.line = self.line + 1
      self.col = 1
    else
      self.col = self.col + 1
    end
    self.i = self.i + 1
  end
end

function Lexer:add(type_, value, line, col)
  table.insert(self.tokens, make_token(type_, value, line, col))
end

function Lexer:skip_whitespace()
  while true do
    local c = self:peek(0)
    -- whitespace
    if c == " " or c == "\t" or c == "\r" or c == "\n" then
      self:advance(1)
    -- line comments: // ... \n
    elseif c == "/" and self:peek(1) == "/" then
      while self:peek(0) ~= "" and self:peek(0) ~= "\n" do
        self:advance(1)
      end
    -- block comments: /* ... */
    elseif c == "/" and self:peek(1) == "*" then
      self:advance(2)
      while true do
        if self:peek(0) == "" then break end
        if self:peek(0) == "*" and self:peek(1) == "/" then
          self:advance(2)
          break
        end
        self:advance(1)
      end
    else
      break
    end
  end
end

function Lexer:read_number()
  local start_line, start_col = self.line, self.col
  local s = ""
  local saw_dot = false
  while true do
    local c = self:peek(0)
    if is_digit(c) then
      s = s .. c
      self:advance(1)
    elseif c == "." and not saw_dot and is_digit(self:peek(1)) then
      saw_dot = true
      s = s .. c
      self:advance(1)
    else
      break
    end
  end
  self:add("NUMBER", s, start_line, start_col)
end

function Lexer:read_identifier()
  local start_line, start_col = self.line, self.col
  local s = ""
  while true do
    local c = self:peek(0)
    if c ~= "" and is_alnum(c) then
      s = s .. c
      self:advance(1)
    else
      break
    end
  end
  if KEYWORDS[s] then
    self:add("KW", s, start_line, start_col)
  else
    self:add("IDENT", s, start_line, start_col)
  end
end

function Lexer:read_string()
  local quote = self:peek(0)
  local start_line, start_col = self.line, self.col
  self:advance(1)
  local s = ""
  while true do
    local c = self:peek(0)
    if c == "" then
      error("Unterminated string at line " .. start_line)
    elseif c == "\\" then
      local n = self:peek(1)
      if n == "n" then s = s .. "\n" elseif n == "t" then s = s .. "\t" elseif n == quote then s = s .. quote else s = s .. n end
      self:advance(2)
    elseif c == quote then
      self:advance(1)
      break
    else
      s = s .. c
      self:advance(1)
    end
  end
  self:add("STRING", s, start_line, start_col)
end

local TWO_CHAR_OPS = {
  ["=="] = true, ["!="] = true, ["<="] = true, [">="] = true,
  ["->"] = true, ["&&"] = true, ["||"] = true, [".."] = true,
  ["::"] = true,
  ["=>"] = true,
}

local THREE_CHAR_OPS = {
  ["..="] = true,
}

local SINGLE_CHAR = {
  ["("] = "LPAREN", [")"] = "RPAREN",
  ["{"] = "LBRACE", ["}"] = "RBRACE",
  ["["] = "LSQUARE", ["]"] = "RSQUARE",
  [","] = "COMMA",  [";"] = "SEMI", [":"] = "COLON",
}

local ONE_CHAR_OPS = {
  ["="] = true, ["+"] = true, ["-"] = true, ["*"] = true,
  ["/"] = true, ["%"] = true, ["<"] = true, [">"] = true,
  ["!"] = true, ["."] = true,
}

function Lexer:read_operator_or_symbol()
  local start_line, start_col = self.line, self.col
  local c1 = self:peek(0)
  local c2 = self:peek(1)
  local c3 = self:peek(2)
  local pair = c1 .. c2
  local tri = pair .. c3
  if THREE_CHAR_OPS[tri] then
    self:add("OP", tri, start_line, start_col)
    self:advance(3)
    return
  end
  if TWO_CHAR_OPS[pair] then
    self:add("OP", pair, start_line, start_col)
    self:advance(2)
    return
  end
  if SINGLE_CHAR[c1] then
    self:add(SINGLE_CHAR[c1], c1, start_line, start_col)
    self:advance(1)
    return
  end
  if ONE_CHAR_OPS[c1] then
    self:add("OP", c1, start_line, start_col)
    self:advance(1)
    return
  end
  error("Unexpected character '" .. c1 .. "' at line " .. start_line .. ", col " .. start_col)
end

function Lexer:tokenize()
  while self:peek(0) ~= "" do
    self:skip_whitespace()
    local c = self:peek(0)
    if c == "" then break end

    if is_alpha(c) then
      self:read_identifier()
    elseif is_digit(c) then
      self:read_number()
    elseif c == '"' or c == "'" then
      self:read_string()
    else
      self:read_operator_or_symbol()
    end
  end
  self:add("EOF", "", self.line, self.col)
  return self.tokens
end

return Lexer


]] local _load = loadstring or load local _fn, _err = _load(_src, '@'.."Rulu.lib.lexer") if not _fn then error(_err) end return _fn(...) end
preload["Rulu.lib.parser"] = function(... ) local _src = [[-- Made by 0x1000007e

-- rulu: Parser
-- Transforms tokens to an AST

local Parser = {}
Parser.__index = Parser

local function node(kind, props)
  props = props or {}
  props.kind = kind
  return props
end

function Parser.new(tokens)
  return setmetatable({ tokens = tokens, pos = 1 }, Parser)
end

function Parser:current()
  return self.tokens[self.pos]
end

function Parser:peek(n)
  n = n or 0
  return self.tokens[self.pos + n]
end

function Parser:is(type_, value)
  local t = self:current()
  if not t then return false end
  if t.type ~= type_ then return false end
  if value ~= nil and t.value ~= value then return false end
  return true
end

function Parser:match(type_, value)
  if self:is(type_, value) then
    local t = self:current()
    self.pos = self.pos + 1
    return t
  end
  return nil
end

function Parser:expect(type_, value, message)
  local t = self:match(type_, value)
  if not t then
    local cur = self:current() or { type = "<eof>", value = "" }
    error((message or "Unexpected token") .. ": expected " .. type_ .. (value and (" '" .. value .. "'") or "") ..
      ", got " .. cur.type .. (cur.value and (" '" .. tostring(cur.value) .. "'") or ""))
  end
  return t
end

function Parser:parse_program()
  local body = {}
  local moduleName = nil
  while not self:is("EOF") do
    local stmt = self:parse_top_level()
    if stmt then table.insert(body, stmt) end
  end
  -- Allow a leading module decl: mod Name;
  if body[1] and body[1].kind == "ModuleDecl" then
    moduleName = body[1].name
    table.remove(body, 1)
  end
  return node("Program", { body = body, moduleName = moduleName })
end

function Parser:parse_top_level()
  if self:is("KW", "mod") then
    return self:parse_module_decl()
  elseif self:is("KW", "use") then
    return self:parse_use()
  elseif self:is("KW", "const") or self:is("KW", "static") then
    return self:parse_const_like()
  elseif self:is("KW", "pub") then
    if self:peek(1) and self:peek(1).type == "KW" and self:peek(1).value == "fn" then
      return self:parse_function()
    elseif self:peek(1) and self:peek(1).type == "KW" and (self:peek(1).value == "const" or self:peek(1).value == "static") then
      return self:parse_const_like()
    end
  elseif self:is("KW", "fn") then
    return self:parse_function()
  elseif self:is("KW", "let") then
    return self:parse_let()
  elseif self:is("SEMI") then
    self:match("SEMI")
    return nil
  else
    -- allow statements at top-level
    return self:parse_statement()
  end
end

function Parser:parse_module_decl()
  self:expect("KW", "mod")
  local name = self:expect("IDENT", nil, "Module name expected").value
  self:expect("SEMI", nil, "';' expected after module decl")
  return node("ModuleDecl", { name = name })
end

function Parser:parse_function()
  local is_public = false
  if self:match("KW", "pub") then
    is_public = true
  end
  self:expect("KW", "fn")
  local name_tok = self:expect("IDENT", nil, "Function name expected")
  self:expect("LPAREN")
  local params = {}
  if not self:is("RPAREN") then
    repeat
      local param_name = self:expect("IDENT", nil, "Parameter name expected").value
      if self:match("COLON") then
        -- consume a simple type identifier if present
        if self:is("IDENT") or self:is("KW") then self.pos = self.pos + 1 end
      end
      table.insert(params, { name = param_name })
    until not self:match("COMMA")
  end
  self:expect("RPAREN")
  if self:match("OP", "->") then
    -- return type (skip simple ident)
    if self:is("IDENT") or self:is("KW") then self.pos = self.pos + 1 end
  end
  local body = self:parse_block()
  return node("Function", { name = name_tok.value, params = params, body = body, isPublic = is_public })
end

function Parser:parse_block()
  self:expect("LBRACE")
  local stmts = {}
  while not self:is("RBRACE") do
    local stmt = self:parse_statement()
    if stmt then table.insert(stmts, stmt) end
  end
  self:expect("RBRACE")
  return node("Block", { body = stmts })
end

function Parser:parse_statement()
  if self:is("KW", "let") then
    return self:parse_let()
  elseif self:is("KW", "return") then
    return self:parse_return()
  elseif self:is("KW", "if") then
    return self:parse_if()
  elseif self:is("KW", "while") then
    return self:parse_while()
  elseif self:is("KW", "loop") then
    return self:parse_loop()
  elseif self:is("KW", "break") then
    self:match("KW", "break")
    self:expect("SEMI", nil, "';' expected after break")
    return node("Break", {})
  elseif self:is("KW", "const") or self:is("KW", "static") then
    return self:parse_const_like()
  elseif self:is("KW", "continue") then
    self:match("KW", "continue")
    self:expect("SEMI", nil, "';' expected after continue")
    return node("Continue", {})
  elseif self:is("KW", "for") then
    return self:parse_for()
  elseif self:is("KW", "match") then
    return self:parse_match()
  elseif self:is("IDENT") and self:peek(1) and self:peek(1).type == "OP" and self:peek(1).value == "=" then
    return self:parse_assign()
  else
    local expr = self:parse_expression()
    self:expect("SEMI", nil, "';' expected after expression")
    return node("ExprStmt", { expression = expr })
  end
end

function Parser:parse_match()
  -- match expr { pat => { block } , ... }
  self:expect("KW", "match")
  local discr = self:parse_expression()
  self:expect("LBRACE")
  local arms = {}
  local hasWildcard = false
  while not self:is("RBRACE") do
    local pat
    if (self:is("IDENT") and self:current().value == "_") then
      hasWildcard = true
      self.pos = self.pos + 1
      pat = node("PatternWildcard", {})
    elseif self:is("NUMBER") or self:is("STRING") or self:is("KW", "true") or self:is("KW", "false") then
      local val = self:parse_primary()
      pat = node("PatternLiteral", { value = val })
    else
      error("Unsupported match pattern; only literals and '_' are supported")
    end
    self:expect("OP", "=>")
    local body = self:parse_block()
    table.insert(arms, { pattern = pat, body = body })
    self:match("COMMA")
  end
  self:expect("RBRACE")
  return node("Match", { discriminant = discr, arms = arms, hasWildcard = hasWildcard })
end

function Parser:parse_for()
  -- for i in a..b { ... }
  self:expect("KW", "for")
  local iter = self:expect("IDENT", nil, "Iterator variable name expected").value
  self:expect("KW", "in")
  local range_expr = self:parse_expression()
  local start_expr, end_expr, inclusive, step_expr
  if self:is("OP", "..") or self:is("OP", "..=") then
    local op = self:current().value; self.pos = self.pos + 1
    inclusive = (op == "..=")
    start_expr = range_expr
    end_expr = self:parse_expression()
    if self:match("KW", "step") then
      step_expr = self:parse_expression()
    end
  elseif range_expr.kind == "Binary" and range_expr.operator == ".." then
    start_expr = range_expr.left
    end_expr = range_expr.right
    if self:match("KW", "step") then
      step_expr = self:parse_expression()
    end
  else
    -- for v in expr { ... }
    local body = self:parse_block()
    return node("ForEach", { var = iter, iterExpr = range_expr, body = body })
  end
  local body = self:parse_block()
  return node("ForRange", { var = iter, startExpr = start_expr, endExpr = end_expr, inclusive = inclusive, stepExpr = step_expr, body = body })
end

function Parser:parse_let()
  self:expect("KW", "let")
  local mutable = self:match("KW", "mut") ~= nil
  local name_tok = self:expect("IDENT", nil, "Variable name expected")
  if self:match("COLON") then
    -- simple type ident
    if self:is("IDENT") or self:is("KW") then self.pos = self.pos + 1 end
  end
  local init = nil
  if self:match("OP", "=") then
    init = self:parse_expression()
  end
  self:expect("SEMI", nil, "';' expected after let binding")
  return node("Let", { name = name_tok.value, mutable = mutable, init = init })
end

function Parser:parse_assign()
  local name = self:expect("IDENT").value
  self:expect("OP", "=")
  local value = self:parse_expression()
  self:expect("SEMI", nil, "';' expected after assignment")
  return node("Assign", { name = name, value = value })
end

function Parser:parse_return()
  self:expect("KW", "return")
  local expr = nil
  if not self:is("SEMI") then
    expr = self:parse_expression()
  end
  self:expect("SEMI", nil, "';' expected after return")
  return node("Return", { argument = expr })
end

function Parser:parse_if()
  self:expect("KW", "if")
  local test = self:parse_expression()
  local consequent = self:parse_block()
  local alternate = nil
  if self:match("KW", "else") then
    alternate = self:parse_block()
  end
  return node("If", { test = test, consequent = consequent, alternate = alternate })
end

function Parser:parse_while()
  self:expect("KW", "while")
  local test = self:parse_expression()
  local body = self:parse_block()
  return node("While", { test = test, body = body })
end

function Parser:parse_loop()
  self:expect("KW", "loop")
  local body = self:parse_block()
  return node("Loop", { body = body })
end

function Parser:parse_use()
  -- use path::to::name as alias;
  self:expect("KW", "use")
  local parts = {}
  local function accept_part()
    if self:is("IDENT") or self:is("KW", "self") or self:is("KW", "crate") or self:is("KW", "super") then
      local t = self:current(); self.pos = self.pos + 1
      return t.value or t.type
    end
    return nil
  end
  local first = accept_part()
  if not first then error("Expected path after 'use'") end
  table.insert(parts, first)
  while self:is("OP", "::") do
    self.pos = self.pos + 1
    local p = accept_part()
    if not p then error("Expected identifier after '::'") end
    table.insert(parts, p)
  end
  local alias = nil
  if self:match("KW", "as") then
    alias = self:expect("IDENT", nil, "Alias name expected").value
  end
  self:expect("SEMI", nil, "';' expected after use statement")
  return node("Use", { parts = parts, alias = alias })
end

function Parser:parse_const_like()
  -- [pub] const/ static NAME = expr;
  local is_public = false
  if self:match("KW", "pub") then
    is_public = true
  end
  local kind
  if self:is("KW", "const") or self:is("KW", "static") then
    kind = self:current().value
    self.pos = self.pos + 1
  else
    error("const/static expected")
  end
  local name = self:expect("IDENT", nil, "Name expected").value
  if self:match("COLON") then
    if self:is("IDENT") or self:is("KW") then self.pos = self.pos + 1 end
  end
  self:expect("OP", "=")
  local value = self:parse_expression()
  self:expect("SEMI")
  return node("Const", { storage = kind, name = name, value = value, isPublic = is_public })
end

-- Expressions (Pratt parser)
local PRECEDENCE = {
  ["||"] = 1,
  ["&&"] = 2,
  ["=="] = 3, ["!="] = 3,
  ["<"] = 4, [">"] = 4, ["<="] = 4, [">="] = 4,
  [".."] = 5,
  ["+"] = 6, ["-"] = 6,
  ["*"] = 7, ["/"] = 7, ["%"] = 7,
}

function Parser:parse_expression()
  return self:parse_precedence(1)
end

function Parser:parse_precedence(min_prec)
  local left = self:parse_unary()
  while true do
    local t = self:current()
    if not t or t.type ~= "OP" then break end
    local prec = PRECEDENCE[t.value]
    if not prec or prec < min_prec then break end
    local op = t.value
    self.pos = self.pos + 1
    local right = self:parse_precedence(prec + 1)
    left = node("Binary", { operator = op, left = left, right = right })
  end
  return left
end

function Parser:parse_unary()
  if self:is("OP", "!") or self:is("OP", "-") then
    local op = self:current().value
    self.pos = self.pos + 1
    local argument = self:parse_unary()
    return node("Unary", { operator = op, argument = argument })
  end
  return self:parse_call()
end

function Parser:parse_call()
  local expr = self:parse_primary()
  while true do
    if self:is("LPAREN") then
      self:expect("LPAREN")
      local args = {}
      if not self:is("RPAREN") then
        repeat
          table.insert(args, self:parse_expression())
        until not self:match("COMMA")
      end
      self:expect("RPAREN")
      expr = node("Call", { callee = expr, arguments = args })
    elseif self:is("LSQUARE") then
      self:expect("LSQUARE")
      local idx = self:parse_expression()
      self:expect("RSQUARE")
      expr = node("Index", { object = expr, index = idx })
    elseif self:is("OP", ".") then
      self:expect("OP", ".")
      local prop = self:expect("IDENT", nil, "Property name expected").value
      expr = node("Member", { object = expr, property = prop })
    else
      break
    end
  end
  return expr
end

function Parser:parse_primary()
  if self:is("NUMBER") then
    local v = self:current().value
    self.pos = self.pos + 1
    return node("Literal", { value = v, raw = v, literalType = "number" })
  elseif self:is("STRING") then
    local v = self:current().value
    self.pos = self.pos + 1
    return node("Literal", { value = v, raw = string.format("%q", v), literalType = "string" })
  elseif self:is("KW", "true") or self:is("KW", "false") then
    local v = self:current().value == "true"
    self.pos = self.pos + 1
    return node("Literal", { value = v, raw = v and "true" or "false", literalType = "boolean" })
  elseif self:is("IDENT") then
    local name = self:current().value
    self.pos = self.pos + 1
    return node("Identifier", { name = name })
  elseif self:is("LSQUARE") then
    self:expect("LSQUARE")
    local elements = {}
    if not self:is("RSQUARE") then
      repeat
        table.insert(elements, self:parse_expression())
      until not self:match("COMMA")
    end
    self:expect("RSQUARE")
    return node("ArrayLiteral", { elements = elements })
  elseif self:is("KW", "self") or self:is("KW", "Self") or self:is("KW", "crate") then
    local name = self:current().value
    self.pos = self.pos + 1
    return node("Identifier", { name = name })
  elseif self:is("LPAREN") then
    self:expect("LPAREN")
    local e = self:parse_expression()
    self:expect("RPAREN")
    return e
  else
    local t = self:current() or { type = "<eof>", value = "" }
    error("Unexpected token in expression: " .. t.type .. " '" .. tostring(t.value) .. "'")
  end
end

return Parser


]] local _load = loadstring or load local _fn, _err = _load(_src, '@'.."Rulu.lib.parser") if not _fn then error(_err) end return _fn(...) end
preload["Rulu.lib.emitter"] = function(... ) local _src = [[-- Made by 0x1000007e

-- rulu: Emitter
-- Emits Lua code from AST

local Emitter = {}
Emitter.__index = Emitter

local function indent_str(depth)
  return string.rep("  ", depth)
end

local function to_lua_operator(op)
  if op == "!" then return "not" end
  if op == "&&" then return "and" end
  if op == "||" then return "or" end
  if op == "!=" then return "~=" end
  return op
end

function Emitter.new()
  return setmetatable({ lines = {}, depth = 0, label_counter = 0, continue_label_stack = {} }, Emitter)
end

function Emitter:emit(line)
  table.insert(self.lines, (indent_str(self.depth) .. (line or "")))
end

function Emitter:with_indent(fn)
  self.depth = self.depth + 1
  fn()
  self.depth = self.depth - 1
end

function Emitter:gen_temp(prefix)
  self.label_counter = self.label_counter + 1
  return string.format("__rulu_%s_%d", prefix or "t", self.label_counter)
end

function Emitter:push_continue_label()
  local label = self:gen_temp("cont")
  table.insert(self.continue_label_stack, label)
  return label
end

function Emitter:pop_continue_label()
  table.remove(self.continue_label_stack)
end

function Emitter:current_continue_label()
  return self.continue_label_stack[#self.continue_label_stack]
end

function Emitter:emit_program(ast)
  self:emit("-- generated by rulu")
  -- Prelude: small standard helpers
  self:emit("local function println(...) print(...) end")
  self:emit("local function range(a, b, step)\n  a = a or 1\n  b = b or a\n  if a == b then return { a } end\n  step = step or (a <= b and 1 or -1)\n  local t = {}\n  if step > 0 then\n    for i = a, b, step do t[#t+1] = i end\n  else\n    for i = a, b, step do t[#t+1] = i end\n  end\n  return t\nend")
  if ast.moduleName then
    self._inside_module = true
    self:emit("local M = {} -- module: " .. ast.moduleName)
  else
    self._inside_module = false
  end
  for _, stmt in ipairs(ast.body) do
    self:emit_statement(stmt)
  end
  if ast.moduleName then
    self:emit("return M")
  end
  return table.concat(self.lines, "\n") .. "\n"
end

function Emitter:emit_block(block)
  for _, stmt in ipairs(block.body) do
    self:emit_statement(stmt)
  end
end

function Emitter:emit_block_with_ctx(block, ctx)
  for _, stmt in ipairs(block.body) do
    if ctx.guard_active.value then
      self:emit("if not " .. ctx.guard_var .. " then")
      self:with_indent(function()
        self:emit_statement_with_ctx(stmt, ctx)
      end)
      self:emit("end")
    else
      self._encountered_continue = false
      self:emit_statement_with_ctx(stmt, ctx)
      if self._encountered_continue then
        ctx.guard_active.value = true
      end
    end
  end
end

function Emitter:emit_statement(stmt)
  return self:emit_statement_with_ctx(stmt, nil)
end

function Emitter:emit_statement_with_ctx(stmt, ctx)
  local k = stmt.kind
  if k == "Function" then
    local names = {}
    for _, p in ipairs(stmt.params) do table.insert(names, p.name) end
    if stmt.isPublic and self._inside_module then
      self:emit(string.format("function M.%s(%s)", stmt.name, table.concat(names, ", ")))
    else
      self:emit(string.format("function %s(%s)", stmt.name, table.concat(names, ", ")))
    end
    self:with_indent(function()
      self:emit_block(stmt.body)
    end)
    self:emit("end")
  elseif k == "Let" then
    if stmt.init then
      self:emit("local " .. stmt.name .. " = " .. self:emit_expression(stmt.init))
    else
      self:emit("local " .. stmt.name)
    end
  elseif k == "Assign" then
    self:emit(stmt.name .. " = " .. self:emit_expression(stmt.value))
  elseif k == "Return" then
    if stmt.argument then
      self:emit("return " .. self:emit_expression(stmt.argument))
    else
      self:emit("return")
    end
  elseif k == "If" then
    self:emit("if " .. self:emit_expression(stmt.test) .. " then")
    self:with_indent(function()
      if ctx then self:emit_block_with_ctx(stmt.consequent, ctx) else self:emit_block(stmt.consequent) end
    end)
    if stmt.alternate then
      self:emit("else")
      self:with_indent(function()
        if ctx then self:emit_block_with_ctx(stmt.alternate, ctx) else self:emit_block(stmt.alternate) end
      end)
    end
    self:emit("end")
  elseif k == "While" then
    self:emit("while " .. self:emit_expression(stmt.test) .. " do")
    self:with_indent(function()
      local guard_var = self:gen_temp("contflag")
      self:emit("local " .. guard_var .. " = false")
      local subctx = { guard_var = guard_var, guard_active = { value = false }, inside_loop = true }
      self:emit_block_with_ctx(stmt.body, subctx)
    end)
    self:emit("end")
  elseif k == "ForRange" then
    local start_code = self:emit_expression(stmt.startExpr)
    local end_code = self:emit_expression(stmt.endExpr)
    local step_code
    if stmt.stepExpr then
      step_code = self:emit_expression(stmt.stepExpr)
    elseif stmt.inclusive then
      step_code = nil -- default step from start/end sign
    else
      step_code = nil -- default
    end
    if stmt.inclusive then
      -- Inclusive range: adjust end depending on step
      local s = self:gen_temp("step")
      local e = self:gen_temp("end")
      self:emit(string.format("local %s = %s", e, end_code))
      if step_code then
        self:emit(string.format("local %s = %s", s, step_code))
        self:emit(string.format("for %s = %s, %s, %s do", stmt.var, start_code, e, s))
      else
        self:emit(string.format("for %s = %s, %s do", stmt.var, start_code, e))
      end
    else
      if step_code then
        self:emit(string.format("for %s = %s, %s, %s do", stmt.var, start_code, end_code, step_code))
      else
        self:emit(string.format("for %s = %s, %s do", stmt.var, start_code, end_code))
      end
    end
    self:with_indent(function()
      local guard_var = self:gen_temp("contflag")
      self:emit("local " .. guard_var .. " = false")
      local subctx = { guard_var = guard_var, guard_active = { value = false }, inside_loop = true }
      self:emit_block_with_ctx(stmt.body, subctx)
    end)
    self:emit("end")
  elseif k == "ForEach" then
    local iter = self:emit_expression(stmt.iterExpr)
    local idx = self:gen_temp("i")
    self:emit(string.format("for %s = 1, #(%s) do", idx, iter))
    self:with_indent(function()
      self:emit(string.format("local %s = (%s)[%s]", stmt.var, iter, idx))
      local guard_var = self:gen_temp("contflag")
      self:emit("local " .. guard_var .. " = false")
      local subctx = { guard_var = guard_var, guard_active = { value = false }, inside_loop = true }
      self:emit_block_with_ctx(stmt.body, subctx)
    end)
    self:emit("end")
  elseif k == "Loop" then
    self:emit("while true do")
    self:with_indent(function()
      local guard_var = self:gen_temp("contflag")
      self:emit("local " .. guard_var .. " = false")
      local subctx = { guard_var = guard_var, guard_active = { value = false }, inside_loop = true }
      self:emit_block_with_ctx(stmt.body, subctx)
    end)
    self:emit("end")
  elseif k == "Break" then
    self:emit("break")
  elseif k == "Continue" then
    if not ctx or not ctx.inside_loop then
      self:emit("error('continue used outside of loop')")
    else
      self:emit(ctx.guard_var .. " = true")
      self._encountered_continue = true
    end
  elseif k == "Match" then
    local tmp = self:gen_temp("scrut")
    self:emit("local " .. tmp .. " = " .. self:emit_expression(stmt.discriminant))
    local first = true
    for _, arm in ipairs(stmt.arms) do
      if arm.pattern.kind == "PatternLiteral" then
        local cond = tmp .. " == " .. self:emit_expression(arm.pattern.value)
        if first then
          self:emit("if " .. cond .. " then")
          first = false
        else
          self:emit("elseif " .. cond .. " then")
        end
        self:with_indent(function()
          self:emit_block(arm.body)
        end)
      elseif arm.pattern.kind == "PatternWildcard" then
        if first then
          self:emit("do")
          self:with_indent(function()
            self:emit_block(arm.body)
          end)
          self:emit("end")
          first = false
        else
          self:emit("else")
          self:with_indent(function()
            self:emit_block(arm.body)
          end)
          self:emit("end")
        end
      else
        error("Unsupported match pattern kind: " .. tostring(arm.pattern.kind))
      end
    end
    if not first then
      -- if/elseif/else already emitted end in branches except last elseif case
      -- Ensure an end if last branch wasn't wildcard
      local has_wild = stmt.hasWildcard or false
      if not has_wild then
        self:emit("end")
      end
    end
  elseif k == "Use" then
    local parts = {}
    for i, p in ipairs(stmt.parts) do
      if not (i == 1 and (p == "crate" or p == "self")) then
        table.insert(parts, p)
      end
    end
    local modname = table.concat(parts, ".")
    local localname = stmt.alias or (parts[#parts] or stmt.parts[#stmt.parts])
    if localname then
      self:emit(string.format("local %s = require(%q)", localname, modname))
    else
      self:emit("-- use (ignored): " .. table.concat(stmt.parts, "::"))
    end
  elseif k == "Const" then
    local line = ("%s = %s"):format(stmt.name, self:emit_expression(stmt.value))
    if stmt.isPublic and self._inside_module then
      self:emit("M." .. line)
    else
      self:emit("local " .. line)
    end
  elseif k == "ExprStmt" then
    local code = self:emit_expression(stmt.expression)
    self:emit(code)
  else
    error("Unknown statement kind: " .. tostring(k))
  end
end

function Emitter:emit_expression(expr)
  local k = expr.kind
  if k == "Literal" then
    if expr.literalType == "string" then
      return string.format("%q", expr.value)
    elseif expr.literalType == "boolean" then
      return expr.value and "true" or "false"
    else
      return tostring(expr.value)
    end
  elseif k == "Identifier" then
    return expr.name
  elseif k == "Unary" then
    local op = to_lua_operator(expr.operator)
    if op == "not" then
      return "not " .. self:emit_expression(expr.argument)
    else
      return op .. self:emit_expression(expr.argument)
    end
  elseif k == "Binary" then
    local op = to_lua_operator(expr.operator)
    return "(" .. self:emit_expression(expr.left) .. " " .. op .. " " .. self:emit_expression(expr.right) .. ")"
  elseif k == "Call" then
    local args = {}
    for _, a in ipairs(expr.arguments) do table.insert(args, self:emit_expression(a)) end
    return self:emit_expression(expr.callee) .. "(" .. table.concat(args, ", ") .. ")"
  elseif k == "Member" then
    return self:emit_expression(expr.object) .. "." .. expr.property
  elseif k == "Index" then
    return self:emit_expression(expr.object) .. "[" .. self:emit_expression(expr.index) .. "]"
  elseif k == "ArrayLiteral" then
    local parts = {}
    for _, e in ipairs(expr.elements) do table.insert(parts, self:emit_expression(e)) end
    return "{" .. table.concat(parts, ", ") .. "}"
  else
    error("Unknown expression kind: " .. tostring(k))
  end
end

return Emitter


]] local _load = loadstring or load local _fn, _err = _load(_src, '@'.."Rulu.lib.emitter") if not _fn then error(_err) end return _fn(...) end
preload["Rulu.rulu"] = function(... ) local _src = [=[-- Made by 0x1000007e

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

  if not BUNDLED and (args.print or not args.out) then
    io.write(lua_code)
  end

  if (not BUNDLED and args.run) or (BUNDLED and args.input) then
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


]=] local _load = loadstring or load local _fn, _err = _load(_src, '@'.."Rulu.rulu") if not _fn then error(_err) end return _fn(...) end
preload["Rulu.lib.checker"] = function(... ) local _src = [[-- Made by 0x1000007e

-- rulu: Semantic checker
-- Enforces simple rules like immutable bindings (no assign without `mut`)

local Checker = {}

local function new_scope(parent)
  return { parent = parent, symbols = {} }
end

local function declare(scope, name, props)
  if scope.symbols[name] then
    error("Redeclaration of '" .. name .. "' in the same scope")
  end
  scope.symbols[name] = props
end

local function lookup(scope, name)
  local s = scope
  while s do
    if s.symbols[name] then return s.symbols[name] end
    s = s.parent
  end
  return nil
end

local function check_block(block, scope, visit_stmt)
  local inner = new_scope(scope)
  for _, stmt in ipairs(block.body) do
    visit_stmt(stmt, inner)
  end
end

function Checker.check(ast)
  local function visit_expr(_) end -- placeholder for future type checks

  local function visit_stmt(stmt, scope)
    local k = stmt.kind
    if k == "Function" then
      local fn_scope = new_scope(scope)
      for _, p in ipairs(stmt.params or {}) do
        declare(fn_scope, p.name, { mutable = false, kind = "param" })
      end
      check_block(stmt.body, fn_scope, visit_stmt)
    elseif k == "Let" then
      declare(scope, stmt.name, { mutable = stmt.mutable and true or false, kind = "var" })
      if stmt.init then visit_expr(stmt.init) end
    elseif k == "Assign" then
      local sym = lookup(scope, stmt.name)
      if not sym then
        error("Assignment to undeclared variable '" .. stmt.name .. "'")
      end
      if not sym.mutable then
        error("Cannot assign to immutable variable '" .. stmt.name .. "'")
      end
      visit_expr(stmt.value)
    elseif k == "Return" then
      if stmt.argument then visit_expr(stmt.argument) end
    elseif k == "If" then
      visit_expr(stmt.test)
      check_block(stmt.consequent, scope, visit_stmt)
      if stmt.alternate then check_block(stmt.alternate, scope, visit_stmt) end
    elseif k == "While" then
      visit_expr(stmt.test)
      check_block(stmt.body, scope, visit_stmt)
    elseif k == "ForRange" then
      local loop_scope = new_scope(scope)
      declare(loop_scope, stmt.var, { mutable = false, kind = "loop" })
      check_block(stmt.body, loop_scope, visit_stmt)
    elseif k == "ForEach" then
      visit_expr(stmt.iterExpr)
      local loop_scope = new_scope(scope)
      declare(loop_scope, stmt.var, { mutable = false, kind = "loop" })
      check_block(stmt.body, loop_scope, visit_stmt)
    elseif k == "Loop" then
      check_block(stmt.body, scope, visit_stmt)
    elseif k == "Break" or k == "Continue" then
      -- ok
    elseif k == "Match" then
      visit_expr(stmt.discriminant)
      for _, arm in ipairs(stmt.arms or {}) do
        check_block(arm.body, scope, visit_stmt)
      end
    elseif k == "Use" then
      -- ignore for now
    elseif k == "Const" then
      declare(scope, stmt.name, { mutable = false, kind = "const" })
    elseif k == "ExprStmt" then
      visit_expr(stmt.expression)
    else
      error("Unknown statement kind in checker: " .. tostring(k))
    end
  end

  local root = new_scope(nil)
  for _, stmt in ipairs(ast.body or {}) do
    visit_stmt(stmt, root)
  end
end

return Checker


]] local _load = loadstring or load local _fn, _err = _load(_src, '@'.."Rulu.lib.checker") if not _fn then error(_err) end return _fn(...) end
local code = require('Rulu.rulu') or 0
if type(code) == 'number' then os.exit(code) end
