-- Made by 0x1000007e

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
  elseif self:is("KW", "pub") and self:peek(1) and self:peek(1).type == "KW" and self:peek(1).value == "fn" then
    return self:parse_function()
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
  local start_expr, end_expr
  if self:match("OP", "..") then
    start_expr = range_expr
    end_expr = self:parse_expression()
  elseif range_expr.kind == "Binary" and range_expr.operator == ".." then
    start_expr = range_expr.left
    end_expr = range_expr.right
  else
    -- for v in expr { ... }
    local body = self:parse_block()
    return node("ForEach", { var = iter, iterExpr = range_expr, body = body })
  end
  local body = self:parse_block()
  return node("ForRange", { var = iter, startExpr = start_expr, endExpr = end_expr, body = body })
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
  -- const/ static NAME = expr;
  local kind = self:current().value
  self.pos = self.pos + 1
  local name = self:expect("IDENT", nil, "Name expected").value
  if self:match("COLON") then
    if self:is("IDENT") or self:is("KW") then self.pos = self.pos + 1 end
  end
  self:expect("OP", "=")
  local value = self:parse_expression()
  self:expect("SEMI")
  return node("Const", { storage = kind, name = name, value = value })
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


