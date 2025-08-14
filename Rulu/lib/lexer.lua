-- Made by 0x1000007e

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


