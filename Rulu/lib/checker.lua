-- Made by 0x1000007e

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


