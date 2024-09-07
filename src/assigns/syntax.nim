import std/macros, ./impl

proc doDef(a: NimNode): NimNode =
  var
    a = a
    kind = akLet
  if a.eqIdent"_": return newEmptyNode()
  if a.kind in nnkCallKinds and a[0].eqIdent"mut":
    a = a[1]
    kind = akVar
  let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
  result = openAssign(lhs, rhs)

macro def*(assignments): untyped =
  ## Goes through each assignment expression in `assignments` and processes them into definitions.
  runnableExamples:
    def:
      (a, b) = (1, 2)
      c as d = 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  if assignments.kind == nnkStmtList:
    result = newStmtList()
    for a in assignments:
      result.add(doDef(a))
  else:
    result = doDef(assignments)

macro def*(assignments, body): untyped =
  ## Goes through each assignment expression in `assignments` and processes them into definitions.
  ## These definitions are then put into a new scope where `body` is evaluated.
  ##
  ## Example:
  ##
  ## .. code-block::nim
  ##  var a = 3
  ##  def:
  ##    a = 1
  ##  do:
  ##    doAssert a == 1
  ##  doAssert a == 3
  result = newStmtList()
  if assignments.kind == nnkStmtList:
    for a in assignments:
      result.add(doDef(a))
  else:
    result.add(doDef(assignments))
  result.add(body)
  result = newBlockStmt(result)

macro `:=`*(a, b): untyped =
  ## Unpacks `b` with the given description `a`.
  runnableExamples:
    (a, b) := (1, 2)
    c as d := 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  result = openAssign(a, b)

template `:=?`*(a, b): bool =
  ## Executes (!) ``a := b`` and returns false if the checks in the assignment
  ## fail. Otherwise returns `true`.
  ## Note that the executed ``a := b`` will not have any
  ## affect on the scope of the following statements since
  ## it is in a `block` statement.
  runnableExamples:
    doAssert (a, b) :=? (1, 2)
    import options
    let a = none(int)
    doAssert not (some(n) :=? a)
  block match:
    var passed = false
    block success:
      template assignCheckBreakpoint(checkBody) {.redefine, used.} =
        passed = false
        break success
      `a` := `b`
      template assignCheckBreakpoint(checkBody) {.redefine, used.} =
        checkBody
      passed = true
    passed

template `:=?`*(a, b, body): untyped =
  ## Executes `body` if ``a := b`` doesn't give a runtime error.
  ## `body` will be in the same scope as the definition ``a := b``.
  ##
  ## Example:
  ##
  ## .. code-block::nim
  ##  import options
  ##  let a = some(3)
  ##  some(n) :=? a:
  ##    doAssert n == 3
  block match:
    template assignCheckBreakpoint(checkBody) {.redefine, used.} =
      break match
    `a` := `b`
    template assignCheckBreakpoint(checkBody) {.redefine, used.} =
      checkBody
    `body`

macro `:=?`*(a, b, body, elseBranch): untyped =
  ## Executes `body` if ``a := b`` doesn't fail any checks,
  ## otherwise executes `elseBranch`.
  ## `body` will be in the same scope as the definition ``a := b``.
  ## 
  ## Uses `break` instead of exceptions for handling check failures unilke
  ## `tryAssign`. Due to Nim limitations, this means this cannot be used
  ## as an expression and must be a statement.
  ##
  ## Example:
  ##
  ## .. code-block::nim
  ##  import options
  ##  let a = some(3)
  ##  some(n) :=? a:
  ##    doAssert n == 3
  ##  else:
  ##    doAssert false
  let elseExpr = if elseBranch.kind == nnkElse: elseBranch[0] else: elseBranch
  result = quote:
    block match:
      block success:
        template assignCheckBreakpoint(checkBody) {.redefine, used.} =
          break success
        `a` := `b`
        template assignCheckBreakpoint(checkBody) {.redefine, used.} =
          checkBody
        `body`
        break match
      `elseExpr`

macro tryAssign*(a, b, body, elseBranch): untyped =
  ## Executes `body` if ``a := b`` doesn't fail any checks,
  ## otherwise executes `elseBranch`.
  ## `body` will be in the same scope as the definition ``a := b``.
  ## 
  ## Uses exceptions instead of `break` unlike `:=?`. This allows it to be
  ## used as an expression but has a runtime cost.
  ##
  ## Example:
  ##
  ## .. code-block::nim
  ##  import options
  ##  let a = some(3)
  ##  tryAssign some(n), a:
  ##    doAssert n == 3
  ##  else:
  ##    doAssert false
  let elseExpr = if elseBranch.kind == nnkElse: elseBranch[0] else: elseBranch
  result = quote:
    var assignFinished = false
    try:
      `a` := `b`
      assignFinished = true
      `body`
    except AssignError:
      if assignFinished:
        raise
      else:
        `elseExpr`

macro tryAssign*(a, b, c): untyped =
  ## Version of `tryAssign` with either no `else` block, or with an infix
  ## assignment as the first argument.
  ## 
  ## Example:
  ##
  ## .. code-block::nim
  ##  import options
  ##  let a = some(3)
  ##  tryAssign some(n) := a: # or = a
  ##    doAssert n == 3
  ##  else:
  ##    doAssert false
  ## 
  ##  tryAssign some(n), a:
  ##    doAssert n == 3
  if a.kind == nnkInfix and a[0].eqIdent":=":
    let x = a[1]
    let y = a[2]
    result = getAst(tryAssign(x, y, b, c))
  elif a.kind in {nnkAsgn, nnkExprEqExpr}:
    let x = a[0]
    let y = a[1]
    result = getAst(tryAssign(x, y, b, c))
  else:
    let disc = newTree(nnkDiscardStmt, newEmptyNode())
    result = getAst(tryAssign(a, b, c, disc))

macro tryAssign*(a, b): untyped =
  ## Version of `tryAssign` with an infix assignment as the first argument
  ## and no `else` block.
  ## 
  ## Example:
  ##
  ## .. code-block::nim
  ##  import options
  ##  let a = some(3)
  ##  tryAssign some(n) := a: # or = a
  ##    doAssert n == 3
  if a.kind == nnkInfix and a[0].eqIdent":=":
    let x = a[1]
    let y = a[2]
    result = getAst(tryAssign(x, y, b))
  elif a.kind in {nnkAsgn, nnkExprEqExpr}:
    let x = a[0]
    let y = a[1]
    result = getAst(tryAssign(x, y, b))
  else:
    let t = newLit(true)
    let f = newLit(false)
    result = getAst(tryAssign(a, b, t, f))

template tryAssign*(a): untyped =
  ## Version of `tryAssign` that returns `false` if the assignment failed
  ## and `true` otherwise.
  ## Note that the executed ``a := b`` will not have any
  ## affect on the scope of the following statements since
  ## it is in a `try` statement.
  tryAssign(a, true, false)

macro unpackArgs*(args, routine): untyped =
  ## Injects unpacking assignments into the body of a given routine.
  runnableExamples:
    proc foo(tup: (int, string)) {.unpackArgs: [(a, b): tup].} =
      doAssert a == b.len
    let t = (3, "abc")
    foo(t)
  case routine.kind
  of nnkStmtList:
    result = newStmtList()
    for r in routine:
      result.add(getAst(unpackArgs(args, r)))
  of RoutineNodes:
    let bod = block:
      if routine[^1].kind != nnkStmtList:
        routine[^1] = newStmtList(routine[^1])
      routine[^1]
    for a in args:
      case a.kind
      of nnkInfix:
        if a[0].eqIdent("<-"):
          bod.insert(0, openAssign(a[2], a[1]))
        else:
          bod.insert(0, openAssign(a[1], a[2]))
      of nnkExprColonExpr, nnkExprEqExpr, nnkAsgn:
        bod.insert(0, openAssign(a[0], a[1]))
      else:
        error("unrecognized unpacking expression for unpackArgs with kind " & $a.kind, a)
    result = routine
  else:
    error("unrecognized routine expression for unpackArgs with kind " & $routine.kind, routine)

macro match*(val: untyped, branches: varargs[untyped]): untyped =
  ## Naive pattern matching implementation based on `:=?`. Has the same
  ## limitation as `:=?`, which is that it cannot be used as an expression.
  runnableExamples:
    proc fizzbuzz(n: int): string =
      match (n mod 3, n mod 5):
      of (0, 0): result = "FizzBuzz"
      of (0, _): result = "Fizz"
      of (_, 0): result = "Buzz"
      else: result = $n
    for i in 1..100:
      echo fizzbuzz(i)
  result = newEmptyNode()
  for i in countdown(branches.len - 1, 0):
    let b = branches[i]
    case b.kind
    of nnkElse:
      result = b[0]
    of nnkElifBranch:
      let cond = b[0]
      let bod = b[1]
      result = quote do:
        if `cond`:
          `bod`
        else:
          `result`
    of nnkOfBranch:
      let bod = b[^1]
      for vi in 0..<b.len - 1:
        let v = b[vi]
        result = getAst(`:=?`(v, val, bod, result))
    else:
      error("invalid branch for match", b)

macro tryMatch*(val: untyped, branches: varargs[untyped]): untyped =
  ## Naive pattern matching implementation based on `tryAssign`. Has the same
  ## caveat of `tryAssign`, which is that it uses exceptions.
  runnableExamples:
    proc fizzbuzz(n: int): string =
      tryMatch (n mod 3, n mod 5):
      of (0, 0): "FizzBuzz"
      of (0, _): "Fizz"
      of (_, 0): "Buzz"
      else: $n
    for i in 1..100:
      echo fizzbuzz(i)
  result = newEmptyNode()
  for i in countdown(branches.len - 1, 0):
    let b = branches[i]
    case b.kind
    of nnkElse:
      result = b[0]
    of nnkElifBranch:
      let cond = b[0]
      let bod = b[1]
      result = quote do:
        if `cond`:
          `bod`
        else:
          `result`
    of nnkOfBranch:
      let bod = b[^1]
      for vi in 0..<b.len - 1:
        let v = b[vi]
        result = getAst(tryAssign(v, val, bod, result))
    else:
      error("invalid branch for tryMatch", b)
