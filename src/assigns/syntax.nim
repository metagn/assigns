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
  ## Executes (!) ``a := b`` and returns false if it gives a runtime error.
  ## Otherwise returns `true`.
  ## Note that the executed ``a := b`` will not have any
  ## affect on the scope of the following statements since
  ## it uses a `try` statement.
  runnableExamples:
    doAssert (a, b) :=? (1, 2)
    import options
    let a = none(int)
    doAssert not (some(n) :=? a)
  try:
    a := b
    true
  except AssignError:
    false

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
  var assignFinished = false
  try:
    a := b
    assignFinished = true
    body
  except AssignError:
    if assignFinished:
      raise

macro `:=?`*(a, b, body, elseBranch): untyped =
  ## Executes `body` if ``a ::= b`` doesn't give a runtime error,
  ## otherwise executes `elseBranch`.
  ## `body` will be in the same scope as the definition ``a := b``.
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
  ## Naive pattern matching implementation based on `:=?`.
  runnableExamples:
    proc fizzbuzz(n: int): string =
      match (n mod 3, n mod 5):
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
        result = getAst(`:=?`(v, val, bod, result))
    else:
      error("invalid branch for match", b)
