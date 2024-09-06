import macros, impl

proc trySimpleForVar(n: NimNode, tupleNest = 0): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
    result = ident $n
  of nnkPragmaExpr:
    let val = trySimpleForVar(n[0])
    if not val.isNil:
      result = copy n
      result[0] = val
  of nnkPar, nnkTupleConstr:
    if tupleNest > 1: return nil
    result = newNimNode(nnkVarTuple, n)
    for i in 0 ..< n.len:
      let val = trySimpleForVar(n[i], tupleNest + 1)
      if val == nil: return nil
      result.add(val)
    result.add(newEmptyNode())
  else: result = nil

type LhsContext = enum
  None, Tuple, Array

proc lhsToVal(n: NimNode, context = None): NimNode =
  case n.kind
  of nnkIdent:
    result = n
  of nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
    result = ident $n
  of nnkPar, nnkTupleConstr, nnkBracket:
    var current = newNimNode(n.kind, n)
    let context = if n.kind == nnkBracket: Array else: Tuple
    for i in 0 ..< n.len:
      let val = lhsToVal(n[i], context)
      if val == nil: return nil
      if val.kind == nnkDerefExpr:
        let val = val[0]
        if current.len == 0:
          result = val
        else:
          result = infix(current, "&", val)
        current = newNimNode(n.kind, n)
        result = infix(result, "&", current)
      else:
        current.add val
    if result.isNil:
      result = current
  of nnkLiterals:
    result = n
  of nnkPrefix:
    let a = $n[0]
    case a
    of "@", "^", "==":
      result = lhsToVal(n[1])
    of "*", "..", "...":
      if context in {Tuple, Array}:
        result = newTree(nnkDerefExpr, n[1])
      else:
        result = nil
    else:
      result = nil
  of nnkInfix:
    let a = $n[0]
    case a
    of "is", "of":
      result = lhsToVal(n[1], context)
    of "as", ":=":
      result = lhsToVal(n[2], context)
      if result.isNil:
        result = lhsToVal(n[1], context)
    else:
      result = nil
  of nnkExprColonExpr:
    case context
    of None: result = nil
    of Tuple:
      if n[0].kind in {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
        let val = lhsToVal(n[1])
        if val.isNil: return nil
        result = newNimNode(nnkExprColonExpr)
        result.add ident $n[0]
        result.add val
      else:
        result = nil
    of Array:
      if n[0].kind in {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
        let val = lhsToVal(n[1])
        if val.isNil: return nil
        result = newNimNode(nnkExprColonExpr)
        result.add ident $n[0]
        result.add val
      elif n[0].kind in nnkCharLit..nnkUInt64Lit:
        let val = lhsToVal(n[1])
        if val.isNil: return nil
        result = newNimNode(nnkExprColonExpr)
        result.add n[0]
        result.add val
      else:
        result = nil
  else:
    result = nil

proc breakingBreakpoint(label: NimNode): NimNode =
  # template assignCheckBreakpoint(body) = break tapSuccess
  newProc(
    name = ident"assignCheckBreakpoint",
    params = [newEmptyNode(), newIdentDefs(ident"body", ident"untyped")],
    body = newTree(nnkBreakStmt, label),
    procType = nnkTemplateDef,
    pragmas = newTree(nnkPragma, ident"redefine", ident"used"))

proc defaultBreakpoint(): NimNode =
  # template assignCheckBreakpoint(body) = body
  newProc(
    name = ident"assignCheckBreakpoint",
    params = [newEmptyNode(), newIdentDefs(ident"body", ident"untyped")],
    body = ident"body",
    procType = nnkTemplateDef,
    pragmas = newTree(nnkPragma, ident"redefine", ident"used"))

proc transformTap(e: NimNode, body, elseBody, successLabel: NimNode): NimNode =
  if e.kind in nnkCallKinds and e[0].eqIdent"in" and e.len == 3:
    let forVal = e[1]
    let forValSimple = trySimpleForVar(forVal)
    if forValSimple.isNil:
      let forTemp = genSym(nskForVar, "tmpTapFor")
      result = newNimNode(nnkForStmt, e)
      result.add(forTemp)
      result.add(e[2])
      result.add(newStmtList(newCall(bindSym"openAssign", forVal, forTemp)))
    else:
      result = newNimNode(nnkForStmt, e)
      if forValSimple.kind == nnkVarTuple:
        for i in 0 ..< forValSimple.len - 1:
          result.add(forValSimple[i])
      else:
        result.add(forValSimple)
      result.add(e[2])
      result.add(body)
  elif e.kind in nnkCallKinds and e[0].eqIdent"result" and e.len == 2:
    var val = e[1]
    if val.kind in {nnkAsgn, nnkExprEqExpr}:
      let a = val[0]
      let b = val[1]
      val = newNimNode(nnkInfix, val)
      val.add(ident":=")
      val.add(a)
      val.add(b)
    if val.kind in nnkCallKinds and (val[0].eqIdent":=" or val[0].eqIdent":=?" or val[0].eqIdent"=?"):
      let lhsVal = lhsToVal(val[1])
      if lhsVal.isNil:
        error("cannot get result value from " & val[1].repr, val[1])
      else:
        val[1] = newTree(nnkVarTy, val[1])
        result = newStmtList(transformTap(val, body, elseBody, successLabel), lhsVal)
    else:
      result = newStmtList(body, val)
  elif e.kind in nnkCallKinds and e[0].eqIdent"filter" and e.len == 2:
    result = newNimNode(nnkIfStmt, e)
    var branch = newNimNode(nnkElifBranch, e[1])
    branch.add(e[1])
    branch.add(body)
    result.add(branch)
  elif e.kind in {nnkAsgn, nnkExprEqExpr}:
    result = newNimNode(nnkInfix, e)
    result.add(ident":=")
    result.add(e[0])
    result.add(e[1])
    result = newStmtList(result, body)
  elif e.kind in nnkCallKinds and (e[0].eqIdent":=?" or e[0].eqIdent"=?") and e.len == 3:
    result = newNimNode(nnkInfix, e)
    result.add(ident":=")
    result.add(e[1])
    result.add(e[2])
    result = newStmtList(breakingBreakpoint(successLabel), result, defaultBreakpoint(), body)
  elif e.kind == nnkStmtList:
    result = body
    for i in countdown(e.len - 1, 0):
      result = transformTap(e[i], result, elseBody, successLabel)
  else:
    result = newStmtList(e, body)

proc tapImpl(nodes: NimNode): NimNode =
  var finalIndex = nodes.len - 1
  var value = nodes[finalIndex]
  var finallyBody: NimNode = nil
  if value.kind == nnkFinally:
    finallyBody = value
    dec finalIndex
    value = nodes[finalIndex]
  var exceptBranches: seq[NimNode]
  while value.kind == nnkExceptBranch:
    exceptBranches.insert(value, 0)
    dec finalIndex
    value = nodes[finalIndex]
  var elseBody: NimNode = nil
  if value.kind in {nnkElse, nnkElseExpr}:
    elseBody = value[0]
    dec finalIndex
    value = nodes[finalIndex]
  if value.kind in {nnkElifExpr, nnkElifBranch}:
    let kind = if value.kind == nnkElifExpr: nnkIfExpr else: nnkIfStmt
    if elseBody.isNil:
      elseBody = newNimNode(kind, value)
    else:
      let newElse = newNimNode(elseBody.kind, elseBody)
      newElse.add elseBody
      elseBody = newNimNode(kind, elseBody)
      elseBody.add newElse
    while value.kind in {nnkElifExpr, nnkElifBranch}:
      elseBody.insert(0, value)
      dec finalIndex
      value = nodes[finalIndex]
  let topLabel = genSym(nskLabel, "tap")
  let successLabel = if elseBody.isNil: topLabel else: genSym(nskLabel, "tapSuccess")
  result = value
  for i in countdown(finalIndex - 1, 0):
    result = transformTap(nodes[i], result, elseBody, successLabel)
  if not elseBody.isNil:
    result = newBlockStmt(successLabel,
      newStmtList(
        result,
        newTree(nnkBreakStmt, topLabel)))
    result = newStmtList(result, elseBody)
  result = newBlockStmt(topLabel, result)
  if exceptBranches.len != 0 or not finallyBody.isNil:
    result = newTree(nnkTryStmt, result)
    for x in exceptBranches: result.add(x)
    if not finallyBody.isNil: result.add(finallyBody)

macro tap*(nodes: varargs[untyped]): untyped =
  result = tapImpl(nodes)
