import macros, options

template openDefine*(lhs, rhs: NimNode): NimNode =
  newCall(bindSym("define", brOpen), lhs, rhs)

proc defaultDefine*(lhs, rhs: NimNode): NimNode =
  if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and lhs[0].eqIdent"mut":
    result = newVarStmt(lhs[1], rhs)
  else:
    result = newLetStmt(lhs, rhs)

macro define*[T: auto](lhs; rhs: T): untyped =
  if lhs.kind == nnkPar:
    let tmp = genSym(nskLet, "tmpTuple")
    result = newStmtList(newLetStmt(tmp, rhs))
    for i, name in lhs.pairs:
      if name.kind == nnkExprColonExpr:
        result.add(openDefine(name[1], newDotExpr(tmp, name[0])))
      else:
        result.add(openDefine(name, newTree(nnkBracketExpr, tmp, newLit(i))))
  else:
    result = defaultDefine(lhs, rhs)

macro define*[T](lhs; rhs: Option[T]): untyped =
  if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
    result = openDefine(lhs[1], newCall(bindSym"get", rhs))
  else:
    result = defaultDefine(lhs, rhs)

macro def*(body): untyped =
  if body.kind == nnkStmtList:
    result = newStmtList()
    for a in body:
      let
        lhs = a[0]
        rhs = a[1]
      result.add(openDefine(lhs, rhs))
  else:
    let
      lhs = body[0]
      rhs = body[1]
    result = openDefine(lhs, rhs)

macro `:=`*(a, b): untyped =
  result = openDefine(a, b)