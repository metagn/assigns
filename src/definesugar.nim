import macros, options

template openDefine*(lhs, rhs: NimNode): NimNode =
  newCall(bindSym("define", brForceOpen), lhs, rhs)

# needed for openDefine to work
proc defaultDefine*(lhs, rhs: NimNode): NimNode

macro define*[T](lhs; rhs: T): untyped =
  result = defaultDefine(lhs, rhs)

proc defaultDefine*(lhs, rhs: NimNode): NimNode =
  if lhs.kind == nnkPar:
    let tmp = genSym(nskLet, "tmpPar")
    result = newStmtList(newLetStmt(tmp, rhs))
    # TODO: allow (head, *rest) syntax
    for i, name in lhs.pairs:
      if name.kind == nnkExprColonExpr:
        result.add(openDefine(name[1], newDotExpr(tmp, name[0])))
      else:
        result.add(openDefine(name, newTree(nnkBracketExpr, tmp, newLit(i))))
  elif lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and lhs[0].eqIdent"mut":
    result = newVarStmt(lhs[1], rhs)
  elif lhs.kind == nnkInfix and lhs[0].eqIdent"as":
    let tmp = genSym(nskLet, "tmpAs")
    result = newStmtList(newLetStmt(tmp, rhs))
    var last = lhs
    while true:
      result.add(openDefine(last[2], tmp))
      last = last[1]
      if not (last.kind == nnkInfix and last[0].eqIdent"as"):
        result.add(openDefine(last, tmp))
        break
  else:
    result = newLetStmt(lhs, rhs)

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