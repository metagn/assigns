import macros, options, definesugar/tupleaccess

export tupleaccess

type DefineKind* = enum
  dkLet, dkVar, dkAssign

template openDefine*(lhs, rhs: NimNode, dk: DefineKind = dkLet): NimNode =
  ## Creates a node that calls `define` again with `lhs` and `rhs`
  ## 
  if rhs.kind == nnkVarTy:
    newCall(bindSym("define", brForceOpen), lhs, rhs[0], newLit dkVar)
  else:
    newCall(bindSym("define", brForceOpen), lhs, rhs, newLit dk)

# needed for openDefine to work
proc defaultDefine*(lhs, rhs: NimNode, kind = dkLet): NimNode

macro define*[T](lhs; rhs: T, kind: static[DefineKind]): untyped =
  result = defaultDefine(lhs, rhs, kind)

proc defaultDefine*(lhs, rhs: NimNode, kind = dkLet): NimNode =
  proc defstmt(a, b: NimNode): NimNode =
    case kind
    of dkVar: newVarStmt(a, b)
    of dkLet: newLetStmt(a, b)
    of dkAssign: newAssignment(a, b)
  if lhs.kind == nnkPar:
    let tmp = genSym(nskLet, "tmpPar")
    result = newStmtList(newLetStmt(
      if lhs.len == 0:
        newTree(nnkPragmaExpr, tmp, newTree(nnkPragma, ident"used"))
      else:
        tmp, rhs))
    # TODO: allow (head, *rest) syntax
    var toIndex: seq[NimNode]
    var spreadIndex = -1
    for i, name in lhs.pairs:
      if name.kind == nnkExprColonExpr:
        result.add(openDefine(name[1],
          if name[0].kind == nnkIdent:
            newDotExpr(tmp, name[0])
          else:
            newTree(nnkBracketExpr, tmp, name[0]), kind))
      elif name.kind == nnkPrefix and name[0].eqIdent"*":
        if spreadIndex >= 0:
          error("cannot have 2 spread defines", name)
        spreadIndex = toIndex.len
        toIndex.add(name[1])
      else:
        toIndex.add(name)
    for i, x in toIndex:
      let index =
        if spreadIndex == -1 or i < spreadIndex:
          newLit(i)
        elif i == spreadIndex:
          infix(newLit i, "..", prefix(newLit(toIndex.len - i), "^"))
        else:
          prefix(newLit(toIndex.len - i), "^")
      let o = openDefine(x, newTree(nnkBracketExpr, tmp, index), kind)
      result.add(o)
  elif lhs.kind in nnkLiterals:
    result = newCall("assert", infix(lhs, "==", rhs))
  elif lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and lhs[0].eqIdent"mut":
    result = defaultDefine(lhs[1], rhs, dkVar)
  elif lhs.kind == nnkInfix and lhs[0].eqIdent"as":
    let tmp = genSym([nskLet, nskVar][int kind], "tmpAs")
    result = newStmtList(defstmt(tmp, rhs))
    var last = lhs
    while true:
      result.add(openDefine(last[2], tmp, kind))
      last = last[1]
      if not (last.kind == nnkInfix and last[0].eqIdent"as"):
        result.add(openDefine(last, tmp, kind))
        break
  else:
    result = defstmt(lhs, rhs)

macro define*[T](lhs; rhs: Option[T], kind: static[DefineKind] = dkLet): untyped =
  if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
    result = openDefine(lhs[1], newCall(bindSym"get", rhs), kind)
  else:
    result = defaultDefine(lhs, rhs, kind)

template implementDefine*(T; body) {.dirty.} =
  macro define(lhs; rhs: T, kind: static[DefineKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openDefine(a, b, kind)
    template open(a, b: NimNode, isReallyVar: static[bool]): NimNode {.used.} = openDefine(a, b, isReallyVar)
    template default(a, b: NimNode): NimNode {.used.} = defaultDefine(a, b, kind)
    template default(a, b: NimNode, isReallyVar: static[bool]): NimNode {.used.} = defaultDefine(a, b, isReallyVar)
    body

template implementDefineExported*(T; body) {.dirty.} =
  macro define*(lhs; rhs: T, kind: static[DefineKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openDefine(a, b, kind)
    template open(a, b: NimNode, isReallyVar: static[bool]): NimNode {.used.} = openDefine(a, b, isReallyVar)
    template default(a, b: NimNode): NimNode {.used.} = defaultDefine(a, b, kind)
    template default(a, b: NimNode, isReallyVar: static[bool]): NimNode {.used.} = defaultDefine(a, b, isReallyVar)
    body

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

macro `::=`*(a, b): untyped =
  result = openDefine(a, b, dkAssign)