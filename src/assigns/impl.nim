## The `assign` macro in this module is overloadable, and the `openAssign` template creates a
## NimNode that calls a forced open symbol of `assign` with the AST of the left hand side,
## AST of the right hand side, and the flag of whether or not it is a `let`, `var`, or mutating
## assignment. You can use the `implementAssign` and `implementAssignExported` templates as a
## shorthand for declaring these overloads.

import std/[macros, options], ./tupleindex

type AssignKind* = enum
  ## Propagated flag of how definitions should create assignments.
  akLet, akVar, akSet

type
  AssignError* = object of CatchableError
    ## any kind of check error in assignments
  AssignEqualityError* = object of AssignError
    ## error for failed equality checks in assignments
  AssignContainsError* = object of AssignError
    ## error for failed contains checks in assignments
  AssignBoundError* = object of AssignError
    ## error for failed bound checks in assignments

template assignCheckEqual*(a, b): untyped =
  ## template for equality checks in assignments
  if a != b:
    raise newException(AssignEqualityError, "expected " & astToStr(b) & ", got " & astToStr(a))

template assignCheckNotEqual*(a, b): untyped =
  ## template for non-equality checks in assignments
  if a == b:
    raise newException(AssignEqualityError, "did not expect " & astToStr(b) & ", got " & astToStr(a))

template assignCheckType*(a, b): untyped =
  ## template for type checks in assignments
  when a isnot b:
    {.error: "type of " & astToStr(a) & " was " & $typeof(a) & ", not " & astToStr(b).}

template assignCheckNotType*(a, b): untyped =
  ## template for non-type checks in assignments
  when a is b:
    {.error: "type of " & astToStr(a) & " was not " & $typeof(a) & ", was " & astToStr(b).}

template assignCheckContains*(a, b): untyped =
  ## template for equality checks in assignments
  if a notin b:
    raise newException(AssignContainsError, "expected " & astToStr(a) & " to be in " & astToStr(b))

template assignCheckNotContains*(a, b): untyped =
  ## template for non-equality checks in assignments
  if a in b:
    raise newException(AssignContainsError, "did not expect " & astToStr(a) & " to be in " & astToStr(b))

template assignCheckLess*(a, b): untyped =
  ## template for non-equality checks in assignments
  if not (a < b):
    raise newException(AssignBoundError, "expected " & astToStr(a) & " to be less than " & astToStr(b))

template assignCheckLessEqual*(a, b): untyped =
  ## template for non-equality checks in assignments
  if not (a <= b):
    raise newException(AssignBoundError, "expected " & astToStr(a) & " to be less than or equal to " & astToStr(b))

template assignCheckGreater*(a, b): untyped =
  ## template for non-equality checks in assignments
  if not (a > b):
    raise newException(AssignBoundError, "expected " & astToStr(a) & " to be greater than " & astToStr(b))

template assignCheckGreaterEqual*(a, b): untyped =
  ## template for non-equality checks in assignments
  if not (a >= b):
    raise newException(AssignBoundError, "expected " & astToStr(a) & " to be greater than or equal to " & astToStr(b))

template openAssign*(lhs, rhs: NimNode, ak: AssignKind = akLet): NimNode =
  ## Creates a node that calls an open symbol `assign` with `lhs` and `rhs`.
  if rhs.kind == nnkVarTy:
    newCall(bindSym("assign", brForceOpen), lhs, rhs[0], newLit akVar)
  else:
    newCall(bindSym("assign", brForceOpen), lhs, rhs, newLit ak)

# needs to be here for openAssign to work
proc defaultAssign*(lhs, rhs: NimNode, kind = akLet): NimNode
  ## The default interpretation of a definition.

macro assign*[T](lhs; rhs: T, kind: static AssignKind): untyped =
  ## Handles definitions for a type `T`.
  ## Can be overloaded, optionally via the convenience macros
  ## `implementAssign` and `implementAssignExported`. 
  result = defaultAssign(lhs, rhs, kind)

from strutils import toLowerAscii

proc identStr(s: string): string =
  result = newStringOfCap(s.len)
  for c in s:
    if c != '_':
      result.add(toLowerAscii(c))

proc defaultAssign*(lhs, rhs: NimNode, kind = akLet): NimNode =
  proc defstmt(a, b: NimNode): NimNode =
    case kind
    of akVar: newVarStmt(a, b)
    of akLet: newLetStmt(a, b)
    of akSet: newAssignment(a, b)
  case lhs.kind
  of nnkPar, nnkTupleConstr, nnkBracket:
    let tmp = genSym(nskLet, "tmpPar")
    result = newStmtList(newLetStmt(
      if lhs.len == 0:
        newTree(nnkPragmaExpr, tmp, newTree(nnkPragma, ident"used"))
      else:
        tmp, rhs))
    var toIndex: seq[NimNode]
    var spreadIndex = -1
    for i, name in lhs.pairs:
      if name.kind == nnkExprColonExpr:
        result.add(openAssign(name[1],
          if lhs.kind != nnkBracket and name[0].kind == nnkIdent:
            newDotExpr(tmp, name[0])
          else:
            newTree(nnkBracketExpr, tmp, name[0]), kind))
      elif name.kind == nnkPrefix and
        (name[0].eqIdent"*" or name[0].eqIdent".." or name[0].eqIdent"..."):
        if spreadIndex >= 0:
          error("cannot have 2 spread assignments", name)
        spreadIndex = toIndex.len
        toIndex.add(name[1])
      else:
        toIndex.add(name)
    if lhs.kind == nnkPar and toIndex.len == 1 and spreadIndex == -1: # (a)
      result = openAssign(toIndex[0], rhs, kind)
    else:
      for i, x in toIndex:
        let index =
          if spreadIndex == -1 or i < spreadIndex:
            newLit(i)
          elif i == spreadIndex:
            infix(newLit i, "..", prefix(newLit(toIndex.len - i), "^"))
          else:
            prefix(newLit(toIndex.len - i), "^")
        let o = openAssign(x, newTree(nnkBracketExpr, tmp, index), kind)
        result.add(o)
  of nnkLiterals:
    result = newCall(bindSym"assignCheckEqual", rhs, lhs)
  of nnkPrefix:
    case lhs[0].strVal.identStr
    of "==":
      result = newCall(bindSym"assignCheckEqual", rhs, lhs[1])
    of "!=":
      result = newCall(bindSym"assignCheckNotEqual", rhs, lhs[1])
    of "<":
      result = newCall(bindSym"assignCheckLess", rhs, lhs[1])
    of "<=":
      result = newCall(bindSym"assignCheckLessEqual", rhs, lhs[1])
    of ">":
      result = newCall(bindSym"assignCheckGreater", rhs, lhs[1])
    of ">=":
      result = newCall(bindSym"assignCheckGreaterEqual", rhs, lhs[1])
    of "is":
      result = newCall(bindSym"assignCheckType", rhs, lhs[1])
    of "isnot":
      result = newCall(bindSym"assignCheckNotType", rhs, lhs[1])
    of "in":
      result = newCall(bindSym"assignCheckContains", rhs, lhs[1])
    of "notin":
      result = newCall(bindSym"assignCheckNotContains", rhs, lhs[1])
    of "^":
      result = openAssign(lhs[1], rhs, akSet)
    of "@":
      result = openAssign(lhs[1], rhs, akVar)
    else:
      result = defstmt(lhs, rhs)
  of nnkInfix:
    case lhs[0].strVal.identStr
    of "of":
      result = openAssign(lhs[1], newCall(lhs[2], rhs), kind)
    of "as", ":=":
      let tmp = genSym(nskLet, "tmpAs")
      result = newStmtList(newLetStmt(tmp, rhs))
      var last = lhs
      while true:
        result.add(openAssign(last[2], tmp, kind))
        last = last[1]
        if not (last.kind == nnkInfix and (lhs[0].eqIdent"as" or lhs[0].eqIdent":=")):
          result.add(openAssign(last, tmp, kind))
          break
    of "is":
      if lhs[1].kind notin {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
        error("type annotations should be given directly to identifiers", lhs[1])
      if kind == akSet:
        error("cannot put type annotation on assignment", lhs)
      result = newTree(if kind == akVar: nnkVarSection else: nnkLetSection,
        newIdentDefs(lhs[1], lhs[2], rhs))
    else:
      result = defstmt(lhs, rhs)
  of nnkVarTy:
    result = openAssign(lhs[0], rhs, akVar)
  else:
    result = defstmt(lhs, rhs)

when not defined(assignsDisableOptionAssign):
  type
    AssignOptionError* = object of AssignError
      ## error for failed contains checks in assignments

  template assignCheckOption*(a): untyped =
    ## template for equality checks in assignments
    if not a.isSome:
      raise newException(AssignOptionError, "option " & astToStr(a) & " was not Some")

  macro assign*[T](lhs; rhs: Option[T], kind: static AssignKind = akLet): untyped =
    ## The library's builtin overload of `assign` for `Option[T]`.
    if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
      let tmp = genSym(nskLet, "tmpOption")
      result = newStmtList(
        newLetStmt(tmp, rhs),
        newCall(bindSym"assignCheckOption", tmp),
        openAssign(lhs[1], newCall(bindSym"unsafeGet", tmp), kind))
    else:
      result = defaultAssign(lhs, rhs, kind)

template implementAssign*(T; body) {.dirty.} =
  ## Implements an overload (non-exported) of `assign` for `T` with the macro body being `body`.
  ## Macro argument names in order are `lhs`, `rhs` and `kind`.
  ## Has shorthand aliases `open` and `default` for `openAssign` and `defaultAssign` respectively.
  runnableExamples:
    import std/macros
    type LinkedList[T] {.acyclic.} = ref object
      leaf: T
      next: LinkedList[T]
    
    implementAssign LinkedList:
      let newLhs = if lhs.kind == nnkBracket and lhs.len == 1: lhs[0] else: lhs
      if newLhs.kind == nnkInfix and newLhs[0].eqIdent"|":
        newStmtList(
          open(newLhs[1], newDotExpr(rhs, ident"leaf")),
          open(newLhs[2], newDotExpr(rhs, ident"next")))
      else:
        default()
    
    let a = LinkedList[int](leaf: 1, next:
      LinkedList[int](leaf: 2, next:
        LinkedList[int](leaf: 3, next: nil)))
    
    import ./syntax
    x | [y | [z | _]] := a
    doAssert (x, y, z) == (1, 2, 3)
  macro assign(lhs; rhs: T, kind: static AssignKind): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openAssign(a, b, kind)
    template open(a, b: NimNode, k: static AssignKind): NimNode {.used.} = openAssign(a, b, k)
    template default(): NimNode {.used.} = defaultAssign(lhs, rhs, kind)
    template default(a, b: NimNode): NimNode {.used.} = defaultAssign(a, b, kind)
    template default(a, b: NimNode, k: static AssignKind): NimNode {.used.} = defaultAssign(a, b, k)
    body

template implementAssignExported*(T; body) {.dirty.} =
  ## Same as `implementAssign` but exports the generated macro.
  macro assign*(lhs; rhs: T, kind: static AssignKind): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openAssign(a, b, kind)
    template open(a, b: NimNode, k: static AssignKind): NimNode {.used.} = openAssign(a, b, k)
    template default(): NimNode {.used.} = defaultAssign(lhs, rhs, kind)
    template default(a, b: NimNode): NimNode {.used.} = defaultAssign(a, b, kind)
    template default(a, b: NimNode, k: static AssignKind): NimNode {.used.} = defaultAssign(a, b, k)
    body
