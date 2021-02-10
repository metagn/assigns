## Provides default overloads and utilities for macros of definitions.
## 
## Examples
## ========
## 
## .. code-block::nim
##     
##   # regular definition
##   a := b
##   
##   # definition block
##   def: a = b
##   def:
##     a = b
##     c = d
##   
##   # scoped definition block
##   def:
##     a = b
##   do:
##     use a
##   
##   # mutable variable definition
##   mut(a) := b
##   a := var b
##   def: mut a = b
##   
##   # assignment to existing variable
##   a ::= b
##   
##   # aliasing
##   a as b := c
##   a := b := c
##   
##   # checks (for matching)
##   == 1 := 2 # raises exception
##   1 := 2 # same, literals inferred to be equality checks
##   (== ok, res) := (ok, 1)
##   (in [1, 2, 3]) := 2
##   (is float) := 2.0
##   != 1 := 2
##   (notin [1, 2, 3]) := 4
##   (isnot int) := 2.0
##   
##   # collection unpacking (works for anything if it can be indexed with integers):
##   (a, b, c) := d
##   (a) := d # single parens does not unpack, equivalent to a := b
##   (a,) := d # equivalent to a := b[0]
## 
##   # default syntax supports brackets instead of parens:
##   [a, b, c] := d
##   [a] := d # equivalent to a := d[0]
##   
##   # collection spreading (works if you can do d[i], d[i..^j] and d[^i]):
##   (a, *b, c) := d
##   (a, ..b, c) := d
##   (a, ...b, c) := d
##   
##   # nesting (applies to everything)
##   (a, b) as mut(c) := d # => let temp = d; let a = temp[0]; let b = temp[1]; var c = temp
##   
##   # empty tuple unpacking, discards right hand side:
##   () := a
##   
##   # named unpacking, works on anything (prop1 and prop2 must be identifiers):
##   (prop1: a, prop2: b) := c
## 
##   # unpacking by given index:
##   (0..4: hello, 6..^1: world) := "hello world"
##   import json
##   ("name": name, "age": age) := %*{"name": "John", "age": 30}
## 
##   # indexing by identifier if brackets used instead of parens (for enum indexed arrays):
##   [index1: a, index2: b] := c
##   
##   # conversion to type:
##   a of int := 4.0
##   type Obj = ref object of RootObj
##   type Obj2 = ref object of Obj
##   let x: Obj = Obj2()
##   y of Obj2 := x
##   
##   # type annotation (must be directly tied to identifiers):
##   a is uint8 := 4
##   
##   # option unpacking, only custom type implementation that comes with the library:
##   Some(a) := b
##   some(a) := b
##   def: some a = b
## 
## Note about tuple spreading
## """"""""""""""""""""""""""
## 
## The best way to make tuple spreading (``(a, *b, c) := d``) generic that I have come up with seems to be to
## generate something like ``a = d[0]; b = d[1..^2]; c = d[^1]``. This is why the `tupleindex <tupleindex.html>`_
## module is imported and exported in this module, to provide the required routines for tuple spreading.
## However, if my package `sliceutils <https://github.com/hlaaftana/sliceutils>`_ is installed, it will instead
## import and export `sliceutils/tuples <https://hlaaftana.github.io/sliceutils/tuples.html>`_ which has our required
## routines. I have not made `sliceutils` a dependency because there are 4 other modules within it and some routines
## that we don't need in `sliceutils/tuples` as well, not to mention that `sliceutils` does not support versions
## below 1.4. I hope that if you are going to be using both modules, the extra inclusion of the file `tupleindex`
## will not be too much of an inconvenience.
## 
## Custom definitions
## ==================
## 
## A large and important feature in this package is that you can overload definitions for custom types.
## The `assign` macro in this module is overloadable, and the `openAssign` template creates a
## NimNode that calls a forced open symbol of `assign` with the AST of the left hand side,
## AST of the right hand side, and the flag of whether or not it is a `let`, `var`, or mutating
## assignment. You can use the `implementAssign` and `implementAssignExported` templates as a
## shorthand for declaring these overloads.

import macros, options

when (compiles do: import sliceutils/tuples):
  import sliceutils/tuples
  export tuples
else:
  import assigns/tupleindex
  export tupleindex

type AssignKind* = enum
  ## Propagated flag of how definitions should create assignments.
  akLet, akVar, akAssign

type
  AssignCheckError* = object of CatchableError
    ## any kind of check error in assignments
  AssignEqualityCheckError* = object of AssignCheckError
    ## error for failed equality checks in assignments
  AssignContainsCheckError* = object of AssignCheckError
    ## error for failed contains checks in assignments

template assignCheckEqual*(a, b): untyped =
  ## template for equality checks in assignments
  if a != b:
    raise newException(AssignEqualityCheckError, "expected " & astToStr(b) & ", got " & astToStr(a))

template assignCheckNotEqual*(a, b): untyped =
  ## template for non-equality checks in assignments
  if a == b:
    raise newException(AssignEqualityCheckError, "did not expect " & astToStr(b) & ", got " & astToStr(a))

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
    raise newException(AssignContainsCheckError, "expected " & astToStr(a) & " to be in " & astToStr(b))

template assignCheckNotContains*(a, b): untyped =
  ## template for non-equality checks in assignments
  if a in b:
    raise newException(AssignContainsCheckError, "did not expect " & astToStr(a) & " to be in " & astToStr(b))

template openAssign*(lhs, rhs: NimNode, ak: AssignKind = akLet): NimNode =
  ## Creates a node that calls an open symbol `assign` with `lhs` and `rhs`.
  if rhs.kind == nnkVarTy:
    newCall(bindSym("assign", brForceOpen), lhs, rhs[0], newLit akVar)
  else:
    newCall(bindSym("assign", brForceOpen), lhs, rhs, newLit ak)

# needs to be here for openAssign to work
proc defaultAssign*(lhs, rhs: NimNode, kind = akLet): NimNode
  ## The default interpretation of a definition.

macro assign*[T](lhs; rhs: T, kind: static[AssignKind]): untyped =
  ## Handles definitions for a type `T`.
  ## Can be overloaded, optionally via the convenience macros
  ## `implementAssign` and `implementAssignExported`. 
  result = defaultAssign(lhs, rhs, kind)

proc defaultAssign*(lhs, rhs: NimNode, kind = akLet): NimNode =
  proc defstmt(a, b: NimNode): NimNode =
    case kind
    of akVar: newVarStmt(a, b)
    of akLet: newLetStmt(a, b)
    of akAssign: newAssignment(a, b)
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
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"==":
    result = newCall(bindSym"assignCheckEqual", rhs, lhs[1])
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"!=":
    result = newCall(bindSym"assignCheckNotEqual", rhs, lhs[1])
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"is":
    result = newCall(bindSym"assignCheckType", rhs, lhs[1])
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"isnot":
    result = newCall(bindSym"assignCheckNotType", rhs, lhs[1])
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"in":
    result = newCall(bindSym"assignCheckContains", rhs, lhs[1])
  elif lhs.kind == nnkPrefix and lhs[0].eqIdent"notin":
    result = newCall(bindSym"assignCheckNotContains", rhs, lhs[1])
  elif lhs.kind == nnkInfix and lhs[0].eqIdent"of":
    result = openAssign(lhs[1], newCall(lhs[2], rhs), kind)
  elif lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and lhs[0].eqIdent"mut":
    result = openAssign(lhs[1], rhs, akVar)
  elif lhs.kind == nnkInfix and (lhs[0].eqIdent"as" or lhs[0].eqIdent":="):
    let tmp = genSym(nskLet, "tmpAs")
    result = newStmtList(newLetStmt(tmp, rhs))
    var last = lhs
    while true:
      result.add(openAssign(last[2], tmp, kind))
      last = last[1]
      if not (last.kind == nnkInfix and (lhs[0].eqIdent"as" or lhs[0].eqIdent":=")):
        result.add(openAssign(last, tmp, kind))
        break
  elif lhs.kind == nnkInfix and lhs[0].eqIdent"is":
    if lhs[1].kind notin {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
      error("type annotations should be given directly to identifiers", lhs[1])
    if kind == akAssign:
      error("cannot put type annotation on assignment", lhs)
    result = newTree(if kind == akVar: nnkVarSection else: nnkLetSection,
      newIdentDefs(lhs[1], lhs[2], rhs))
  else:
    result = defstmt(lhs, rhs)

when not defined(assignsDisableOptionAssign):
  macro assign*[T](lhs; rhs: Option[T], kind: static[AssignKind] = akLet): untyped =
    ## The library's builtin overload of `assign` for `Option[T]`.
    if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
      result = openAssign(lhs[1], newCall(bindSym"get", rhs), kind)
    else:
      result = defaultAssign(lhs, rhs, kind)

template implementAssign*(T; body) {.dirty.} =
  ## Implements an overload (non-exported) of `assign` for `T` with the macro body being `body`.
  ## Macro argument names in order are `lhs`, `rhs` and `kind`.
  ## Has shorthand aliases `open` and `default` for `openAssign` and `defaultAssign` respectively.
  runnableExamples:
    import macros
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
    
    x | [y | [z | _]] := a
    doAssert (x, y, z) == (1, 2, 3)
  macro assign(lhs; rhs: T, kind: static[AssignKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openAssign(a, b, kind)
    template open(a, b: NimNode, k: static[AssignKind]): NimNode {.used.} = openAssign(a, b, k)
    template default(): NimNode {.used.} = defaultAssign(lhs, rhs, kind)
    template default(a, b: NimNode): NimNode {.used.} = defaultAssign(a, b, kind)
    template default(a, b: NimNode, k: static[AssignKind]): NimNode {.used.} = defaultAssign(a, b, k)
    body

template implementAssignExported*(T; body) {.dirty.} =
  ## Same as `implementAssign` but exports the generated macro.
  macro assign*(lhs; rhs: T, kind: static[AssignKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openAssign(a, b, kind)
    template open(a, b: NimNode, k: static[AssignKind]): NimNode {.used.} = openAssign(a, b, k)
    template default(): NimNode {.used.} = defaultAssign(lhs, rhs, kind)
    template default(a, b: NimNode): NimNode {.used.} = defaultAssign(a, b, kind)
    template default(a, b: NimNode, k: static[AssignKind]): NimNode {.used.} = defaultAssign(a, b, k)
    body

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
      let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
      result.add(openAssign(lhs, rhs))
  else:
    let a = assignments
    let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
    result = openAssign(lhs, rhs)

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
      let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
      result.add(openAssign(lhs, rhs))
  else:
    let a = assignments
    let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
    result.add(openAssign(lhs, rhs))
  result.add(body)
  result = newBlockStmt(result)

macro `:=`*(a, b): untyped =
  ## Unpacks `b` with the given description `a`.
  runnableExamples:
    (a, b) := (1, 2)
    c as d := 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  result = openAssign(a, b)

macro `::=`*(a, b): untyped =
  ## Same as `:=`, except assigns to an existing variable
  ## instead of creating a `let` declaration.
  runnableExamples:
    var a, b, c, d: int
    (a, b) ::= (1, 2)
    c as d ::= 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  result = openAssign(a, b, akAssign)

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
  except:
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
  except:
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
    except:
      if assignFinished:
        raise
      else:
        `elseExpr`

template `::=?`*(a, b): bool =
  ## Executes ``a ::= b`` and returns false if it gives a runtime error.
  ## Otherwise returns `true`.
  ## Does not need conditional variants since it doesn't
  ## modify the scope, you can use `if`/`else` instead.
  runnableExamples:
    var a, b: int
    doAssert (a, b) ::=? (1, 2)
    import options
    let x = none(int)
    var n: int
    doAssert not (some(n) ::=? x)
  try:
    a ::= b
    true
  except:
    false

template `:=??`*(a, b): bool =
  ## Returns `false` if ``a := b`` doesn't compile, then
  ## executes (!) ``a := b`` and returns false if it errors.
  ## Otherwise returns `true`.
  ## Note that the executed ``a := b`` will not have any
  ## affect on the scope of the following statements since
  ## it uses a `try` statement.
  runnableExamples:
    when false:
      doAssert (a, b) :=?? (1, 2)
      doAssert not ((a, b, c) :=?? (1, 2))
  when compiles(a :=? b):
    a :=? b
  else:
    false

template `:=??`*(a, b, body): untyped =
  ## Executes `body` if ``a := b`` compiles and doesn't error at runtime.
  ## `body` will be in the same scope as the definition ``a := b``.
  runnableExamples:
    when false:
      template foo(a, b, works) =
        a :=?? b:
          doAssert works
      foo((a, b), (1, 2), true)
  when compiles(a := b):
    a :=? b:
      body

macro `:=??`*(a, b, body, elseBranch): untyped =
  ## Executes `body` if ``a := b`` compiles and doesn't error at runtime,
  ## otherwise executes `elseBranch`.
  ## `body` will be in the same scope as the definition ``a := b``.
  runnableExamples:
    when false:
      template foo(a, b, works) =
        a :=?? b:
          doAssert works
        else:
          doAssert not works
      foo((a, b), (1, 2), true)
      foo((a, b, c), (1, 2), false)
  let elseExpr = if elseBranch.kind == nnkElse: elseBranch[0] else: elseBranch
  result = quote:
    when compiles(`a` := `b`):
      `a` :=? `b`:
        `body`
      else:
        `elseExpr`
    else:
      `elseExpr`

template `::=??`*(a, b): bool =
  ## Returns `false` if ``a ::= b`` doesn't compile or gives a runtime error.
  ## Otherwise returns `true`.
  runnableExamples:
    when false:
      var a, b: int
      doAssert (a, b) ::=? (1, 2)
      doAssert not ((b, c) ::=? (1, 2))
  when compiles(a ::= b):
    a ::=? b
  else:
    false

template `::=??`*(a, b, body): bool =
  ## Executes `body` if ``a ::= b`` compiles and doesn't give a runtime error.
  when compiles(a ::= b):
    if a ::=? b:
      body

macro `::=??`*(a, b, body, elseBranch): untyped =
  ## Executes `body` if ``a ::= b`` compiles and doesn't give a runtime error,
  ## otherwise executes `elseBranch`.
  runnableExamples:
    when false:
      var a, b: int
      template foo(a, b, works) =
        a ::=?? b:
          doAssert works
        else:
          doAssert not works
      foo((a, b), (1, 2), true)
      foo((a, c), (1, 2), false)
  let elseExpr = if elseBranch.kind == nnkElse: elseBranch[0] else: elseBranch
  result = quote:
    when compiles(`a` := `b`):
      if `a` ::=? `b`:
        `body`
      else:
        `elseExpr`
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
