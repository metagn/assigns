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
##   
##   # nesting (applies to everything)
##   a as mut(b) := c # => let temp = c; let a = temp; var b = temp
##   
##   # literal assertion (for matching)
##   1 := 2 # => doAssert 1 == 2
##   
##   # collection unpacking (works for anything if it can be indexed with integers):
##   (a, b, c) := d
##   
##   # collection spreading (works if you can do d[i], d[i..^j] and d[^i]):
##   (a, *b, c) := d
##   
##   # empty tuple unpacking, discards right hand side:
##   () := a
##   
##   # named unpacking, works on anything:
##   (prop1: a, prop2: b) := c
##   
##   # option unpacking, only custom type implementation that comes with the library:
##   Some(a) := b
##   some(a) := b
##   def: some a = b
## 
## Custom definitions
## ==================
## 
## A large and important feature in this package is that you can overload definitions for custom types.
## The `define` macro in this module is overloadable, and the `openDefine` template creates a
## NimNode that calls a forced open symbol of `define` with the AST of the left hand side,
## AST of the right hand side, and the flag of whether or not it is a `let`, `var`, or mutating
## assignment. You can use the `implementDefine` and `implementDefineExported` templates as a
## shorthand for declaring these overloads.
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

import macros, options

when (compiles do: import sliceutils/tuples):
  import sliceutils/tuples
  export tuples
else:
  import defines/tupleindex
  export tupleindex

type DefineKind* = enum
  ## Propagated flag of how definitions should create assignments.
  dkLet, dkVar, dkAssign

template openDefine*(lhs, rhs: NimNode, dk: DefineKind = dkLet): NimNode =
  ## Creates a node that calls an open symbol `define` with `lhs` and `rhs`.
  if rhs.kind == nnkVarTy:
    newCall(bindSym("define", brForceOpen), lhs, rhs[0], newLit dkVar)
  else:
    newCall(bindSym("define", brForceOpen), lhs, rhs, newLit dk)

# needs to be here for openDefine to work
proc defaultDefine*(lhs, rhs: NimNode, kind = dkLet): NimNode
  ## The default interpretation of a definition.

macro define*[T](lhs; rhs: T, kind: static[DefineKind]): untyped =
  ## Handles definitions for a type `T`.
  ## Can be overloaded, optionally via the convenience macros
  ## `implementDefine` and `implementDefineExported`. 
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
    result = newCall("doAssert", infix(lhs, "==", rhs))
  elif lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and lhs[0].eqIdent"mut":
    result = defaultDefine(lhs[1], rhs, dkVar)
  elif lhs.kind == nnkInfix and lhs[0].eqIdent"as":
    let tmp = genSym(nskLet, "tmpAs")
    result = newStmtList(newLetStmt(tmp, rhs))
    var last = lhs
    while true:
      result.add(openDefine(last[2], tmp, kind))
      last = last[1]
      if not (last.kind == nnkInfix and last[0].eqIdent"as"):
        result.add(openDefine(last, tmp, kind))
        break
  else:
    result = defstmt(lhs, rhs)

when not defined(definesDisableOptionDefine):
  macro define*[T](lhs; rhs: Option[T], kind: static[DefineKind] = dkLet): untyped =
    ## The library's builtin overload of `define` for `Option[T]`.
    if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
      result = openDefine(lhs[1], newCall(bindSym"get", rhs), kind)
    else:
      result = defaultDefine(lhs, rhs, kind)

template implementDefine*(T; body) {.dirty.} =
  ## Implements an overload (non-exported) of `define` for `T` with the macro body being `body`.
  ## Macro argument names in order are `lhs`, `rhs` and `kind`.
  ## Has shorthand aliases `open` and `default` for `openDefine` and `defaultDefine` respectively.
  runnableExamples:
    import macros
    type LinkedList[T] {.acyclic.} = ref object
      leaf: T
      next: LinkedList[T]
    
    implementDefine LinkedList:
      if lhs.kind == nnkBracket and lhs.len == 1 and lhs[0].kind == nnkInfix and lhs[0][0].eqIdent"|":
        newStmtList(open(lhs[0][1], newDotExpr(rhs, ident"leaf")), open(lhs[0][2], newDotExpr(rhs, ident"next")))
      else:
        default(lhs, rhs)
    
    let a = LinkedList[int](leaf: 1, next:
      LinkedList[int](leaf: 2, next:
        LinkedList[int](leaf: 3, next: nil)))
    
    [x | [y | [z | _]]] := a
    doAssert (x, y, z) == (1, 2, 3)
  macro define(lhs; rhs: T, kind: static[DefineKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openDefine(a, b, kind)
    template open(a, b: NimNode, k: static[DefineKind]): NimNode {.used.} = openDefine(a, b, k)
    template default(a, b: NimNode): NimNode {.used.} = defaultDefine(a, b, kind)
    template default(a, b: NimNode, k: static[DefineKind]): NimNode {.used.} = defaultDefine(a, b, k)
    body

template implementDefineExported*(T; body) {.dirty.} =
  ## Same as `implementDefine` but exports the generated macro.
  macro define*(lhs; rhs: T, kind: static[DefineKind]): untyped =
    template open(a, b: NimNode): NimNode {.used.} = openDefine(a, b, kind)
    template open(a, b: NimNode, k: static[DefineKind]): NimNode {.used.} = openDefine(a, b, k)
    template default(a, b: NimNode): NimNode {.used.} = defaultDefine(a, b, kind)
    template default(a, b: NimNode, k: static[DefineKind]): NimNode {.used.} = defaultDefine(a, b, k)
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
      result.add(openDefine(lhs, rhs))
  else:
    let a = assignments
    let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
    result = openDefine(lhs, rhs)

macro def*(assignments, body): untyped =
  ## Goes through each assignment expression in `assignments` and processes them into definitions.
  ## These definitions are then put into a new scope where `body` is evaluated.
  ##
  ## Example:
  ##
  ## .. code-block::nim
  ##  def:
  ##    a = 1
  ##  do:
  ##    doAssert a == 1
  ##  doAssert a == 3
  result = newStmtList()
  if assignments.kind == nnkStmtList:
    for a in assignments:
      let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
      result.add(openDefine(lhs, rhs))
  else:
    let a = assignments
    let (lhs, rhs) = if a.kind == nnkInfix: (a[1], a[2]) else: (a[0], a[1])
    result.add(openDefine(lhs, rhs))
  result.add(body)
  result = newBlockStmt(result)

macro `:=`*(a, b): untyped =
  ## Unpacks `b` with the given description `a`.
  runnableExamples:
    (a, b) := (1, 2)
    c as d := 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  result = openDefine(a, b)

macro `::=`*(a, b): untyped =
  ## Same as `:=`, except assigns to an existing variable
  ## instead of creating a `let` declaration.
  runnableExamples:
    var a, b, c, d: int
    (a, b) ::= (1, 2)
    c as d ::= 3
    doAssert (a, b, c, d) == (1, 2, 3, 3)
  result = openDefine(a, b, dkAssign)

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
  var defineFinished = false
  try:
    a := b
    defineFinished = true
    body
  except:
    if defineFinished:
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
    var defineFinished = false
    try:
      `a` := `b`
      defineFinished = true
      `body`
    except:
      if defineFinished:
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
