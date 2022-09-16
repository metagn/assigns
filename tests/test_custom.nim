when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import assigns, assigns/impl, macros

test "custom Result[T]":
  type Result[T] = object
    case success: bool
    of true:
      value: T
    else:
      error: ref Exception

  macro assign[T](lhs; rhs: Result[T], kind: static AssignKind): untyped =
    let isCallCommandLen2 = lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2
    if (isCallCommandLen2 and (lhs[0].eqIdent"ok" or lhs[0].eqIdent"Ok")) or
       (lhs.kind == nnkPrefix and lhs[0].eqIdent"?"):
        let tmp = genSym(nskLet, "tmpResult")
        let asgn = openAssign(lhs[1], newDotExpr(tmp, ident"value"), kind)
        result = quote do:
          let `tmp` = `rhs`
          if not `tmp`.success:
            raise `tmp`.error
          `asgn`
    elif isCallCommandLen2 and (lhs[0].eqIdent"err" or lhs[0].eqIdent"Err"):
      result = openAssign(lhs[1], newDotExpr(rhs, ident"error"), kind)
    else:
      result = defaultAssign(lhs, rhs, kind)

  let success = Result[int](success: true, value: 5)
  Ok(a) := success
  check a == 5
  ?a2 := success
  check a2 == 5
  let error = Result[void](success: false, error: newException(Exception, "error"))
  def: Err(b) = error
  check b.msg == "error"

import options

test "redefined for Option[T]":
  macro assign[T](lhs; rhs: Option[T], kind: static AssignKind): untyped =
    if (lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some")) or
       (lhs.kind == nnkPrefix and lhs[0].eqIdent"?"):
      result = openAssign(lhs[1], newCall(bindSym"get", rhs), kind)
    else:
      result = defaultAssign(lhs, rhs, kind)

  let opt = some(5)
  ?a := opt
  check a == 5
  (b, ?c) := (4, opt)
  check b == 4
  check c == 5

test "custom with convenience template for LinkedList[T]":
  type LinkedList[T] {.acyclic.} = ref object
    leaf: T
    next: LinkedList[T]
  
  implementAssign LinkedList:
    if lhs.kind == nnkBracket and lhs.len == 1 and lhs[0].kind == nnkInfix and lhs[0][0].eqIdent"|":
      newStmtList(open(lhs[0][1], newDotExpr(rhs, ident"leaf")), open(lhs[0][2], newDotExpr(rhs, ident"next")))
    else:
      default(lhs, rhs)
  
  let a = LinkedList[int](leaf: 1, next:
    LinkedList[int](leaf: 2, next:
      LinkedList[int](leaf: 3, next: nil)))
  
  [x | [y | [z | _]]] := a
  check:
    x == 1
    y == 2
    z == 3

  proc `[]`[T](list: LinkedList[T], i: int): T =
    if i == 0: list.leaf
    else: list.next[i - 1]
  
  [t | (u, v)] := a
  check:
    t == 1
    u == 2
    v == 3
