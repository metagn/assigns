# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import definesugar

test "basic def":
  def:
    a = 3
    mut x = 5
  
  check:
    a == 3
    x == 5
  x = 6
  check x == 6

test "basic :=":
  a := 3
  mut(x) := 5

  check:
    a == 3
    x == 5
  x = 6
  check x == 6

import options
test "options":
  def:
    Some(opt1) = some 4
  check opt1 == 4

  Some(opt2) := some 4
  check opt2 == 4

test "tuple":
  (a, b, c) := (1, 2, 3)
  check (a, b, c) == (1, 2, 3)

test "nested tuple":
  (a, (b, c), d) := (1, (2, 3), 4)
  check (a, b, c, d) == (1, 2, 3, 4)

test "named tuple":
  type Person = tuple[name: string, age: int]
  (age: a, name: n) := ("John Smith", 30).Person
  check:
    a == 30
    n == "John Smith"

test "as":
  (a, b) as c := (1, 2)
  check (a, b) == c
  check c == (1, 2) 

test "combo":
  (a, Some(mut b), mut c) := (1, some(2), 3)
  check (a, b, c) == (1, 2, 3)
  c = 4
  check c == 4
  b = 5
  check b == 5

import macros
test "custom":
  type Result[T] = object
    case success: bool
    of true:
      value: T
    else:
      error: ref Exception

  macro define[T](lhs; rhs: Result[T]): untyped =
    let isCallCommandLen2 = lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2
    if (isCallCommandLen2 and (lhs[0].eqIdent"ok" or lhs[0].eqIdent"Ok")) or
       (lhs.kind == nnkPrefix and lhs[0].eqIdent"?"):
        let tmp = genSym(nskLet, "tmpResult")
        let asgn = openDefine(lhs[1], newDotExpr(tmp, ident"value"))
        result = quote do:
          let `tmp` = `rhs`
          if not `tmp`.success:
            raise `tmp`.error
          `asgn`
    elif isCallCommandLen2 and (lhs[0].eqIdent"err" or lhs[0].eqIdent"Err"):
      result = openDefine(lhs[1], newDotExpr(rhs, ident"error"))
    else:
      result = defaultDefine(lhs, rhs)
  
  macro define[T](lhs; rhs: Option[T]): untyped =
    if (lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some")) or
       (lhs.kind == nnkPrefix and lhs[0].eqIdent"?"):
      result = openDefine(lhs[1], newCall(bindSym"get", rhs))
    else:
      result = defaultDefine(lhs, rhs)

  let success = Result[int](success: true, value: 5)
  Ok(a) := success
  check a == 5
  ?a2 := success
  check a2 == 5
  let error = Result[void](success: false, error: newException(Exception, "error"))
  def: Err b = error
  check b.msg == "error"
  let opt = some(5)
  ?a3 := opt
  check a3 == 5