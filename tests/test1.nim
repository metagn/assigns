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

import macros
test "custom":
  type Result[T] = object
    case success: bool
    of true:
      value: T
    else:
      error: string
  
  macro define[T](lhs; rhs: Result[T]): untyped =
    if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"ok" or lhs[0].eqIdent"Ok"):
      result = newCall(bindSym("define", brOpen), lhs[1], newDotExpr(rhs, ident"value"))
    elif lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"err" or lhs[0].eqIdent"Err"):
      result = newCall(bindSym("define", brOpen), lhs[1], newDotExpr(rhs, ident"error"))
    else:
      result = defaultDefine(lhs, rhs)

  let success = Result[int](success: true, value: 5)
  Ok(a) := success
  check a == 5
  let error = Result[void](success: false, error: "error")
  def: Err b = error
  check b == "error"