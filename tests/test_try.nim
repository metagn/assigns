import unittest
import assigns

test ":=??":
  var executed1 = false
  (a, b) :=?? (1, 2):
    check a == 1
    check b == 2
    executed1 = true
  check executed1
  
  (a, b) :=?? 3:
    check false
  
  var executed2 = false
  (a, b) :=?? (1, 2):
    check a == 1
    check b == 2
    executed2 = true
  else:
    check false
  check executed2
  
  var executed3 = false
  (a, b) :=?? 3:
    check false
  else:
    executed3 = true
  check executed3

test "::=??":
  var a, b: int
  check (a, b) ::=?? (1, 2)

  check not ((a, b) ::=?? 3)
  check not ((a, c) ::=?? (1, 2))
  
  a = 0
  b = 0
  var executed2 = false
  (a, b) ::=?? (1, 2):
    executed2 = true
  else:
    check false
  check a == 1
  check b == 2
  check executed2
  
  var executed3 = false
  (a, b) ::=?? 3:
    check false
  else:
    executed3 = true
  check executed3

import options
test ":=? based on option":
  proc foo(x: Option[int]): int =
    some(n) :=? x:
      n + 1
    else:
      0
  
  check foo(some(0)) == 1
  check foo(none(int)) == 0

import macros

macro match(val: untyped, branches: varargs[untyped]): untyped =
  result = newEmptyNode()
  for i in countdown(branches.len - 1, 0):
    let b = branches[i]
    case b.kind
    of nnkElse:
      result = b[0]
    of nnkElifBranch:
      let cond = b[0]
      let bod = b[1]
      result = quote do:
        if `cond`:
          `bod`
        else:
          `result`
    of nnkOfBranch:
      let bod = b[^1]
      for vi in 0..<b.len - 1:
        let v = b[vi]
        result = quote do:
          `v` :=? `val`:
            `bod`
          else:
            `result`
    else:
      error("invalid branch for match", b)

test "match with :=? works":
  proc isSomeYesOrNo[T](opt: Option[T]): string =
    match opt:
    of Some(_): "yes"
    else: "no"
  
  check isSomeYesOrNo(some 3) == "yes"
  check isSomeYesOrNo(none int) == "no"

  var x: string
  match range[1..5](4):
  of 1, 2, 3: x = "below or equal to three"
  of 4, 5: x = "above three"

  check x == "above three"
