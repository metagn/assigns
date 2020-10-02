import unittest, definesugar, macros

macro match*(val: untyped, branches: varargs[untyped]): untyped =
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
          var raisedInBody = false
          try:
            `v` := `val`
            try:
              `bod`
            except Exception:
              raisedInBody = true
              raise
          except Exception:
            if raisedInBody:
              raise
            else:
              `result`
    else:
      error("invalid branch for match", b)

import options

test "naive match works":
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
