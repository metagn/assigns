## The routines in this module are copied from
## `sliceutils/tuples <https://metagn.github.io/sliceutils/tuples.html>`_
## to provide support for tuple spreading.

import macros, typetraits

when not declared(tupleLen):
  template tupleLen(T): untyped = arity(T)

template `[]`*[T: tuple](t: T, i: static BackwardsIndex): auto = system.`[]`(t, tupleLen(T).int - i.int)
template `[]`*[T: tuple](t: T, i: static int): auto = system.`[]`(t, i)

proc `[]`*[T: tuple](t: T, s: static[HSlice[int, BackwardsIndex]]): auto {.inline.} =
  macro generate(t: untyped, s: static[HSlice[int, BackwardsIndex]]): untyped =
    result = newTree(nnkTupleConstr)
    for i in s.a..tupleLen(T).int - s.b.int:
      result.add(newTree(nnkBracketExpr, t, newLit i))
  generate(t, s)
