{.warning[UnusedImport]: off.}
when (compiles do: import sliceutils/tuples):
  import sliceutils/tuples
  export tuples
else:
  import std/[macros, typetraits]

  when not declared(tupleLen):
    template tupleLen(T): untyped = arity(type(T))

  template `[]`*[T: tuple](t: T, i: static BackwardsIndex): auto = system.`[]`(t, tupleLen(T).int - i.int)
  template `[]`*[T: tuple](t: T, i: static int): auto = system.`[]`(t, i)

  proc `[]`*[T: tuple](t: T, s: static HSlice[int, BackwardsIndex]): auto {.inline.} =
    macro generate(t: untyped, s: static HSlice[int, BackwardsIndex]): untyped =
      result = newTree(nnkTupleConstr)
      for i in s.a..tupleLen(T).int - s.b.int:
        result.add(newTree(nnkBracketExpr, t, newLit i))
    generate(t, s)
