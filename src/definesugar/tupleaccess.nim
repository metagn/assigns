import macros, typetraits

type
  NamedTuple* = concept type T
    T is tuple
    isNamedTuple(T)
  UnnamedTuple* = concept type T
    T is tuple
    not isNamedTuple(T)

template `[]`*[T: tuple](t: T, i: static BackwardsIndex): auto = system.`[]`(t, tupleLen(T).int - i.int)
template `[]`*[T: tuple](t: T, i: static int): auto = system.`[]`(t, i)

proc `[]`*[T: tuple](t: T, s: static[Slice[int]]): auto {.inline.} =
  macro generate(t: untyped, s: static[Slice[int]]): untyped =
    result = newTree(nnkTupleConstr)
    for i in s.a..s.b:
      result.add(newTree(nnkBracketExpr, t, newLit i))
  generate(t, s)

when false:
  template `[]`*[T: tuple](t: T, s: static[HSlice[int, BackwardsIndex]]): auto =
    t[s.a.int..tupleLen(T).int - s.b.int]

  template `[]`*[T: tuple](t: T, s: static[HSlice[BackwardsIndex, int]]): auto =
    t[tupleLen(T).int - s.a.int..s.b.int]

  template `[]`*[T: tuple](t: T, s: static[HSlice[BackwardsIndex, BackwardsIndex]]): auto =
    t[tupleLen(T).int - s.a.int..tupleLen(T).int - s.b.int]
else:
  proc `[]`*[T: tuple](t: T, s: static[HSlice[BackwardsIndex, BackwardsIndex]]): auto {.inline.} =
    macro generate(t: untyped, s: static[HSlice[BackwardsIndex, BackwardsIndex]]): untyped =
      result = newTree(nnkTupleConstr)
      for i in tupleLen(T).int - s.a.int..tupleLen(T).int - s.b.int:
        result.add(newTree(nnkBracketExpr, t, newLit i))
    generate(t, s)
  proc `[]`*[T: tuple](t: T, s: static[HSlice[int, BackwardsIndex]]): auto {.inline.} =
    macro generate(t: untyped, s: static[HSlice[int, BackwardsIndex]]): untyped =
      result = newTree(nnkTupleConstr)
      for i in s.a..tupleLen(T).int - s.b.int:
        result.add(newTree(nnkBracketExpr, t, newLit i))
    generate(t, s)
  proc `[]`*[T: tuple](t: T, s: static[HSlice[BackwardsIndex, int]]): auto {.inline.} =
    macro generate(t: untyped, s: static[HSlice[BackwardsIndex, int]]): untyped =
      result = newTree(nnkTupleConstr)
      for i in tupleLen(T).int - s.a.int..s.b.int:
        result.add(newTree(nnkBracketExpr, t, newLit i))
    generate(t, s)
