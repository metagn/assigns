when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import assigns, assigns/tap

test "basic test":
  let val = tap(a := 5): a + 1
  check val == 6
  let x = tap(a := 5, result s := newSeq[int](a), i in 0 ..< a):
    s[i] = i + 1
  check x == @[1, 2, 3, 4, 5]
  var s: seq[int]
  tap a = 5, i in 1 .. a, filter i mod 2 != 0:
    s.add(i)
  check s == @[1, 3, 5]
  var s2: seq[int]
  tap:
    a = 5
    i in 1 .. a
    filter i mod 2 != 0
  do:
    s2.add(i)
  check s2 == @[1, 3, 5]

import options

test "matching":
  let x = some(5)
  var branch = 0
  tap some(a) :=? x:
    branch = 1
    check a == 5
  else:
    branch = 2
  check branch == 1
  let y = none(int)
  branch = 0
  tap some(a) :=? y:
    branch = 1
    check a == 5
  else:
    branch = 2
  check branch == 2
  branch = 0
  tap some(a) =? y:
    branch = 1
    check a == 5
  else:
    branch = 2
  check branch == 2
  let xStr = tap result res := "":
    tap some(a) :=? x:
      res = "some(" & $a & ")"
    else:
      res = "none"
  check xStr == "some(5)"
  let yStr = tap result res := "":
    tap some(a) :=? y:
      res = "some(" & $a & ")"
    else:
      res = "none(" & $typeof(y.get) & ")"
  check yStr == "none(int)"
