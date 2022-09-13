when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import assigns

test "basic def":
  def:
    a = 3
    mut x = 5
    y = var 5
  
  check:
    a == 3
    x == 5
    y == 5
  x = 6
  check x == 6
  y = 6
  check y == 6

test "def with scoped block":
  var executed = false
  def:
    scoped = "abc"
  do:
    check scoped == "abc"
    executed = true
  check not declared(scoped)
  check executed

test "basic :=":
  a := 3
  mut(x) := 5
  y := var 5

  check:
    a == 3
    x == 5
    y == 5
  x = 6
  check x == 6
  y = 6
  check y == 6
  `case` := 4
  check `case` == 4

test "pragmas":
  when not defined(nimscript):
    proc foo: int =
      x {.global.} := var 0
      inc x
      result = x
    check foo() == 1
    check foo() == 2

test "more var":
  mut((a, b, c)) := (1, 2, 3)
  (a, b, c) = (6, 5, 4)
  check (a, b, c) == (6, 5, 4)
  (d, e, f) := var (1, 2, 3)
  (d, e, f) = (6, 5, 4)
  check (d, e, f) == (6, 5, 4)

import options
test "options":
  def:
    Some(opt1) = some 4
  check opt1 == 4

  Some(opt2) := some 4
  check opt2 == 4

test "basic tuple":
  (a, b, c) := (1, 2, 3)
  check (a, b, c) == (1, 2, 3)

test "tuple ambiguity":
  (a) := 1
  check a == 1
  (b,) := (2,)
  check b == 2

test "empty tuple acts like discard":
  var modified = false
  () := (modified = true; 0)
  check modified

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
  (d, e) := f := (1, 2)
  check (d, e) == f
  check f == (1, 2)

test "combo":
  (a, Some(mut b), mut c) := (1, some(2), 3)
  check (a, b, c) == (1, 2, 3)
  c = 4
  check c == 4
  b = 5
  check b == 5

test "tuple spread":
  (a, *b) := (1, 2, 3)
  check a == 1
  check b == (2, 3)
  
  (*c, d) := (1, 2, 3)
  check c == (1, 2)
  check d == 3

  (e, *f, g) := (1, 2, 3, 4, 5)
  check e == 1
  check f == (2, 3, 4)
  check g == 5

test "array/seq spread":
  (a, *b, c) := [1, 2, 3, 4, 5]
  (d, *e, f) := @[1, 2, 3, 4, 5]
  check a == 1
  check d == 1
  check b == [2, 3, 4]
  check e == @[2, 3, 4]
  check c == 5
  check f == 5

test "bracket spread":
  [a, *b, c] := [1, 2, 3, 4, 5]
  check:
    a == 1
    b == [2, 3, 4]
    c == 5
  type Foo = enum x, y, z
  arr is array[Foo, int] := [1, 2, 3]
  [z: f, x: d, y: e] := arr
  check (d, e, f) == (1, 2, 3)
  [g] := [1]
  check g == 1

import json

test "custom indices":
  ("name": name, "age": age) := %*{"name": "John", "age": 30}
  check name.getStr == "John"
  check age.getInt == 30

  (0..4: hello, 6..^1: world) := "hello world"
  check hello == "hello"
  check world == "world"

test "type coercion":
  a of int := 4.0
  check a == 4

  type
    Obj = ref object of RootObj
    Obj2 = ref object of Obj
      x: int
  
  var o: Obj
  o = Obj2(x: 3)
  o2 of Obj2 := o
  check o2.x == 3

converter toSeq[I, T](a: array[I, T]): seq[T] = @a

test "type coercion with converter":
  s of seq := [1, 2, 3, 4, 5]
  check s == @[1, 2, 3, 4, 5]

test "type annotation":
  a is uint8 := 4
  check a is uint8
  check a == 4u8

  check not compiles(b is uint8 := -4)

import sequtils

test "unpackArgs":
  proc foo(x: (int, string)) {.unpackArgs: [(a, b): x].} =
    check a == b.len
  
  foo((3, "abc"))
  check @{"a": "b", "c": "d"}.map(
    proc (tup: (string, string)): auto {.unpackArgs: ((a, b) = tup).} =
      a & ':' & b) ==
    @["a:b", "c:d"]

test "checks":
  a := 1
  == a := a
  [0..4: == "hello", 6..^1: == "world"] := "hello world"
  (in [1, 2, 3]) := 2
  (is float) := 2.0
  != 1 := 2
  (notin [1, 2, 3]) := 4
  (isnot int) := 2.0
