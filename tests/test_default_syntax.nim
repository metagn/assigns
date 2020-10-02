import unittest, definesugar

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

import json

test "custom indices":
  ("name": name, "age": age) := %*{"name": "John", "age": 30}
  check name.getStr == "John"
  check age.getInt == 30

  (0..4: hello, 6..^1: world) := "hello world"
  check hello == "hello"
  check world == "world"
