## Provides default overloads and utilities for macros of definitions.
## 
## Examples
## ========
## 
## .. code-block::nim
##     
##   # regular definition
##   a := b
##   
##   # definition block
##   def: a = b
##   def:
##     a = b
##     c = d
##   
##   # scoped definition block
##   def:
##     a = b
##   do:
##     use a
##   
##   # mutable variable definition
##   @a := b
##   a := var b
##   def: @a = b
##   
##   # assignment to existing variable
##   ^a := b
##   
##   # aliasing
##   a as b := c
##   a := b := c
##   
##   # checks (for matching)
##   == 1 := 2 # raises exception
##   1 := 2 # same, literals inferred to be equality checks
##   (== ok, res) := (ok, 1)
##   (in [1, 2, 3]) := 2
##   (is float) := 2.0
##   != 1 := 2
##   (notin [1, 2, 3]) := 4
##   (isnot int) := 2.0
##   
##   # collection unpacking (works for anything if it can be indexed with integers):
##   (a, b, c) := d
##   (a) := d # single parens does not unpack, equivalent to a := b
##   (a,) := d # equivalent to a := b[0]
## 
##   # default syntax supports brackets instead of parens:
##   [a, b, c] := d
##   [a] := d # equivalent to a := d[0]
##   
##   # collection spreading (works if you can do d[i], d[i..^j] and d[^i]):
##   (a, *b, c) := d
##   (a, ..b, c) := d
##   (a, ...b, c) := d
##   
##   # nesting (applies to everything)
##   (a, b) as @c := d # => let temp = d; let a = temp[0]; let b = temp[1]; var c = temp
##   
##   # empty tuple unpacking, discards right hand side:
##   () := a
##   
##   # named unpacking, works on anything (prop1 and prop2 must be identifiers):
##   (prop1: a, prop2: b) := c
## 
##   # unpacking by given index:
##   (0..4: hello, 6..^1: world) := "hello world"
##   import json
##   ("name": name, "age": age) := %*{"name": "John", "age": 30}
## 
##   # indexing by identifier if brackets used instead of parens (for enum indexed arrays):
##   [index1: a, index2: b] := c
##   
##   # conversion to type:
##   a of int := 4.0
##   type Obj = ref object of RootObj
##   type Obj2 = ref object of Obj
##   let x: Obj = Obj2()
##   y of Obj2 := x
##   
##   # type annotation (must be directly tied to identifiers):
##   a is uint8 := 4
##   
##   # option unpacking, only custom type implementation that comes with the library:
##   Some(a) := b
##   some(a) := b
## 
## Custom definitions
## ==================
## 
## A large and important feature in this package is that you can overload
## definitions for custom types by importing `assigns/impl <impl.html>`_.
## The `assign` macro in this module is overloadable, and the `openAssign` template creates a
## NimNode that calls a forced open symbol of `assign` with the AST of the left hand side,
## AST of the right hand side, and the flag of whether or not it is a `let`, `var`, or mutating
## assignment. You can use the `implementAssign` and `implementAssignExported` templates as a
## shorthand for declaring these overloads.

import assigns/[syntax, tupleindex, impl]
export syntax, tupleindex, impl.assign
