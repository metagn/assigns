# defines

Macros to allow custom/complicated assignment for Nim. Allows for overloading with typed macros.

Examples (more in tests):

```nim
import defines
  
# regular definition
a := b

# definition block
def: a = b
def:
  a = b
  c = d

# scoped definition block
def:
  a = b
do:
  use a

# mutable variable definition
mut(a) := b
a := var b
def: mut a = b

# assignment to existing variable
a ::= b

# aliasing
a as b := c
a := b := c

# literal assertion (for matching)
1 := 2 # => doAssert 1 == 2

# collection unpacking (works for anything if it can be indexed with integers):
(a, b, c) := d

# collection spreading (works if you can do d[i], d[i..^j] and d[^i]):
(a, *b, c) := d
(a, ..b, c) := d
(a, ...b, c) := d

# nesting (applies to everything)
(a, b) as mut(c) := d # => let temp = d; let a = temp[0]; let b = temp[1]; var c = temp

# empty tuple unpacking, discards right hand side:
() := a

# named unpacking, works on anything (prop1 and prop2 must be identifiers):
(prop1: a, prop2: b) := c

# unpacking by given index:
(0..4: hello, 6..^1: world) := "hello world"
import json
("name": name, "age": age) := %*{"name": "John", "age": 30}

# conversion to type:
a of int := 4.0
type Obj = ref object of RootObj
type Obj2 = ref object of Obj
let x: Obj = Obj2()
y of Obj2 := x

# type annotation (must be directly tied to variable names):
a is uint8 := 4

# option unpacking, only custom type implementation that comes with the library:
import options
Some(a) := b
some(a) := b
def: some a = b
```

You can overload definitions for types like so:

```nim
import defines, macros

type Result[T] = object
  case success: bool
  of true:
    value: T
  else:
    error: ref Exception

macro define[T](lhs; rhs: Result[T], kind: static[DefineKind]): untyped =
  if lhs.kind == nnkPrefix and lhs[0].eqIdent"?":
    let tmp = genSym(nskLet, "tmpResult")
    # openDefine means use any custom overload of define for the rest
    let asgn = openDefine(lhs[1], newDotExpr(tmp, ident"value"), kind)
    result = quote do:
      let `tmp` = `rhs`
      if not `tmp`.success:
        raise `tmp`.error
      `asgn`
  else:
    # defaultDefine is a proc(NimNode, NimNode): NimNode and is what is applied by default if no overload is found
    result = defaultDefine(lhs, rhs, kind)
# alternatively
implementDefine Result: # implementDefineExported is the exported version
  if lhs.kind == nnkPrefix and lhs[0].eqIdent"?":
    let tmp = genSym(nskLet, "tmpResult")
    let asgn = open(lhs[1], newDotExpr(tmp, ident"value"))
    result = quote do:
      let `tmp` = `rhs`
      if not `tmp`.success:
        raise `tmp`.error
      `asgn`
  else:
    result = default(lhs, rhs)

let successful = Result[int](success: true, value: 5)
?x := successful
echo x # 5
```

Not implemented:

* Exported variables, ie `let a* = 3`. I don't think the complexity this will add, both to the syntax and the behavior of the macros, is worth it. You would have to do `export a` after for now.
* Const variables, doesn't make very much sense IMO. Should be pretty trivial to support if needed though
* Type definitions. As much as I think type definitions need some improvement or sugar, I think that is out of the scope of this package, and there are a lot of existing packages with differing opinions.
* Procs/lambdas. I think ``sugar.`=>` `` is good enough for procs and `unpackArgs` exists if you want to unpack arguments.
