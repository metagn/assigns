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

# nesting (applies to everything)
a as mut(b) := c # => let temp = c; let a = temp; var b = temp

# literal assertion (for matching)
1 := 2 # => doAssert 1 == 2

# collection unpacking (works for anything if it can be indexed with integers):
(a, b, c) := d

# collection spreading (works if you can do d[i], d[i..^j] and d[^i]):
(a, *b, c) := d

# empty tuple unpacking, discards right hand side:
() := a

# named unpacking, works on anything:
(prop1: a, prop2: b) := c

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

* Exported variables, ie `let a* = 3`. Could be done like `a.export := 3`, but that looks weird. Either way, this would need yet another flag to propagate between overloads and doesn't seem that useful to me.
* Type annotations, would probably use `is` because `of` would be nice for objects with inheritance. This would make it
look ugly though so I'm not sure if it's worth it.
