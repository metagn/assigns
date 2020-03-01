# definesugar

`:=` and `def` macros to allow custom/complicated assignment for Nim.

Uses a kind of "polymorphic macros" to function.

Examples (more in tests):

```nim
import definesugar

def:
  a = 3
  mut x = 5
x = 6

a := 3
mut(x) := 5 # := has different precedence than =, you have to use parentheses
x = 6

# tuples:
(a, b) := (1, 2)

# named tuples/generic field access:
type Person = tuple[name: string, age: int]
(age: a, name: n) := ("John Smith", 30).Person

# aliases:
(a, b) as c := (1, 2)
# c == (1, 2)
# (a, b) == (1, 2)

# nesting:
(a, (b, c), d) := (1, (2, 3), 4)

# options:
import options
let opt = some(4)
def:
  Some a = opt
Some(a) = opt
echo a # 4

# custom types:
type Result[T] = object
  case success: bool
  of true:
    value: T
  else:
    error: ref Exception

import macros
macro define[T](lhs; rhs: Result[T]): untyped =
  if lhs.kind == nnkPrefix and lhs[0].eqIdent"?":
    let tmp = genSym(nskLet, "tmpResult")
    result = newStmtList(
      newLetStmt(tmp, rhs),
      quote do:
        if not `tmp`.success:
          raise `tmp`.error,
      # openDefine means use any custom overload of define for the rest
      openDefine(lhs[1], newDotExpr(tmp, ident"value"))
    )
  else:
    # defaultDefine is a proc(NimNode, NimNode): NimNode and is what is applied by default if no overload is found
    result = defaultDefine(lhs, rhs)

let successful = Result[int](success: true, value: 5)
?x := successful
echo x # 5
```

Not implemented yet:

* Exported variables, ie `let a* = 3`. Don't know how to do this with regular Nim syntax since `export` is also a keyword
* Splatting in tuples, like `(a, *bc, d) := (1, 2, 3, 4)`. Very doable, I know how to do this