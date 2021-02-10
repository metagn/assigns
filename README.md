# assigns

Disclaimer: [fusion/matching](https://nim-lang.github.io/fusion/src/fusion/matching.html) has
passed this library by a fair amount, this package probably has little need for new changes.
I would tag this as "abandoned" or "archived" in Nimble if it was possible.
If you want new changes, you can fork and even republish on Nimble by linking
the Nimble package to your repository.

Macros to allow custom/complicated assignment for Nim. Allows for overloading with typed macros.

Examples in [docs](https://hlaaftana.github.io/assigns/assigns.html#examples) and in tests.

Not implemented:

* Exported variables, ie `let a* = 3`. I don't think the complexity this will add, both to the syntax and the behavior of the macros, is worth it. You would have to do `export a` after for now.
* Const variables, doesn't make very much sense IMO. Should be pretty trivial to support if needed though
* Type definitions. As much as I think type definitions need some improvement or sugar, I think that is out of the scope of this package, and there are a lot of existing packages with differing opinions.
* Procs/lambdas. I think ``sugar.`=>` `` is good enough for procs and `unpackArgs` exists if you want to unpack arguments.
