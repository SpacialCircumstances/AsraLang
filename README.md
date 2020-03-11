# AsraLang

My second attempt at writing a compiler.
Asra (Quenya for "simple") was supposed to be a basic, functional programming language that compiles to Javascript.
At the current stage, the language supports curried functions, recursion and lists.
The syntax is very simple and uses prefix notation. 
All control flow is implemented in JS functions, so if has the type signature `if: (unit -> bool) (unit -> 'a) (unit -> 'a) -> 'a`.
This makes implementing the language very simple because all code can be compiled to bindings and function calls.


During this project, it turned out that writing parsers is harder than I thought. Therefore, the parser is full of bugs.
This enlightenment was also found on other areas: Writing a typechecker for a HM-style type system should probably not (at least not by me) be done without researching first.
Still, the typechecker I came up with works *somewhat* and at least uses the right principles (unification *almost* works).

The compiler is at a stage where simple programs can be compiled and typechecked, even with the broken, unsound typechecker.

## Example

Creating an array and mapping over it.

```
testArray = newArray 5 [ i: Number -> i ]
forEach println testArray
println (map [ x: Number -> + x 2 ] testArray)
secondArray = { "a"; "b"; "c" }
println secondArray
```
