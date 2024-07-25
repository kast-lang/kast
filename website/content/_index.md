+++
title = "Juice"
sort_by = "weight"
+++

# Current State

This page describes the design of the language.

The current work is focused on getting a proof of concept implementation of the described features.

**The language is not usable yet**.

# Powerful

While a language being easy to learn is a nice thing to have,
it should not be done at the cost of missing features.

Being a beginner is only temporary,
and thus being beginner friendly is not a priority for Kast.

At the same time, if something can be made simpler without sacrificing power,
it should be made simpler.

# Reliable

Kast is focusing more on program's ease of maintenance
instead of focusing on the speed of prototyping.

The goal is to eliminate as many classes of bugs at compile-time as possible.

# Minimal but General

Kast is designed to have a very simple minimal set of core features,
which can be composed into more complex ones easily.

If a feature can be broken down into simpler one, it should not be in the core.

For example, while the core of the language is purely functional,
we can still have familiar `for` loops, `return`s from functions,
**checked** exceptions from other languages implemented as library features:

```rs
fn find[T](predicate :: T -> bool, values :: [T]) -> T throws[NotFound] {
  for value in list {
    if predicate(value) then
      return value
  }
  throw NotFound
}

let result :: Result<int32, NotFound> = try find(x => x % 2 == 0, [1, 2, 3]);
let value :: int32 = result catch NotFound => -1;
```

This works as a combination of core features like:

- first-class functions
- function contexts
- sum types
- unwinding

Another example of composable feature is async/await,
which also needs delimited continuations in addition to the above

# Compiled or interpreted?

Kast is an interpreter with compiler implemented as a library.

The interpreter still has the "compilation" process,
which is "compiling" the code into IR.
During that "compilation" interpreting might still be triggered
if something needs to be known at "comptime".

By having first-class IR (intermetiate representation as value)
you can write a function that coverts it into the desired target:

```rs
fn vertex_shader(vertex_data :: (pos: vec3)) -> vertex_shader_output {
  ...
}

fn main() io {
  const vertex_shader_glsl :: string = transpile_to_glsl(vertex_shader);
  ...
}

let exe = build_exe(main);
let c_source = transpile_to_c(main);
```

# Performance

The IR should have enough information for the compiler to produce performant code.
For example, compilation target is not required to have a garbage collector.

Those references that can be checked at compile time, are checked.

If you need recursive data structures (for example construct mutually recursive closures),
then they must be explicitly marked as such:

```rs
let arena = rec (
  let f = () => g();
  let g = () => f();
);

arena.f();

```

In this case, both `f` and `g` are going to be freed as soon as the `arena` object itself needs to be.
In there are still references to them when `arena` is dropped,
the borrow checker is going to result in compilation error.

In addition to compile checked borrows,
cell types / reference counting / garbage collector are implementable as a library

# Type system

Kast is a strongly typed language.

## Sum types

aka discriminated union / algebraic data type:

```
let Option = forall T. (Some T | None );
```

# Traits

aka typeclasses

we can associate values of basically any type (we call those types traits)
with any other types:

```
kast: impl Trait for Type as ImplBlock;
haskell: instance Trait Type where ImplBlock;
```

what the compiler (interpreter) does when it sees this declaration is:

```
trait_impls :: Map[Type, Map[trait: Type, Value: trait]];
trait_impls[Type][Trait] = ImplBlock;

impl int32 for MyType as 0
```

The only distinction about the special `trait` types
is that they are aware of the type they are implemented for
(you can refer to them as `Self`):

```
let Parse :: type = trait {
  parse: string -> Self
};
impl Parse for int32 as {
  parse: parse_string_to_int32
}
```

In order to get the value of trait type back, you can use `as` operator:

```
(int32 as Parse) :: Parse

(int32 as Parse).parse :: string -> int32
```

## Tuples

Tuples in Kast can have both named and unnamed fields:

```
let tuple :: (int32, int64, float32, four: int32, five: int32) =
  (1, 2, 3, four: 4, five: 5);
```

Variadic length tuples:

```
let variadic_int :: (*int32) = (1, 2, 3, 4)
```

Variadic tuples allow for variadic generics easily:

```
forall (*fields :: type). (
  impl Trait for (*fields)
)
```

## Type inference

Kast implements an inference algorithm based on the Hindley–Milner approach,
which allows most of types in the program to be inferred.

# Function contexts

Not every function in Kast can just be called whenever.
Functions can specify contexts that must be in scope when calling them.
Contexts can be of any type:

```
fn function_that_allocates_a_lot() allocator { ... }
```

This function specifies that a allocator must exist.
In a way, function contexts act like implicit arguments (but implemented differently).

In order to bring a context into scope, use `with`:

```
with ArenaAllocator.new() (
  function_that_allocates_a_lot()
)
```

## overflows

```
add_int32 :: (a :: int32, b :: int32) -> int32 potentially_overflows;

with saturating (
  a + with overflowing (b + c)
)

# for compiler to optimize the checks away
with undefined_behavior_on_overflow (
  a + b
)
```

# Unwinding

Similar to lisp's [`block`/`return-from`](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/speope_return-from.html)

We can declare named blocks that we can return to without executing the rest of the body:

```
let f = fn (token :: unwind_token) {
  ...
  unwind (~token, ~value);
  ...
};
unwindable_block f
```

`unwindable_block` creates the block and takes a function of type `unwind_token -> T`
and executes it immediately with the token of that block.

When `unwind :: (~token :: unwind_token, ~value :: T)` function is used from within,
it stops executing current function and unwinds the stack until we return to the block.
The value of the block if either the result of executing the function (if `unwind` was not called),
or the value given to the `unwind`.

# Delimited continuations

TODO

# Syntax

Kast's syntax is not static, but dynamic.
The programmer can define their own syntax easily.

The default syntax is declared in std and can be not used if you wish.

The only builtin syntax is the syntax for defining new syntax:

```
syntax ternary <- 10 = cond "?" then ":" else;
```

This defines left associative (`<-`) syntax for ternary operator with priority of `10`.
When Kast sees this in the source code, its being transformed into an AST:

```
let ternary_ast :: type = (cond: ast, then: ast, else: ast);
```

When compilation happes, the compiler is trying to find
a matching macro for the name given (`ternary` in this case),
which is being invoked and evaluated into a new ast
which is then processed recursively until some builtin macro converts it into IR

Macros in Kast are just like normal functions:

```
let ternary = macro (~cond :: ast, ~then :: ast, ~else :: ast) =>
  `(if $cond then $then else $else)
```

Here, we use a quote operator (`\``) to construct the resulting ast
using already existing syntax.
The unquote operator (`$`) is replacing the following ident with the ast
that was passed to the macro as argument.
