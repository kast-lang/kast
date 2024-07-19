+++
title = "Juice"
sort_by = "weight"
+++

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

PascalCase
camelCase
snake_case
SHOUTY_CASE
kebab-case

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

# Delimited continuations

# Syntax

dynamic


