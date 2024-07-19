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

# Reliability

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

# Syntax

dynamic

# Memory safety without garbage collector

# Types

# Traits

aka typeclasses

## Sum types

## Type inference

# Function contexts

# Unwinding

# Delimited continuations

