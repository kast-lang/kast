# Kast

An experimental programming language

## Todo

- Mark builtin fns with contexts (e.g. print should require io)
- Sum types should have better syntax (fix "ofnone"). I think it should be "let binding" for bindings and "variant" for variants?
- Fix the bugs
- Unnamed tuples
- Lists
- Better error reporting (stack traces / reasons why syntax is not parsed)
- Inference for generic function calls: `f[int32](5)` should just be able to do `f(5)`.
- Ir as value so we can write compiler (glsl)
- Traits
- Delimited continuations
- Effect system (half done, have contexts)
- More examples
- Borrow checker
- Tests
