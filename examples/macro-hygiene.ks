use std.*;

const let_hello = macro _ => `(
  let hello = "hello";
  print hello;
); 
const let_named = macro (name :: ast) => `(
  let $name = "named";
  print $name;
);

let hello = "hi";
print hello;       # hi
let_hello!();      # hello
print hello;       # still hi
let_named!(hello); # named
print hello;       # named
