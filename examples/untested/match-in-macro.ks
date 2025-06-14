use std.*;

const test = macro _ => `(
  match (123 :: int32) { value => value }
);

dbg (match (123 :: int32) { value => value });
dbg test!();
