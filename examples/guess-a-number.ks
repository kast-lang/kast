use std.prelude.*;
let main = () => (
  print "Welcome to the Guessing Number Game :-)";
  let guessed :: string = std.rng.gen_int32 10;
  print guessed;
  let x = std.rng.gen_int32 10;
  print x;
);
main ();
