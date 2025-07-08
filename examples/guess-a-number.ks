use std.prelude.*;
let main = () => (
  print "Welcome to the Guessing Number Game :-)";
  let guessed = std.rng.gen_int32 10;
  print "The number has been guessed!";
  let guess = input "Guess: " |> string_to_int32;
);
main ();
