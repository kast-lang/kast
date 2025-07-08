use std.prelude.*;
let main = () => (
  print "Welcome to the Guessing Number Game :-)";
  let guessed = std.rng.gen_int32 10;
  print "The number has been guessed!";
  let guess = input "Guess: ";
  let guess = std.string_to_int32 guess;
);
main ();
