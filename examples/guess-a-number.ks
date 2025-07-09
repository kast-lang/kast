use std.prelude.*;
let main = () => (
  print "Welcome to the Guessing Number Game :-)";
  let picked = std.rng.gen_int32 10;
  print "The number has been picked!";
  # print <| int32_to_string picked;
  loop (
    let guess = input "Guess: " |> string_to_int32;
    if picked < guess then (
      print "Less!"
    ) else if picked > guess then (
      print "Greater!"
    ) else (
      print "You guessed!";
    );
  );
);
main ();
