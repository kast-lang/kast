use std.prelude.*;
let main = () with io => (
    print "Welcome to the Guessing Number Game :-)";
    let picked = std.rng.gen_int32 (.min = 1, .max = 10);
    print "The number has been picked!";
    # print <| int32_to_string picked;
    let first = true;
    unwindable block (
        loop (
            let prompt = if first then "Guess: " else "Guess again: ";
            first = false;
            let guess = input prompt |> string_to_int32;
            if picked < guess then (
                print "Less!"
            ) else if picked > guess then (
                print "Greater!"
            ) else (
                print "You guessed!";
                unwind block ();
            );
        );
    );
);
main ();
