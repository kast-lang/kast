use std.prelude.*;

let main = () with io => (
    print "Welcome to the Guessing Number Game :-)";
    let picked :: Int32 = std.random.gen_range (.min = 1, .max = 10);
    print "The number has been picked!";
    # dbg.print (.picked);
    let mut first = true;
    loop (
        let prompt = if first then "Guess: " else "Guess again: ";
        first = false;
        let guess = input prompt |> String.parse;
        if picked < guess then (
            print "Less!"
        ) else if picked > guess then (
            print "Greater!"
        ) else (
            print "You guessed!";
            break;
        );
    );
);
main ();
