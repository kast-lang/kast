use std.*;

let run_once = fn (.quit, .first) {
    let picked :: int32 = random (.min = 1, .max = 10);
    print (
        if first then
            "a number has been picked"
        else
            "a number has been picked again"
    );
    print "guess:";
    loop {
        let s = read_line ();
        if s == "exit" then (
            let _ = unwind quit "quitted";
        );
        let guessed = &s |> parse;
        if guessed == picked then (
            break
        ) else (
            if picked < guessed then
                print "less! guess again:"
            else
                print "greater! guess again:";
        )
    };
    print "you have guessed the number! congrats!";
};

let main = fn (void) {
    let result = unwindable quit (
        run_once (.quit, .first = true);
        loop {
            run_once (.quit, .first = false);
        };
        "not quitted"
    );
    print result;
};

main ();
