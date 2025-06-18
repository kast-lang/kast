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
        let guessed = parse &s; # TODO &s |> parse
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
    cfg_if {
        | target.is_web => with ((
            const document = std.web.document;
            let pre = document.createElement("pre");
            document.body.appendChild(pre);
            (.write = native "async (_ctx, arg) => {
                const s = arg.get();
                $(pre).innerText += s;
            }")
        ) :: output)
        | true => ()
    };
    cfg_if {
        | target.is_web => with ((
            const document = std.web.document;
            let input = document.createElement("input");
            document.body.appendChild(input);
            (.read_line = native "(_ctx, _args) => {
                const input = $(input);
                return new Promise((resolve, reject) => {
                    function on_key_down(e) {
                        if (e.key === 'Enter') {
                            resolve(e.currentTarget.value);
                            e.currentTarget.value = '';
                            input.removeEventListener('keydown', on_key_down);
                        }
                    }
                    input.addEventListener('keydown', on_key_down);
                });
            }")
        ) :: input)
        | true => ()
    };
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
