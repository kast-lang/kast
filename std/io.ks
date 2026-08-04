module:
const print = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.print")(line)
        | target.name == "c" => (
            @native "Kast_write(stdout, \(line))";
            @native "fprintf(stdout, \"\\n\")";
        )
        | target.name == "javascript" => (@native "Kast.io.print")(line)
    )
);
# similar to print, but print to stderr
const eprint = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.eprint")(line)
        | target.name == "c" => (
            @native "Kast_write(stderr, \(line))";
            @native "fprintf(stderr, \"\\n\")";
        )
        | target.name == "javascript" => (@native "Kast.io.eprint")(line)
    )
);
const input = async (prompt :: String) -> String => (
    @cfg (
        | target.name == "interpreter" => (@native "io.input")(prompt)
        | target.name == "c" => @native "Kast_input(\(prompt))"
        | target.name == "javascript" => (@native "Kast.io.input")(prompt)
    )
);

const stdout = (
    module:

    const isatty = () -> Bool => @cfg (
        | target.name == "interpreter" => (@native "io.stdout.isatty")()
        | target.name == "c" => @native "Kast_isatty(stdout)"
        | target.name == "javascript" => (@native "Kast.io.stdout.isatty")()
    );

    const write = (line :: String) -> () => @cfg (
        | target.name == "interpreter" => (@native "io.stdout.write")(line)
        | target.name == "c" => (
            @native "Kast_write(stdout, \(line))";
        )
        | target.name == "javascript" => (@native "Kast.io.stdout.write")(line)
    );
);

const stderr = (
    module:

    const isatty = () -> Bool => @cfg (
        | target.name == "interpreter" => (@native "io.stderr.isatty")()
        | target.name == "c" => @native "Kast_isatty(stderr)"
        | target.name == "javascript" => (@native "Kast.io.stderr.isatty")()
    );

    const write = (line :: String) -> () => @cfg (
        | target.name == "interpreter" => (@native "io.stderr.write")(line)
        | target.name == "c" => (
            @native "Kast_write(stderr, \(line))";
        )
        | target.name == "javascript" => (@native "Kast.io.stderr.write")(line)
    );
);

const stdin = (
    module:

    const isatty = () -> Bool => @cfg (
        | target.name == "interpreter" => (@native "io.stdin.isatty")()
        | target.name == "c" => @native "Kast_isatty(stdin)"
        | target.name == "javascript" => (@native "Kast.io.stdin.isatty")()
    );

    const read_until = (c :: Char) -> String => @cfg (
        | target.name == "interpreter" => (@native "io.stdin.read_until")(c)
        | target.name == "c" => @native "Kast_read_until(stdin, \(c))"
        | target.name == "javascript" => (@native "Kast.io.stdin.read_until")(c)
    );

    const read_exactly = (bytes :: Int32) -> String => @cfg (
        | target.name == "interpreter" => (@native "io.stdin.read_exactly")(bytes)
        | target.name == "c" => @native "Kast_read_exactly(stdin, \(bytes))"
        | target.name == "javascript" => (@native "Kast.io.stdin.read_exactly")(bytes)
    );

    const read_to_end = () -> String => @cfg (
        | target.name == "interpreter" => (@native "io.stdin.read_to_end")()
        | target.name == "c" => @native "Kast_read_to_end(stdin)"
        | target.name == "javascript" => (@native "Kast.io.stdin.read_to_end")()
    );
);
