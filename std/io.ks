module:
const print = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.print")(line)
        | target.name == "javascript" => (@native "Kast.io.print")(line)
    )
);
# similar to print, but print to stderr
const eprint = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.eprint")(line)
        | target.name == "javascript" => (@native "Kast.io.eprint")(line)
    )
);
const input = async (prompt :: String) -> String => (
    @cfg (
        | target.name == "interpreter" => (@native "io.input")(prompt)
        | target.name == "javascript" => (@native "Kast.io.input")(prompt)
    )
);

const stdout = (
    module:

    const write = (line :: String) -> () => (
        @cfg (
            | target.name == "interpreter" => (@native "io.stdout.write")(line)
            | target.name == "javascript" => (@native "Kast.io.stdout.write")(line)
        )
    );
);

const stderr = (
    module:

    const write = (line :: String) -> () => (
        @cfg (
            | target.name == "interpreter" => (@native "io.stderr.write")(line)
            | target.name == "javascript" => (@native "Kast.io.stderr.write")(line)
        )
    );
);

const stdin = (
    module:

    const read_until = (c :: Char) -> String => (
        @cfg (
            | target.name == "interpreter" => (@native "io.stdin.read_until")(c)
            | target.name == "javascript" => (@native "Kast.io.stdin.read_until")(c)
        )
    );

    const read_exactly = (bytes :: Int32) -> String => (
        @cfg (
            | target.name == "interpreter" => (@native "io.stdin.read_exactly")(bytes)
            | target.name == "javascript" => (@native "Kast.io.stdin.read_exactly")(bytes)
        )
    );

    const read_to_end = () -> String => (
        @cfg (
            | target.name == "interpreter" => (@native "io.stdin.read_to_end")()
            | target.name == "javascript" => (@native "Kast.io.stdin.read_to_end")()
        )
    );
);