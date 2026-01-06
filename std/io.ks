module:
const print = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.print") line
        | target.name == "ocaml" => (@native "@natives.print") line
        | target.name == "javascript" => (@native "console.log") line
    )
);
# similar to print, but print to stderr
const eprint = (line :: String) -> () => (
    @cfg (
        | target.name == "interpreter" => (@native "io.eprint") line
        | target.name == "ocaml" => (@native "@natives.eprint") line
    )
);
const input = (prompt :: String) -> String => (
    @cfg (
        | target.name == "interpreter" => (@native "io.input") prompt
        | target.name == "ocaml" => (@native "@natives.input") prompt
        | target.name == "javascript" => (@native "Kast.io.input") prompt
    )
)
