module:
const print = (line :: String) -> () => (
    cfg_if (
        | target.name == "interpreter" => (@native "print") line
        | target.name == "ocaml" => (@native "@natives.print") line
        # | target.name == INTERPRETER_NAME => (@native "print") line
        # | target.name == "ocaml" => (@native "@natives.print") line
    )
);
# similar to print, but print to stderr
const eprint = (line :: String) -> () => (
    cfg_if (
        | target.name == "interpreter" => (@native "eprint") line
        | target.name == "ocaml" => (@native "@natives.eprint") line
    )
);
const input = (prompt :: String) -> String => (
    cfg_if (
        | target.name == "interpreter" => (@native "input") prompt
        | target.name == "ocaml" => (@native "@natives.input") prompt
    )
)
