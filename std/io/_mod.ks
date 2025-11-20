module:
const print = (line :: string) -> () => (
    cfg_if (
        | target.name == "interpreter" => (@native "print") line
        | target.name == "ocaml" => (@native "@natives.print") line
        # | target.name == INTERPRETER_NAME => (@native "print") line
        # | target.name == "ocaml" => (@native "@natives.print") line
    )
);
const input = (prompt :: string) -> string => (
    cfg_if (
        | target.name == "interpreter" => (@native "input") prompt
        | target.name == "ocaml" => (@native "@natives.input") prompt
    )
)
