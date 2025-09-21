module:
const print = (line :: string) -> () => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "print") line
        | (native "==") (target.name, "ocaml") => (native "Natives.print") line
        # | target.name == INTERPRETER_NAME => (native "print") line
        # | target.name == "ocaml" => (native "Natives.print") line
    )
);
const input = (prompt :: string) -> string => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "input") prompt
        | (native "==") (target.name, "ocaml") => (native "Natives.input") prompt
    )
)
