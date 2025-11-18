module:
include "./syntax.ks";
const unwind_unit = (native "unwind_token") type ();
_ :: unwind_unit => ();
const loop_block = @context unwind_unit;
#
const bool :: type = native "bool";
const int32 :: type = native "int32";
const string :: type = native "string";
#
const op :: _ = include "./op.ks";
const cmp :: _ = include "./cmp.ks";
const io :: _ = include "./io/_mod.ks";
const rng :: _ = include "./rng/_mod.ks";
const int32_to_string = num => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "int32_to_string") num
        | (native "==") (target.name, "ocaml") => native "Natives.todo()"
    )
);
const string_to_int32 = s => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "string_to_int32") s
        | (native "==") (target.name, "ocaml") => native "Natives.todo()"
    )
);
const prelude :: _ = include "./prelude.ks";
