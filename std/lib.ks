module:
const std = @current_scope;
include "./syntax.ks";
const unwind_unit = (@native "unwind_token") type ();
_ :: unwind_unit => ();
const loop_block = @context unwind_unit;
#
const bool :: type = @native "bool";
const int32 :: type = @native "int32";
const int64 :: type = @native "int64";
const char :: type = @native "char";
const string :: type = @native "string";
#
const dbg = include "./dbg.ks";
const op :: _ = include "./op.ks";
const cmp :: _ = include "./cmp.ks";
const io :: _ = include "./io/_mod.ks";
const String = include "./string.ks";
const path = include "./path.ks";
const fs = include "./fs/_mod.ks";
const sys = include "./sys/_mod.ks";
const rng :: _ = include "./rng/_mod.ks";
const collections = include "./collections/_mod.ks";
const int32_to_string :: int32 -> string = num => (
    cfg_if (
        | target.name == "interpreter" => (@native "int32_to_string") num
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
const string_to_int32 :: string -> int32 = s => (
    cfg_if (
        | target.name == "interpreter" => (@native "string_to_int32") s
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
const int64_to_string :: int64 -> string = num => (
    cfg_if (
        | target.name == "interpreter" => (@native "int64_to_string") num
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
const string_to_int64 :: string -> int64 = s => (
    cfg_if (
        | target.name == "interpreter" => (@native "string_to_int64") s
        | target.name == "ocaml" => @native "@natives.todo()"
    )
);
const prelude :: _ = include "./prelude.ks";
