module:
include "./syntax.ks";
const bool :: type = native "bool";
const int32 :: type = native "int32";
const string :: type = native "string";
const int32_to_string :: int32 -> string = native "int32_to_string";
const string_to_int32 :: string with fail -> int32 = native "string_to_int32";
const op :: _ = include "./op.ks";
const cmp :: _ = include "./cmp.ks";
const io :: _ = include "./io/_mod.ks";
const rng :: _ = include "./rng/_mod.ks";
const prelude :: _ = include "./prelude.ks";
