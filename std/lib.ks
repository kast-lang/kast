module:
include "./syntax.ks";
const bool :: type = native "bool";
const int32 :: type = native "int32";
const string :: type = native "string";
const int32_to_string :: int32 -> string = native "int32_to_string";
const string_to_int32 :: string with fail -> int32 = native "string_to_int32";
const cmp = include "./cmp.ks";
const io = include "./io/_mod.ks";
const rng = include "./rng/_mod.ks";
const prelude = include "./prelude.ks";
