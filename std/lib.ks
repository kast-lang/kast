module:
const std = @current_scope;
include "./syntax.ks";
const unwind_unit = (@native "unwind_token") type ();
const loop_block = @context unwind_unit;
const loop_body = @context unwind_unit;
# TODO module types
const bool :: type = @native "bool";
const int32 :: type = @native "int32";
const uint32 = int32;
const int64 :: type = @native "int64";
const float64 :: type = @native "float64";
const char :: type = @native "char";
const string :: type = @native "string";
#
const dbg = include "./dbg.ks";
const convert = include "./convert.ks";
const op = include "./op.ks";
const cmp = include "./cmp.ks";
const io = include "./io/_mod.ks";
const Char = include "./char.ks";
const String = include "./string.ks";
const path = include "./path.ks";
const fs = include "./fs/_mod.ks";
const sys = include "./sys/_mod.ks";
const rng = include "./rng/_mod.ks";
const collections = include "./collections/_mod.ks";
const panic = [T] (s :: string) -> T => cfg_if (
    | target.name == "interpreter" => (@native "panic") s
);
const prelude = include "./prelude.ks";
