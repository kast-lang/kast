module:
const std = @current_scope;
include "./syntax.ks";
const UnwindUnit = (@native "unwind_token") type ();
const LoopBlock = @context UnwindUnit;
const LoopBody = @context UnwindUnit;

const bool :: type = @native "bool";
const int32 :: type = @native "int32";
const uint32 = int32;
const int64 :: type = @native "int64";
const float64 :: type = @native "float64";
const char :: type = @native "char";
const string :: type = @native "string";

const cmp = include "./cmp.ks";
const op = include "./op.ks";

const Char = include "./char.ks";
const String = include "./string.ks";

const Option = include "./option.ks";
const dbg = include "./dbg.ks";
const convert = include "./convert.ks";
const io = include "./io/_mod.ks";
const path = include "./path.ks";
const fs = include "./fs/_mod.ks";
const net = include "./net/_mod.ks";
const sys = include "./sys/_mod.ks";
const rng = include "./rng/_mod.ks";
const collections = include "./collections/_mod.ks";

const panic = [T] (s :: string) -> T => cfg_if (
    | target.name == "interpreter" => (@native "panic") s
);
const type_of_value = [T] (x :: T) -> type => T;

const prelude = include "./prelude.ks";
