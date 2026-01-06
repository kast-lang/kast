module:
const std = @current_scope;
include "./syntax.ks";
const UnwindUnit = (@native "unwind_token") type ();
const LoopBlock = @context UnwindUnit;
const LoopBody = @context UnwindUnit;

const Bool :: type = @native "bool";
const Int32 :: type = @native "int32";
const UInt32 = Int32;
const Int64 :: type = @native "int64";
const Float64 :: type = @native "float64";
const Char :: type = @native "char";
const String :: type = @native "string";

const cmp = include "./cmp.ks";
const op = include "./op.ks";

include "./char.ks";
include "./string.ks";

const range = include "./range.ks";
const iter = include "./iter.ks";
const Option = include "./option.ks";
const dbg = include "./dbg.ks";
const convert = include "./convert.ks";
const io = include "./io.ks";
const path = include "./path.ks";
const fs = include "./fs.ks";
const net = include "./net.ks";
const sys = include "./sys.ks";
const random = include "./random.ks";
const collections = include "./collections/_mod.ks";

const panic = [T] (s :: String) -> T => @cfg (
    | target.name == "interpreter" => (@native "panic") s
    | target.name == "javascript" => (@native "Kast.panic") s
);
const type_of_value = [T] (x :: T) -> type => T;

const prelude = include "./prelude.ks";
