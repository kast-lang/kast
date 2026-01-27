module:
const std = @current_scope;
include "./syntax.ks";

const Type = @native "Type";

const UnwindToken = [T :: Type] ((@native "UnwindToken")(T) :: Type);
const UnwindUnit = UnwindToken[type ()];
const LoopBlock = @context UnwindUnit;
const LoopBody = @context UnwindUnit;
# const ReturnBlock = @context newtype (
#     .T :: Type,
#     .token :: Any,
# );
const Bool :: Type = @native "Bool";
const Int32 :: Type = @native "Int32";
const UInt32 = Int32;
const Int64 :: Type = @native "Int64";
const Float64 :: Type = @native "Float64";
const Char :: Type = @native "Char";
const String :: Type = @native "String";
const Ast :: Type = @native "Ast";

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
    | target.name == "interpreter" => (@native "panic")(s)
    | target.name == "javascript" => (@native "Kast.panic")(s)
);
const type_of_value = [T] (x :: T) -> Type => T;

const prelude = include "./prelude.ks";
