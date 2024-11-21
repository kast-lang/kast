module:

const @"syntax" = import "./syntax.ks";

impl syntax @"syntax".invoke_macro = macro (.@"macro", .arg) => `(
  compile_ast ($@"macro" !! `($arg))
);

impl syntax @"syntax".pipe_right = macro (.arg, .f) => `(
  let arg = $arg;
  let f = $f;
  f arg
);
impl syntax @"syntax".pipe_left = macro (.f, .arg) => `(
  let f = $f;
  let arg = $arg;
  f arg
);
impl syntax @"syntax".let_infer = macro (.pattern) => `(
  (let $pattern = _; $pattern)
);

const type = native "type";
const ast :: type = native "ast";

const bool :: type = native "bool";

impl syntax @"syntax".@"true" = macro _ => `(native "true");
impl syntax @"syntax".@"false" = macro _ => `(native "false");

const int32 :: type = native "int32";
const int64 :: type = native "int64";
const float64 :: type = native "float64";
const string :: type = native "string";

const output :: type = native "output";

const default_number_type :: type = native "default_number_type";

# TODO panic should return never
const panic :: string -> () = native "panic";

const print :: string -> () with output = line => (
  let output = current output;
  output.write line;
  output.write "\n";
);

const contains :: (.s = string, .substring = string) -> bool = native "contains";

const dbg = forall[T] {
  (value :: T) => (
    let output = current output;
    output.write <| native "dbg" value;
    output.write " :: ";
    output.write <| native "dbg_type" T;
    output.write "\n";
  )
};

# TODO `:: type` should be inferred
const Option = forall[T :: type] { newtype :Some T | :None };

const Either = forall[.left :: type, .right :: type] { newtype :Left left | :Right right };

const Result = forall[.ok :: type, .error :: type] { newtype :Ok ok | :Error error };

# TODO where T: Num or smth
impl syntax @"syntax".@"op binary +" = forall[T] { native "+" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary -" = forall[T] { native "-" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary <" = forall[T] { native "<" :: (.lhs = T, .rhs = T) -> bool };

const TypeName = (.name = string) as type;

impl int32 as TypeName = (.name = "int32");

const Parse = forall[Self] {
  .parse = string -> Self,
};

impl int32 as Parse = (
  .parse = native "parse",
);

impl int64 as Parse = (
  .parse = native "parse",
);

impl float64 as Parse = (
  .parse = native "parse",
);

const parse = forall[T] {
  (T as Parse).parse
};
