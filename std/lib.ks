module:

const @"syntax" = import "./syntax.ks";

const type = native "type";
const ast :: type = native "ast";
const bool :: type = native "bool";
const int32 :: type = native "int32";
const int64 :: type = native "int64";
const float64 :: type = native "float64";
const string :: type = native "string";

# TODO panic should return never
const panic :: string -> () = native "panic";
const print :: string -> () = native "print";

const dbg = forall[T] { native "dbg" :: T -> () };

# TODO `:: type` should be inferred
const Option = forall[T :: type] { newtype :Some T | :None };

const Either = forall[.left = left :: type, .right = right :: type] { newtype :Left left | :Right right };

const Result = forall[.ok = ok :: type, .error = error :: type] { newtype :Ok ok | :Error error };

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
