module:

const @"syntax" = import "./syntax.ks";

const type = native "type";
const ast :: type = native "ast";
const bool :: type = native "bool";
const int32 :: type = native "int32";
const int64 :: type = native "int64";
const string :: type = native "string";

const print :: string -> () = native "print";

const dbg = forall[T] { native "dbg" :: T -> () };

impl syntax @"syntax".@"op binary +" = forall[T] { native "+" :: (lhs: T, rhs: T) -> T };
impl syntax @"syntax".@"op binary -" = forall[T] { native "-" :: (lhs: T, rhs: T) -> T };
impl syntax @"syntax".@"op binary <" = forall[T] { native "<" :: (lhs: T, rhs: T) -> bool };
