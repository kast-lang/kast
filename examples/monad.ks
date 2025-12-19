use std.prelude.*;

# * -> *
const TypeFn = type ([T :: type] type);
const Apply = [F :: TypeFn, A :: type] (
    F[A]
);
const Id :: TypeFn = [T] T;
let _ :: Apply[Id, String] = _;

# const ForEach = [Iterator :: ([T :: type] type)] newtype (
#     .for_each :: [T] (Iterator[T], T -> ()) -> (),
# );
const Functor = [F :: ([T :: type] type)] type (
    .fmap :: [A, B] (F[A], A -> B) -> F[B],
);

impl Option.t as Functor = (
    .fmap = Option.map,
);

const replace_all = [F :: TypeFn] [A, B] (a :: F[A], b :: B) => (
    (F as Functor).fmap (a, _ => b)
);

let opt :: Option.t[_] = :Some "hello";
let replaced = replace_all[Option.t] (opt, "world");
dbg.print replaced;

const Monad = [M :: TypeFn] newtype (
    .ret :: [T] T -> M[T],
    .bind :: [A, B] (M[A], (A -> M[B])) -> M[B],
);

impl Option.t as Monad = (
    .ret = [T] (x :: T) => :Some x,
    .bind = [A, B] (opt, f) => match opt with (
        | :Some x => f x
        | :None => :None
    ),
);

const @">>" = [M :: TypeFn] [A, B] (a :: M[A], b :: M[B]) -> M[B] => (
    (M as Monad).bind (a, _ => b)
);

@syntax ">>=" 6.5 wrap never = a " " ">>=" " " b ->;
@syntax ">>" 6.5 wrap never = a " " ">>" " " b ->;

impl syntax (a >>= b) = `(
    (Option.t as Monad).bind ($a, $b)
    # TODO (_ as Monad).bind ($a, $b)
);
impl syntax (a >> b) = `(
    @">>" ($a, $b)
);

let opt :: Option.t[Int32] = :Some 1;
let result = opt >>= (x => (:Some 2 >>= (y => :Some (x + y))));
dbg.print result; # prints 3
# const fmap = [F] [T, U] (a :: F[T], f :: T -> U) -> F[U] => (
#     (F as Functor).fmap (a, f)
# );
# const Unwrappable = [U] newtype (
#     .unwrap :: [T] (U[T]) -> T,
# );
# const unwrap = [U] (t :: U[int32]) -> int32 => (
#     (U as Unwrappable).unwrap t
# );
# const Monoid = [Self] newtype (
#     .id :: Self,
#     .op :: (Self, Self) -> Self,
# );
