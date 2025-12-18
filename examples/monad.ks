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
const Functor = [F :: ([T :: type] type)] newtype (
    .fmap :: [A, B] (F[A], A -> B) -> F[B],
);
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
