use std.prelude.*;

(# rust:
trait HKT {
    type Output<T>;
}

trait Foo<M: HKT> {
    fn foo<T>() -> M::Output<T>;
}

#)
(# kast 1-1
const HKT = type (
    .Output :: [T] type
);
const Foo = [M :: HKT] type (
    .foo :: [T] () -> M.Output[T]
);
#)
(# 
imaginary Rust

trait Foo<M> {
    fn foo<T> () -> M<T>;
}
 #)
(# 
 real haskell

type Foo (m :: * -> *) = forall t. () -> m t

x :: Foo
  #)
(# Idris

 Foo = \m : (Type -> Type) => {T : Type} -> () -> m T

 #)
const Monad = [M :: [type] -> type] newtype (
    .ret :: [T] T -> M[T],
);

# (A : _) -> (B : _) -> A B
# (C : _) -> (D : _) -> C D

# (_0 : _)
# [U] -> U
# [_0] -> _0

const Treap = (
    module:
    
    const data = [T] newtype (
        .value :: T,
    );
    const t = [T] newtype (
        | :Empty
        | :Node T
    );
    const singleton = [T] (value :: T) -> t[T] => (
        :Node (
            value
        )
    );
);

impl Treap.t as Monad = (
    .ret = Treap.singleton,
);
# const id :: ( 
#     [T] T -> T
# ) = (
#     [T] x => x
# );
