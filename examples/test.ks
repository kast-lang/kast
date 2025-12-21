(# rust:

trait HKT {
    type Output<T>;
}

trait Foo<M: HKT> {
    fn foo<T>() -> M::Output<T>;
}

#)
const HKT = type (
    .Output :: [T] type
);

const Foo = [M :: HKT] type (
    .foo :: [T] () -> M.Output[T]
);

let _ :: Foo[.Output = [T] type T] = _;
# const id :: (
#     [T] T -> T
# ) = (
#     [T] x => x
# );
