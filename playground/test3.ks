const Int32 = @native "Int32";

module:

const Foo = [T] newtype {
    .count :: Int32,
    .value :: T,
};

const update_data = [T] (
    root :: Foo[T],
) -> Foo[T] => {
    .count = 1,
    .value = _,
};

# join :: [T] Foo[T] -> ()
# join :: [T] Foo[Int32] -> ()
const join = [T] (foo :: Foo[T]) -> () => (
    update_data(foo);
);

join[Int32](_);
