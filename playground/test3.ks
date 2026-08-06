const Int32 = @native "Int32";

module:

const Foo = [T] newtype {
    .count :: Int32,
    .value :: T,
};

const update_data = [U] (
    root :: Foo[U],
) -> Foo[U] => {
    .count = 1 :: Int32,
    .value = _ :: U,
};

# join :: [T] Foo[T] -> ()
# join :: [T] Foo[Int32] -> ()
const join = [T] (foo :: Foo[T]) -> () => (
    update_data[T](foo);
);

join[Int32](_);
