const Int32 = @native "Int32";

const GenericType = [T] newtype {
    .field :: T,
};
const generic_fn = [T] (field :: T) -> GenericType[T] => (
    { .field }
);
let x = generic_fn[Int32](123);