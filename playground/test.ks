const Int32 = @native "Int32";
const Int64 = @native "Int64";
const String = @native "String";

const Foo = (
    module:

    const Result = [T, U] newtype {
        .f :: T -> U,
    };

    const apply = [T, U] (f :: T -> U) -> Result[T, U] => {
        .f = arg => apply[T, U](f).f(arg),
    };
);

let result = Foo.apply(x => x).f(123 :: Int32);
let result = Foo.apply(x => x).f(123 :: Int64);
@native "console.log(\(result))";
