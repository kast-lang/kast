const Int = @native "Int32";
const Bool = @native "Bool";
const String = @native "String";

impl syntax (@opaque_type native_name) = `(
    (@native "new_opaque_type")($native_name)
);
const RawUnwindToken = @opaque_type "RawUnwindToken";

const UnwindTokenImpl = [T] newtype {
    .raw :: RawUnwindToken,
    .value :: T,
};

const impl_native = [T] (name :: String, value :: T) => (
    (@native "impl_native")(name, value)
);
@eval impl_native("backend.c.UnwindToken", UnwindTokenImpl);

const iterable = [T] type ((T -> ()) -> ());

const Option = [T] newtype (
    | :None
    | :Some T
);

const find = [T] (iter :: iterable[T], f :: T -> Bool) -> Option[T] => (
    unwindable ret (
        iter(x => (
            if f(x) then (
                unwind ret :Some x
            ) else ()
        ));
        :None
    )
);

const range = (n :: Int) -> iterable[Int] => (
    f => f(n)
);

find(range(10), x => (@native "printf(\"%d\\n\", \(x))"; false));

# std.iter.find({ .iter = (1..10).iter }, x => x % 2 == 0);
