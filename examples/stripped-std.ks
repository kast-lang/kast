module:

impl syntax (@context ty) = `(
    (@native "create_context_type")($ty)
);

impl syntax (arg |> f) = `(
    $f($arg)
);

const Bool = @native "Bool";
const Int = @native "Int32";
const Float = @native "Float64";
const String = @native "String";

const Never = newtype @empty_variant;

impl syntax (@opaque_type native_name) = `(
    (@native "new_opaque_type")($native_name)
);

const impl_native = [T] (name :: String, value :: T) => (
    (@native "impl_native")(name, value)
);

const RawUnwindToken = @opaque_type "RawUnwindToken";

const UnwindTokenImpl = [T] newtype {
    .raw :: RawUnwindToken,
    .value :: T,
};

@eval impl_native("backend.c.UnwindToken", UnwindTokenImpl);

const from_never = [T] (_ :: Never) -> T => (
    @native "#unreachable"
);

const print_String = (s :: String) => (
    @native "print_String(\(s))";
);

const PanicHandler = @context type (String -> Never);

const default_panic_handler = (s :: String) -> Never => (
    print_String("PANIC: ");
    print_String(s);
    @native "#include <stdlib.h>";
    @native "exit(-1)";
    @native "#unreachable"
);

const panic = (s :: String) -> Never => (
    (@current PanicHandler)(s)
);

const Add = [Self] newtype {
    .add :: (Self, Self) -> Self,
};

impl Int as Add = {
    .add = (a, b) => @native "\(a) + \(b)",
};

const add = [T] (a :: T, b :: T) -> T => (
    (T as Add).add(a, b)
);

const sub = [T] (a :: T, b :: T) -> T => (
    @native "\(a) - \(b)"
);

const less_than = [T] (a :: T, b :: T) -> Bool => (
    @native "\(a) < \(b)"
);

const print_Int = (x :: Int) => (
    @native "#include <stdio.h>";
    @native "printf(\"%d\", \(x))";
);

const print_Float = (x :: Float) => (
    @native "#include <stdio.h>";
    @native "printf(\"%lf\", \(x))";
);

const Option = (
    module:
    
    const t = [T] newtype (
        | :None
        | :Some T
    );
);

impl syntax (a < b) = `(less_than($a, $b));
impl syntax (a + b) = `(add($a, $b));
impl syntax (a - b) = `(sub($a, $b));
