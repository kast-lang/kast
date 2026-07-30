module:

impl syntax (@context ty) = `(
    (@native "create_context_type")($ty)
);

const Bool = @native "Bool";
const Int = @native "Int32";
const Float = @native "Float64";
const String = @native "String";

const print_String = (s :: String) => (
    @native "print_String(\(s))";
);

const PanicHandler = @context type (String -> ());

const default_panic_handler = (s :: String) -> () => (
    print_String("PANIC: ");
    print_String(s);
    @native "#include <stdlib.h>";
    @native "exit(-1)";
    @native "#unreachable"
);

const panic = (s :: String) => (
    (@current PanicHandler)(s)
);

const add = [T] (a :: T, b :: T) -> T => (
    @native "\(a) + \(b)"
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
