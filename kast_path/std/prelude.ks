const Type :: type = @native "type Type";
const Bool :: Type = @native "type Bool";
const Int :: Type = @native "type Int";
const String :: Type = @native "type String";
const Ast :: Type = @native "type Ast";

impl syntax (true) = `(@native "true" :: Bool);
impl syntax (false) = `(@native "false" :: Bool);

const op = (
    module:

    const add :: (Int, Int) -> Int = @native "+";
    const sub :: (Int, Int) -> Int = @native "-";
    const mul :: (Int, Int) -> Int = @native "*";
    const div :: (Int, Int) -> Int = @native "/";
);

impl syntax ($a + $b) = `(op.add($a, $b));
impl syntax ($a - $b) = `(op.sub($a, $b));
impl syntax ($a * $b) = `(op.mul($a, $b));
impl syntax ($a / $b) = `(op.div($a, $b));

const cmp = (
    module:

    const less :: (Int, Int) -> Int = @native "<";
    const less_or_equal :: (Int, Int) -> Int = @native "<=";
    const equal :: (Int, Int) -> Int = @native "==";
    const not_equal :: (Int, Int) -> Int = @native "!=";
    const greater_or_equal :: (Int, Int) -> Int = @native ">=";
    const greater :: (Int, Int) -> Int = @native ">";
);

impl syntax ($a < $b) = `(cmp.less($a, $b));
impl syntax ($a <= $b) = `(cmp.less_or_equal($a, $b));
impl syntax ($a == $b) = `(cmp.equal($a, $b));
impl syntax ($a != $b) = `(cmp.not_equal($a, $b));
impl syntax ($a >= $b) = `(cmp.greater_or_equal($a, $b));
impl syntax ($a > $b) = `(cmp.greater($a, $b));

const io = (
    module:

    const print :: String -> () = @native "print";
);

const print = io.print;

const debug = (
    module:

    const print :: Int -> () = @native "debug.print";
);
