const Type :: type = @native "type Type";
const Int :: Type = @native "type Int";
const String :: Type = @native "type String";
const Ast :: Type = @native "type Ast";

const add :: (Int, Int) -> Int = @native "+";
const sub :: (Int, Int) -> Int = @native "-";
const mul :: (Int, Int) -> Int = @native "*";
const div :: (Int, Int) -> Int = @native "/";

impl syntax (\a + \b) = `(add(\a, \b));
impl syntax (\a - \b) = `(sub(\a, \b));
impl syntax (\a * \b) = `(mul(\a, \b));
impl syntax (\a / \b) = `(div(\a, \b));

const print :: String -> () = @native "print";
