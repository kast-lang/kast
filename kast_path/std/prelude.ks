const Type :: type = @native "type Type";
const Bool :: Type = @native "type Bool";
const Int :: Type = @native "type Int";
const String :: Type = @native "type String";
const Ast :: Type = @native "type Ast";

impl syntax (true) = `(@native "true" :: Bool);
impl syntax (false) = `(@native "false" :: Bool);

const add :: (Int, Int) -> Int = @native "+";
const sub :: (Int, Int) -> Int = @native "-";
const mul :: (Int, Int) -> Int = @native "*";
const div :: (Int, Int) -> Int = @native "/";

impl syntax (\a + \b) = `(add(\a, \b));
impl syntax (\a - \b) = `(sub(\a, \b));
impl syntax (\a * \b) = `(mul(\a, \b));
impl syntax (\a / \b) = `(div(\a, \b));

const print :: String -> () = @native "print";
