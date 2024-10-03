module:

const type = native "type";
const ast :: type = native "ast";
const bool :: type = native "bool";
const int32 :: type = native "int32";
const string :: type = native "string";

const print :: string -> () = native "print";

# const inline_field = macro (name : name) => `($name : $name);
# const inline_typed_field = macro (~name, ~type) => `(
#     $name: ($name :: $type)
# );
