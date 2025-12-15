use std.prelude.*;
#  let some1and2 = :Option[int32].Some (1, 2);
# std::variant
let variant_name = value => match value with (
    | :Some (.a = _, .b = value) => value
    | :None => "None"
    | :Any => "Any"
);
let a :: (:Some _ | :None | :Any) = :None;
print <| variant_name a;
a = :Some (.a = 123, .b = "some_value");
print <| variant_name a;
let b = :Any;
print <| variant_name b;
b = :None;
print <| variant_name b;
b = :Any;
print <| variant_name b;
a = b;
print <| variant_name a;
let f = x :: std.Int32 => :Some x;
