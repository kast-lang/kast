use std.*;

let test = (value :: Option[int32]) => (
    if value is :Some nice_value then
        dbg nice_value
    else
        print "None"
);

test :Some 69;
test :None;

let none_is_some = :None is :Some x;
dbg none_is_some;
