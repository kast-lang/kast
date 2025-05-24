use std.*;
use std.collections.*;

let mut map = HashMap.new ();
HashMap.insert (&map, "hello", "world");
HashMap.insert (&map, "second", "2");
#dbg map;
dbg <| HashMap.size &map;
dbg <| HashMap.get (&map, "hello");
dbg <| HashMap.get (&map, "world");

for key :: string, value :: string in HashMap.into_iter map {
    print "iterated";
};
