use std.*;

let mut map = HashMap_new ();
HashMap_insert (&map, "hello", "world");
HashMap_insert (&map, "second", "2");
#dbg map;
dbg (HashMap_size &map);
dbg (HashMap_get (&map, "hello"));
dbg (HashMap_get (&map, "world"));

for key :: string, value :: string in HashMap_into_iter map {
    print "iterated";
};
