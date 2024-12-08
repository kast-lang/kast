use std.*;

let mut map = HashMap_new ();
map = HashMap_insert (map, "hello", "world");
map = HashMap_insert (map, "second", "2");
dbg map;
dbg (HashMap_size map);
dbg (HashMap_get (map, "hello"));
dbg (HashMap_get (map, "world"));

for key :: string, value :: string in HashMap_iter map {
    dbg key;
    dbg value;
};
