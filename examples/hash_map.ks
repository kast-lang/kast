use std.*;

const StringStringMap = HashMap[string, _];

let mut map :: StringStringMap = HashMap_new[_, _] ();
map = HashMap_insert[_, _] (map, "hello", "world");
map = HashMap_insert[_, _] (map, "second", "2");
dbg map;
dbg (HashMap_size[_, _] map);
dbg (HashMap_get[_, _] (map, "hello"));
dbg (HashMap_get[_, _] (map, "world"));

for key :: string, value :: string in HashMap_iter[_, _] map {
    dbg key;
    dbg value;
};
