use std.*;

const f = forall[T] {
    fn(()) -> Option[T] {
        :None
    }
};
dbg <| f[int32] ();

# const HashMap = std.collections.HashMap;

# const map = HashMap.new();
# dbg <| map
