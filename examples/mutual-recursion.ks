use std.prelude.*;
(
    module:
    let mut depth = 0;
    let f = () => (
        print "inside f";
        depth = depth + 1;
        if depth < 5 then g () else ()
    );
    let g = () => (
        print "inside g";
        f ()
    );
    f ();
)
