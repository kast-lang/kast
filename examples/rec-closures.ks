use std;

let fg = rec {
    let f = depth => (
        dbg depth;
        if depth > 0 then (
            print "calling g";
            g (depth - 1);
        );
    );
    let g = depth => (
        dbg depth;
        if depth > 0 then (
            print "calling f";
            f (depth - 1);
        );
    );
};

print "going in";
fg.f(5 :: int32);
