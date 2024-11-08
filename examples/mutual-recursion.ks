# “Given enough eyeballs, all bugs are shallow.” ― Linus Torvalds
use std.*;

let rec_scope = rec (
  let f = depth => (
    print "inside f";
    dbg depth;
    g depth;
  );
  let g = depth => (
    print "inside g";
    dbg depth;
    f depth
  );
);

rec_scope.f (0 :: int32)

