# “Given enough eyeballs, all bugs are shallow.” ― Linus Torvalds
use std.*;

let rec_scope = rec (
  let f = depth => (
    if depth < 10 then (
      print "inside f";
      dbg depth;
      g (depth + 1);
    );
  );
  let g = depth => (
    print "inside g";
    dbg depth;
    f (depth + 1);
  );
);

rec_scope.f (0 :: int32)

