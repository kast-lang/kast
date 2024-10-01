# “Given enough eyeballs, all bugs are shallow.” ― Linus Torvalds
let print = native "print";

let f = rec (
  let f = s => (
    print "inside f";
    print s;
    g s;
  );
  let g = s => (
    print "inside g";
    print s;
    f s
  );
  f
);

f "hello"
