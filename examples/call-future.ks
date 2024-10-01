# “Given enough eyeballs, all bugs are shallow.” ― Linus Torvalds
const int32 = native "int32";
const string = native "string";
let print = native "print";

let f = rec (
  let f = s => (
    print "inside f";
    print s;
    g s;
  );
  let g = (s :: string) => (
    print "inside g";
    print s;
    f s
  );
  f
);

f "hello"
