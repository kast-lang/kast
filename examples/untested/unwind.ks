use std.*;

let result = unwindable outer (
  unwindable inner (
    print "hi";
    unwind outer "im out";
    print "this will never be printed";
    "this is not the result"
  )
);
dbg result;
