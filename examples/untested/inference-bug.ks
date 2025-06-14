use std.*;

const test = fn(T :: type) {
    ()
};

const test2 = fn(T :: type) {
    test(T);
    comptime dbg T;
    test(T, T);
    comptime print "hi";
    comptime dbg T;
};

comptime dbg test;
comptime dbg test2;
#test2 ();
