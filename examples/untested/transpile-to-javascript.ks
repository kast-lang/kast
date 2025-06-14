use std.*;

comptime with default_number_type_based_on_dot;

let console = struct (
    # const log = forall[T] { native "console.log" :: T -> () };
    let log = fn(s :: string) -> () {
        native "console.log" s
    };
);

let g = x => ();
let f = (a, b) => (g a; g b);
let a, b = 12345, 54321;
const main = () => (
    console.log "Hello, world";
    # console.log 123;
);

let js_code = javascript.transpile main;
print "main=";
print &js_code;
print "main()"
