use std.*;

const console = struct (
    # const log = forall[T] { native "console.log" :: T -> () };
    const log = fn(s :: string) -> () {
        native "console.log" s
    };
);
const log = console.log;

const g = x => ();
const f = (a, b) => (g a; g b);
const main = () => 
    console.log "Hello, world";
    # console.log 123;

let js_code = javascript.transpile main;
print "main=";
print &js_code;
print "main()"
