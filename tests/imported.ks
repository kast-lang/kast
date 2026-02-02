module:

const Foo = newtype {
    .x :: Int32,
};

impl Foo as module = (
    module:
    
    const init = () => { .x = 0 };
);

impl Foo as FromString = {
    .from_string = s => { .x = parse(s) }
};

use (import "./imported2.ks").*;
