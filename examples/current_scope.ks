use std.prelude.*;
const std2 = (
    module:
    const current_scope = @current_scope;
    const foo = () => ();
    current_scope.foo ();
);
std2.foo ();
