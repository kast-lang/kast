use std.prelude.*;
const std2 = (
    module:
    const foo = () => ();
    const current_scope = @current_scope;
    current_scope.foo ();
);
std2.foo ();
