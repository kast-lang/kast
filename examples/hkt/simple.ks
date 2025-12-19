use std.prelude.*;

# * -> *
const TypeFn = type ([T :: type] type);
const Apply = [F :: TypeFn, A :: type] (
    F[A]
);
const Id :: TypeFn = [T] T;
let _ :: Apply[Id, String] = _;
