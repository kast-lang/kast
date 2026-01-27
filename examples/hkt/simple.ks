use std.prelude.*;

# * -> *
const TypeFn = type ([T :: Type] Type);
const Apply = [F :: TypeFn, A :: Type] (
    F[A]
);
const Id :: TypeFn = [T] T;
let _ :: Apply[Id, String] = _;
