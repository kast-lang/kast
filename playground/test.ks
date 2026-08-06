const mod = (
    module:

    const foo = [T] (a :: T) => ();
);

const foo = [T] (a :: T) => (
    # mod.foo[_](a);
    mod.foo(a);
);

foo[type ()](());
