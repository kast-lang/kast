let foo = (...args) => (
    args :: { _, _, _ };
);

foo(1, 2, 3);
