const Mat3 = newtype { Int32 };

const S = newtype {
    .a :: Mat3,
    .b :: Mat3,
};

const Foo = @context S;

while true do (
    with Foo = { .a = { 6 }, .b = { 7 } };
    let a = (@current Foo).a.0;
    let b = (@current Foo).b.0;
    a + b;
);
