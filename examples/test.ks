const Int32 = @native "int32";
const zero :: Int32 = 0;
const Foo = (
    let mut x = zero;
    () => (
        x = 1;
        x
    )
);
Foo ();
(@native "console.log") zero
