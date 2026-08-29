const Foo = newtype ( :Foo | :Goo );

let foo :: Foo = :Foo;

let result :: Int32 = unwindable block (
    let y :: Int32 = match foo with (
        | :Goo => 67
        | :Foo => unwind block 123
    );
    y
);

dbg.print(result);
