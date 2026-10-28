const foo = () => (
    panic("hi");
);

for (i :: Int32) in 0..10 do (
    foo();
);
