const f = async () -> Int32 => (
    print("Hello, world!");
    123
);

const g = () => (
    f();
);

g();
