for c in String.iter("Hello, world") do (
    print(to_string(c));
);

for (i :: Int32) in 0..10 do (
    print(to_string(i));
);

const foo = ArrayList.new[Int32]();

dbg.print(&foo);
