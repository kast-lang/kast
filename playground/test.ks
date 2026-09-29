const foo = () => (
    std.fs.read_file("src/transpiler/c/runtime.c");
);

for (_ :: Int32) in 0..1000 do (
    foo();
);

@native "Kast_dump_allocation_stats()";
