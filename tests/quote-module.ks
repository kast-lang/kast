const make_module = () => `(
    module:

    const s = "Hello";
);

const mod = include_ast make_module();

dbg.print(mod.s);
