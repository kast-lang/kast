const make_module = () => `(
    module:

    const This = @current_scope;

    const t = String;

    const s :: This.t = "Hello";
);

const mod = include_ast make_module();

dbg.print(mod.s);
