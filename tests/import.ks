module:

const imported = import "./imported.ks";
let foo :: imported.Foo = parse("123");
dbg.print(foo);
