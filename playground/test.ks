const mod = import "./test2.ks";

let x = ({ Int32, Int32 } as mod.Add).add(1, 2);
dbg.print(x);
