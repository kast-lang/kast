use std.rc.Rc;

let s = Rc.new(std.io.stdin.read_to_end());
let b = s;
let c = s;

for c in &s^ |> as_str |> String.iter do (
    dbg.print(c);
);

Rc.debug_dump_refs(&s);
