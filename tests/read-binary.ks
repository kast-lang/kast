use std.target.c;

let mut f = c.File.open(std.sys.argv_at(1), "rb");
@defer c.File.close(f);
let data = &mut f |> c.File.read_to_end;
dbg.print(data);
