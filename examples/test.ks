use std.prelude.*;

for i in 0..std.sys.argc () do (
    dbg.print (std.sys.argv_at i);
);
