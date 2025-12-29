use std.prelude.*;

for line in String.lines (std.fs.read_file __FILE__) |> std.iter.map (s => s + s) do (
    print line;
);
