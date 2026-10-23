let s = std.io.stdin.read_to_end();

for c in &s |> as_str |> String.iter do (
    dbg.print(c);
);
