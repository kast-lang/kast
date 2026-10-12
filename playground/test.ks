for c in "hello" |> String.iter do (
    if c == 'l' then break;
    dbg.print(c);
);
