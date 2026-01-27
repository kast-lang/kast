let s = "🤔";
for c in String.iter(s) do (
    dbg.print({ .c, .code = Char.code(c) });
);
