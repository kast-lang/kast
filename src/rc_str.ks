use std.rc.Rc;

module:

const RcStr = newtype {
    .original :: Rc.t[String],
    .s :: &str,
};

impl RcStr as module = (
    module:

    const allocate = (s :: &str) -> RcStr => (
        from_String(String.from_str(s))
    );

    const from_String = (s :: String) -> RcStr => (
        let length = &s |> String.as_str |> String.length;
        let original = Rc.new(s);
        let s = &original^ |> String.as_str;
        {
            .original,
            .s,
        }
    );

    const length = (self :: &RcStr) -> Int32 => (
        String.length(self^.s)
    );

    const substring = (self :: &RcStr, offset :: Int32, length :: Int32) -> RcStr => {
        .original = self^.original,
        .s = self^.s |> String.substring(offset, length),
    };

    const as_str = (self :: &RcStr) -> &str => (
        self^.s
    );

    const iter = (self :: &RcStr) -> std.iter.Iterable[Char] => (
        self^.s |> String.iter
    );

    const at = (self :: &RcStr, idx :: Int32) -> Char => (
        self^.s |> String.at(idx)
    );

    const lines = (self :: &RcStr) -> std.iter.Iterable[RcStr] => {
        .iter = @move f => (
            for line in String.lines(self^.s) do (
                let line = {
                    .original = self^.original,
                    .s = line,
                };
                f(line);
            );
        ),
    };
);

impl RcStr as std.fmt.Display = {
    .display = (self, fmt) => (
        fmt^.write_str(self^.s);
    ),
};

impl RcStr as std.cmp.Ord = {
    .compare = (a, b) => std.cmp.default_compare(&a^.s, &b^.s),
};

impl type (&RcStr) as std.cmp.Ord = {
    .compare = (&a, &b) => std.cmp.default_compare(&a^.s, &b^.s),
};
