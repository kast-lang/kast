use std.rc.Rc;

module:

const RcStr = newtype {
    .original :: Rc.t[String],
    .offset :: Int32,
    .length :: Int32,
};

impl RcStr as module = (
    module:

    const from_String = (s :: String) -> RcStr => (
        let length = &s |> String.as_str |> String.length;
        let original = Rc.new(s);
        {
            .original,
            .offset = 0,
            .length,
        }
    );

    const length = (self :: &RcStr) -> Int32 => (
        self^.length
    );

    const substring = (self :: &RcStr, offset :: Int32, length :: Int32) -> RcStr => {
        .original = self^.original,
        .offset = self^.offset + offset,
        .length,
    };

    const as_str = (s :: &RcStr) -> &str => (
        &s^.original^
            |> String.as_str
            |> String.substring(s^.offset, s^.length)
    );

    const iter = (self :: &RcStr) -> std.iter.Iterable[Char] => (
        self |> as_str |> String.iter
    );

    const at = (self :: &RcStr, idx :: Int32) -> Char => (
        self |> as_str |> String.at(idx)
    );
);

impl RcStr as std.fmt.Display = {
    .display = (self, fmt) => (
        fmt^.write_str(self |> RcStr.as_str);
    ),
};

impl RcStr as std.cmp.Ord = {
    .compare = (a, b) => std.cmp.default_compare(&(a |> RcStr.as_str), &(b |> RcStr.as_str)),
};

impl type (&RcStr) as std.cmp.Ord = {
    .compare = (&a, &b) => std.cmp.default_compare(&(a |> RcStr.as_str), &(b |> RcStr.as_str)),
};
