module:

const Into = [T] [Self] newtype (
    .into :: Self -> T
);
impl int32 as Into[int64] = (
    .into = value => (
        value |> String.to_string |> String.parse
    )
);
impl int64 as Into[int32] = (
    .into = value => (
        value |> String.to_string |> String.parse
    )
);
