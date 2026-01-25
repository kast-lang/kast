module:

const Into = [T] [Self] newtype {
    .into :: Self -> T
};
impl Int32 as Into[Int64] = {
    .into = value => (
        value |> String.to_string[_] |> String.parse[_]
    )
};
impl Int64 as Into[Int32] = {
    .into = value => (
        value |> String.to_string[_] |> String.parse[_]
    )
};
