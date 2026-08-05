module:

const Into = [T] [Self] newtype {
    .into :: Self -> T
};

# TODO better impls

impl Int32 as Into[Int64] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};
impl Int32 as Into[Float64] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};
impl Int64 as Into[Int32] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};
impl Int64 as Into[Float64] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};
impl Float64 as Into[Int32] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};
impl Float64 as Into[Int64] = {
    .into = value => (
        value |> String.to_string |> String.parse
    )
};

const int32_to_float64 = (value :: Int32) -> Float64 => (
    (Int32 as Into[Float64]).into(value)
);

const float64_to_int32 = (value :: Float64) -> Int32 => (
    (Float64 as Into[Int32]).into(value)
);
