module:

const Into = [T] [Self] newtype {
    .into :: Self -> T
};

# TODO better impls

const do_impl = (from, into) => `(
    impl from as Into[into] = {
        .into = value => (
            let s = value |> String.to_string;
            &s |> String.as_str |> String.parse
        ),
    };
);

include_ast do_impl(Int32, Int64);
include_ast do_impl(Int32, Float64);
include_ast do_impl(Int64, Int32);
include_ast do_impl(Int64, Float64);
include_ast do_impl(Float64, Int32);
include_ast do_impl(Float64, Int64);

const int32_to_float64 = (value :: Int32) -> Float64 => (
    (Int32 as Into[Float64]).into(value)
);

const float64_to_int32 = (value :: Float64) -> Int32 => (
    (Float64 as Into[Int32]).into(value)
);
