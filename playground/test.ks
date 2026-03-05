const Foo = newtype {
    .a_pos :: Int32,
    .a_uv :: Int64,
};
let x :: Foo = {
    .a_pos = 1,
    .a_uv = 2,
    # .foo = "he",
    # .x = 1,
};
