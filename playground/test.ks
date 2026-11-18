const Trait = [Self] newtype {
    .foo :: (Int32, Int64) -> Self,
};

impl &str as Trait = {
    .foo = (...) => "hi",
};

(&str as Trait).foo(1, 2);
