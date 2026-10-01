const Foo = newtype {
    .a :: Box[Int32],
    .b :: Box[String],
};

let { ... } :: Foo = { .a = Box_new(123), .b = Box_new(String.from_str("hi")) };
