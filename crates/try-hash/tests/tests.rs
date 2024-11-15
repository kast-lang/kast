#![allow(dead_code)]

#[derive(try_hash::TryHash)]
struct FooStruct {
    a: i32,
    b: i64,
}

#[derive(try_hash::TryHash)]
enum FooEnum {
    A,
    B(i32),
    C {
        #[try_hash]
        foo: FooStruct,
        idk: String,
    },
}
