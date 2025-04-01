#![allow(dead_code)]

use kast_try_hash as try_hash;

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
