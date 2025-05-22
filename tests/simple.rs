use kast::*;

fn test_eq(source: &str, expected_value: Value) {
    let source = SourceFile {
        contents: source.to_owned(),
        filename: "<test source>".into(),
    };
    let mut kast = Kast::new("<test>").unwrap();
    let value = kast
        .eval_source::<Value>(source, Some(expected_value.ty()))
        .expect("Failed to eval source");
    assert_eq!(value, expected_value);
}

// cant i write a frontend for LUA instead for kast? I am a LUA pro! 8)
#[test]
fn simple() {
    test_eq(
        "\"hello, world\"",
        ValueShape::String("hello, world".to_owned()).into(),
    );
    test_eq(
        "const int32 = native \"int32\"; 123 :: int32",
        ValueShape::Int32(123).into(),
    );
    test_eq("\"hello\" |> std.dbg", ValueShape::Unit.into());
    test_eq("2 + 2", ValueShape::Int32(4).into());
    test_eq("2 + 2", ValueShape::Int64(4).into());
    test_eq(
        "use std.*; (:Some 123 :: Option[int32]) is :Some _",
        ValueShape::Bool(true).into(),
    );
    test_eq(
        "use std.*; let foo = (a :: int32, b) => a + b; foo (parse \"123\", parse \"456\")",
        ValueShape::Int32(579).into(),
    );
}
