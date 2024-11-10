use kast::*;

fn test_eq(source: &str, expected_value: Value) {
    let source = SourceFile {
        contents: source.to_owned(),
        filename: "<test source>".into(),
    };
    let mut kast = Kast::new();
    let value = kast
        .eval_source(source, Some(expected_value.ty()))
        .expect("Failed to eval source");
    assert_eq!(value, expected_value);
}

#[test]
#[allow(non_snake_case)]
fn special_for_DevNinYa() {
    test_eq("\"hello, world\"", Value::String("hello, world".to_owned()));
    test_eq(
        "const int32 = native \"int32\"; 123 :: int32",
        Value::Int32(123),
    );
    test_eq("\"hello\" |> std.dbg", Value::Unit);
    test_eq("2 + 2", Value::Int32(4));
    test_eq("2 + 2", Value::Int64(4));
}
