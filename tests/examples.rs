use kast::*;
use std::path::Path;

#[allow(dead_code)]
struct Case<'a> {
    name: &'a str,
    input: &'a str,
    expect_output: &'a str,
}

fn test(case: Case<'_>) {
    let mut kast = Kast::new();
    let _value = kast
        .eval_file(Path::new("examples").join(case.name).with_extension("ks"))
        .expect("Failed to run the test");
}

#[test]
fn test_hello_world() {
    test(Case {
        name: "hello",
        input: "",
        expect_output: "Hello\nWorld\n",
    });
}

#[test]
fn test_import_zero() {
    test(Case {
        name: "import-zero",
        input: "",
        expect_output: "0 :: int32\n",
    });
}
