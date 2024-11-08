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
#[ignore = "TODO"]
#[should_panic = "test failure to make sure other tests are actually checking output"]
fn test_import_zero_but_expect_1() {
    test(Case {
        name: "import-zero",
        input: "",
        expect_output: "1 :: int32\n",
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

#[test]
#[ignore = "TODO make this not infinite recursion"]
fn test_mutual_recursion() {
    test(Case {
        name: "mutual-recursion",
        input: "",
        expect_output: "",
    });
}

#[test]

fn test_local_syntax() {
    test(Case {
        name: "local-syntax",
        input: "",
        expect_output: "",
    });
}
