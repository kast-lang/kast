use kast::*;
use std::path::Path;

#[allow(dead_code)]
struct Case<'a> {
    name: &'a str,
    input: &'a str,
    expect_output: &'a str,
}

fn try_test(case: Case<'_>) -> eyre::Result<Value> {
    let mut kast = Kast::new();
    kast.eval_file(Path::new("examples").join(case.name).with_extension("ks"))
}

fn test(case: Case<'_>) {
    try_test(case).expect("Failed to run the test");
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
fn test_mutual_recursion() {
    test(Case {
        name: "mutual-recursion",
        input: "",
        expect_output: "",
    });
}

#[test]

fn test_context_shadow() {
    test(Case {
        name: "context-shadow",
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

#[test]
fn test_fibonacci() {
    let name = "fibonacci";
    let mut kast = Kast::new();
    let module = kast
        .import(Path::new("examples").join(name).with_extension("ks"))
        .expect("Failed to import the test");
    kast.interpreter.insert_local("test", module);
    let mut test_fib = |n: usize, answer: i32| {
        let value = kast
            .eval_source(
                SourceFile {
                    contents: format!("test.fib {n}"),
                    filename: "<test>".into(),
                },
                None,
            )
            .unwrap();
        let value = value.expect_int32().unwrap();
        assert_eq!(answer, value);
    };
    test_fib(1, 1);
    test_fib(5, 8);
    test_fib(10, 89);
}

#[test]
fn test_variant_types() {
    let err = try_test(Case {
        name: "variant-types",
        input: "",
        expect_output: "",
    })
    .unwrap_err();
    let err = format!("{err:?}"); // debug impl shows the whole chain
    assert!(
        err.contains("this is going to panic"),
        "this example does unwrap of .Error"
    );
}
