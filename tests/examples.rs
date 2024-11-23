use kast::*;
use std::path::Path;

#[allow(dead_code)]
struct Case<'a> {
    name: &'a str,
    comment_lines_starting_with: Option<&'a str>,
    input: &'a str,
    expect_output: &'a str,
}

fn try_test(case: Case<'_>) -> eyre::Result<Value> {
    let mut kast = Kast::new();

    struct CaptureOutput {
        output: std::sync::Arc<std::sync::Mutex<String>>,
    }

    impl Output for CaptureOutput {
        fn write(&self, s: String) {
            *self.output.lock().unwrap() += &s;
        }
    }

    let output = std::sync::Arc::new(std::sync::Mutex::new(String::new()));
    kast.output = std::sync::Arc::new(CaptureOutput {
        output: output.clone(),
    });

    let path = Path::new("examples").join(case.name).with_extension("ks");
    let value = match case.comment_lines_starting_with {
        Some(prefix) => {
            let source = SourceFile {
                contents: {
                    let mut result = String::new();
                    for line in std::fs::read_to_string(&path).unwrap().lines() {
                        if line.trim().starts_with(prefix) {
                            continue;
                        }
                        result += line;
                    }
                    result
                },
                filename: path,
            };
            kast.eval_source(source, None)?
        }
        None => kast.eval_file(path)?,
    };

    let output: String = output.lock().unwrap().clone();
    assert_eq!(case.expect_output, output);

    Ok(value)
}

fn test(case: Case<'_>) {
    try_test(case).expect("Failed to run the test");
}

#[test]
fn test_hello_world() {
    test(Case {
        name: "hello",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "Hello\nWorld\n",
    });
}

#[test]
#[should_panic = "assertion `left == right` failed\n  left: \"1 :: int32\\n\"\n right: \"0 :: int32\\n\""]
fn test_import_zero_but_expect_1() {
    test(Case {
        name: "import-zero",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "1 :: int32\n",
    });
}

#[test]
fn test_default_number_type() {
    test(Case {
        name: "default-number-type",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "123 :: int32\n123 :: int64\n123 :: int32\n123 :: float64\n",
    });
}

#[test]
fn test_import_zero() {
    test(Case {
        name: "import-zero",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "0 :: int32\n",
    });
}

#[test]
fn test_mutual_recursion() {
    test(Case {
        name: "mutual-recursion",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "inside f\n0 :: int32\ninside g\n1 :: int32\ninside f\n2 :: int32\ninside g\n3 :: int32\n",
    });
}

#[test]

fn test_context_shadow() {
    test(Case {
        name: "context-shadow",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "123\nshadow\n",
    });
}

#[test]

fn test_local_syntax() {
    test(Case {
        name: "local-syntax",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "hello\nworld\nworld\nhello\n",
    });
}

#[test]

fn test_macro_hygiene() {
    test(Case {
        name: "macro-hygiene",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "hi\nhello\nhi\nnamed\nnamed\n",
    });
}

#[test]

fn test_unsafe() {
    test(Case {
        name: "unsafe",
        comment_lines_starting_with: None,
        input: "",
        expect_output: "hello\n",
    });
}

#[test]
#[should_panic = "context is not available"]

fn test_unsafe_without_unsafe_context() {
    test(Case {
        name: "unsafe",
        comment_lines_starting_with: Some("with"),
        input: "",
        expect_output: "hello\n",
    });
}

#[test]
fn test_fibonacci() {
    let name = "fibonacci";
    let mut kast = Kast::new();
    let module = kast
        .import(Path::new("examples").join(name).with_extension("ks"))
        .expect("Failed to import the test");
    kast.add_local(kast::Name::new("test"), module);
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
        comment_lines_starting_with: None,
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
