#![allow(clippy::type_complexity)]
use eyre::Context;
use kast::*;
use std::{io::Write, path::Path};

#[allow(dead_code)]
struct Case<'a> {
    name: &'a str,
    comment_lines: Option<Box<dyn Fn(&str) -> bool + 'a>>,
    input: &'a str,
    expect_output: &'a str,
}

fn try_test_nodejs(case: Case<'_>) -> eyre::Result<()> {
    let mut kast = Kast::new("<test>").unwrap();
    let path = Path::new("examples").join(case.name).with_extension("ks");
    let ast = match case.comment_lines {
        Some(f) => {
            let source = SourceFile {
                contents: {
                    let mut result = String::new();
                    for line in std::fs::read_to_string(&path).unwrap().lines() {
                        if f(line) {
                            continue;
                        }
                        result += line;
                        result.push('\n');
                    }
                    result
                },
                filename: path,
            };
            kast.parse_ast(source)?
        }
        None => kast.parse_ast_file(path)?,
    };
    let ast = ast.expect("empty test");
    futures_lite::future::block_on(async {
        let ir: Expr = kast.compile(&ast).await.context("Failed to compile test")?;
        let js = kast
            .transpile_to_javascript(
                kast::javascript::JavaScriptEngineType::Node,
                &ValueShape::Expr(Parc::new(ir)).into(),
                javascript::ShowOptions::Pretty,
            )
            .await
            .context("transpiling to js failed")?;

        let js_dir = std::path::Path::new("target").join("test-js");
        std::fs::create_dir_all(&js_dir)?;
        let (mut f, path) = tempfile::NamedTempFile::new_in(js_dir)?.keep()?;
        f.write_all(js.as_bytes())?;
        std::mem::drop(f);

        let mut cmd =
            std::process::Command::new(std::env::var("NODE_EXE").as_deref().unwrap_or("node"));
        cmd.stdin(std::process::Stdio::piped()); // TODO
        cmd.stderr(std::process::Stdio::piped());
        cmd.stdout(std::process::Stdio::piped());
        cmd.arg(path);
        let output = cmd.output().context("Failed to run node")?;
        if !output.status.success() {
            std::io::stdout().write_all(&output.stdout)?;
            eyre::bail!("node exited with failure: {:?}", output.status);
        }
        let output = String::from_utf8(output.stdout).context("non-utf output")?;
        if output != case.expect_output {
            eyre::bail!("Expected output {:?}, got {:?}", case.expect_output, output);
        }
        Ok(())
    })
}

fn try_intepret_test(case: Case<'_>) -> eyre::Result<Value> {
    TestEnv::expect_normal();
    let mut kast = Kast::new("<test>").unwrap();

    struct CaptureOutput {
        output: std::sync::Arc<std::sync::Mutex<String>>,
    }

    impl Output for CaptureOutput {
        fn write(&self, s: &str) {
            *self.output.lock().unwrap() += s;
        }
    }

    let output = std::sync::Arc::new(std::sync::Mutex::new(String::new()));
    kast.output = std::sync::Arc::new(CaptureOutput {
        output: output.clone(),
    });

    let path = Path::new("examples").join(case.name).with_extension("ks");
    let value = match case.comment_lines {
        Some(f) => {
            let source = SourceFile {
                contents: {
                    let mut result = String::new();
                    for line in std::fs::read_to_string(&path).unwrap().lines() {
                        if f(line) {
                            continue;
                        }
                        result += line;
                        result.push('\n');
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

enum TestEnv {
    Normal,
    NodeJs,
}

impl TestEnv {
    fn get() -> Self {
        match std::env::var("TEST_ENV") {
            Ok(env) => match env.as_str() {
                "nodejs" => Self::NodeJs,
                _ => panic!("unknown env: {env}"),
            },
            Err(std::env::VarError::NotPresent) => Self::Normal,
            Err(std::env::VarError::NotUnicode(e)) => panic!("non unicode env: {e:?}"),
        }
    }
    fn expect_normal() {
        match Self::get() {
            Self::Normal => {}
            Self::NodeJs => {
                panic!("supposed to run through node")
            }
        }
    }
}

fn test(case: Case<'_>) {
    match TestEnv::get() {
        TestEnv::Normal => {
            try_intepret_test(case).expect("Failed to run the test");
        }
        TestEnv::NodeJs => {
            try_test_nodejs(case).expect("Failed to run the test");
        }
    }
}

/// file:///./../examples/hello.ks
#[test]
fn test_hello_world() {
    test(Case {
        name: "hello",
        comment_lines: None,
        input: "",
        expect_output: "Hello\nWorld\n",
    });
}

/// file:///./../examples/import-zero.ks
/// file:///./../examples/zero.ks
#[test]
#[should_panic = "assertion `left == right` failed\n  left: \"1 :: int32\\n\"\n right: \"0 :: int32\\n\""]
fn test_import_zero_but_expect_1() {
    test(Case {
        name: "import-zero",
        comment_lines: None,
        input: "",
        expect_output: "1 :: int32\n",
    });
}

/// file:///./../examples/mut.ks
#[test]
fn test_mut() {
    test(Case {
        name: "mut",
        comment_lines: None,
        input: "",
        expect_output: "hello\nworld\n",
    });
}

/// file:///./../examples/default-number-type.ks
#[test]
fn test_default_number_type() {
    test(Case {
        name: "default-number-type",
        comment_lines: None,
        input: "",
        expect_output: "123 :: int32\n123 :: int64\n123 :: int32\n123 :: float64\n",
    });
}

/// file:///./../examples/import-zero.ks
/// file:///./../examples/zero.ks
#[test]
fn test_import_zero() {
    test(Case {
        name: "import-zero",
        comment_lines: None,
        input: "",
        expect_output: "0 :: int32\n",
    });
}

/// file:///./../examples/mutual-recursion.ks
#[test]
fn test_mutual_recursion() {
    test(Case {
        name: "mutual-recursion",
        comment_lines: None,
        input: "",
        expect_output: "inside f\n0 :: int32\ninside g\n1 :: int32\ninside f\n2 :: int32\ninside g\n3 :: int32\n",
    });
}

/// file:///./../examples/context-shadow.ks
// How can I make it not sucking that hard
#[test]

fn test_context_shadow_with_missing_context() {
    let err = try_intepret_test(Case {
        name: "context-shadow",
        comment_lines: None,
        input: "",
        expect_output: "123\nshadow\n456\n",
    })
    .unwrap_err();
    let err = format!("{err:?}"); // debug impl shows the whole chain
    assert!(err.contains("string context not available"));
}

/// file:///./../examples/context-shadow.ks
#[test]
fn test_context_shadow() {
    test(Case {
        name: "context-shadow",
        comment_lines: Some(Box::new(|line| line.contains("should not compile"))),
        input: "",
        expect_output: "123\nshadow\n456\n",
    });
}

/// file:///./../examples/is.ks
#[test]
fn test_is() {
    test(Case {
        name: "is",
        comment_lines: None,
        input: "",
        expect_output: "69 :: int32\nNone\nfalse :: bool\n",
    });
}

/// file:///./../examples/local-syntax.ks
#[test]

fn test_local_syntax() {
    test(Case {
        name: "local-syntax",
        comment_lines: None,
        input: "",
        expect_output: "hello\nworld\nworld\nhello\n",
    });
}

/// file:///./../examples/macro-hygiene.ks
#[test]

fn test_macro_hygiene() {
    test(Case {
        name: "macro-hygiene",
        comment_lines: None,
        input: "",
        expect_output: "hi\nhello\nhi\nnamed\nnamed\n",
    });
}

/// file:///./../examples/ast-nested-scope.ks
#[test]

fn ast_nested_scope() {
    test(Case {
        name: "ast-nested-scope",
        comment_lines: None,
        input: "",
        expect_output: "hi\n",
    });
}

/// file:///./../examples/hash_map.ks
#[test]

fn test_hash_map() {
    test(Case {
        name: "hash_map",
        comment_lines: None,
        input: "",
        expect_output: include_str!("hash_map.output"),
    });
}

/// file:///./../examples/unsafe.ks
#[test]

fn test_unsafe() {
    test(Case {
        name: "unsafe",
        comment_lines: None,
        input: "",
        expect_output: "hello\n",
    });
}

/// file:///./../examples/unsafe.ks
#[test]
#[ignore = "TODO compile checked contexts"]
#[should_panic = "context is not available"]
fn test_unsafe_without_unsafe_context() {
    test(Case {
        name: "unsafe",
        comment_lines: Some(Box::new(|line| line.trim().starts_with("with"))),
        input: "",
        expect_output: "",
    });
}

/// file:///./../examples/fibonacci.ks
#[test]
fn test_fibonacci() {
    TestEnv::expect_normal();
    let name = "fibonacci";
    let mut kast = Kast::new(name).unwrap();
    let module = kast
        .import(Path::new("examples").join(name).with_extension("ks"))
        .expect("Failed to import the test");
    kast.add_local(
        kast.new_symbol(
            "test",
            Span {
                start: Position::ZERO,
                end: Position::ZERO,
                filename: "test_fibonacci".into(),
            },
        ),
        module,
    );
    let mut test_fib = |n: usize, answer: i32| {
        let value = kast
            .eval_source::<Value>(
                SourceFile {
                    contents: format!("test.fib {n}"),
                    filename: "<test>".into(),
                },
                None,
            )
            .unwrap();
        let value = value.into_inferred().unwrap().into_int32().unwrap();
        assert_eq!(answer, value);
    };
    test_fib(1, 1);
    test_fib(5, 8);
    test_fib(10, 89);
}

/// file:///./../examples/variant-types.ks
#[test]
fn test_variant_types() {
    let err = try_intepret_test(Case {
        name: "variant-types",
        comment_lines: None,
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

/// file:///./../examples/embed.rs
/// file:///./../examples/untested
#[test]
fn test_all_examples_are_tested() {
    let this_source = std::fs::read_to_string("tests/examples.rs").unwrap();
    let used_files: std::collections::HashSet<String> = this_source
        .lines()
        .filter_map(|line| line.strip_prefix("/// file:///./../examples/"))
        .map(|s| s.to_owned())
        .collect();
    let mut unused_files = std::collections::BTreeSet::<String>::new();
    for entry in std::fs::read_dir("examples").unwrap() {
        let entry = entry.unwrap();
        let file_name = entry.file_name();
        let file_name = file_name.to_str().unwrap();
        if !used_files.contains(file_name) {
            unused_files.insert(file_name.to_owned());
        }
    }
    if !unused_files.is_empty() {
        panic!("Untested examples: {unused_files:#?}");
    }
}
