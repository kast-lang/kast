use kast_ast::*;
use kast_util::*;

fn test_case(source: &str, checker: impl FnOnce(Result<Option<Ast>, Error>)) {
    let syntax = read_syntax(SourceFile {
        contents: include_str!("syntax.ks").to_owned(),
        filename: "syntax.ks".into(),
    })
    .unwrap();
    let source = SourceFile {
        contents: source.to_owned(),
        filename: "<testcase>".into(),
    };
    checker(parse(&syntax, source));
}

enum Checker {
    Simple(String),
    Complex {
        name: String,
        values: Tuple<Checker>,
    },
}

// I didn't even know that rust is so annoying
// TODO: maybe we can somehow get rid of these parentheses in field matchers?
macro_rules! construct_checker {
    ($value:literal) => {
        Checker::Simple($value.to_owned())
    };
    ($name:ident { $($field_name:ident: ($($field_matcher:tt)*),)* }) => {
        Checker::Complex {
            name: stringify!($name).to_owned(),
            values: {
                let mut values = Tuple::empty();
                $(
                    values.add_named(stringify!($field_name).to_owned(), construct_checker!($($field_matcher)*));
                )*
                values
            },
        }
    };
}

fn check_with(checker: Checker) -> impl FnOnce(Result<Option<Ast>, Error>) {
    fn check(ast: Ast, checker: Checker) {
        match (ast, checker) {
            (Ast::Simple { token, data }, Checker::Simple(expected)) => {
                let raw = token.raw();
                if raw != expected {
                    panic!("expected {expected:?}, got {raw:?} at {data}");
                }
            }
            (other, Checker::Simple(expected)) => {
                panic!("expected {expected:?}, got {other} at {}", other.data());
            }
            (
                Ast::Complex {
                    definition,
                    values,
                    data: span,
                },
                Checker::Complex {
                    name: expected_name,
                    values: expected_values,
                },
            ) => {
                if definition.name != expected_name {
                    panic!(
                        "expected {expected_name}, got {got} at {span}",
                        got = definition.name
                    );
                }
                match values.zip(expected_values) {
                    Err(TupleZipError::DifferentUnnamedAmount(actual, expected)) => {
                        panic!("expected {expected} unnamed fields, got {actual} at {span}");
                    }
                    Err(TupleZipError::NamedNotPresentInOther(field_name)) => {
                        panic!(
                            "field {field_name:?} was not expected in {expected_name} at {span}"
                        );
                    }
                    Err(TupleZipError::NamedOnlyPresentInOther(field_name)) => {
                        panic!("field {field_name:?} was expected but not present in {expected_name} at {span}");
                    }
                    Ok(zipped) => {
                        for (actual, expected) in zipped.into_field_values() {
                            check(actual, expected);
                        }
                    }
                }
            }
            (other, Checker::Complex { name, .. }) => {
                panic!("expected {name} node, got {other} at {}", other.data());
            }
        }
    }
    move |result| {
        let ast = result.unwrap().expect("parsed nothing");
        check(ast, checker);
    }
}

macro_rules! check {
    ($($matcher:tt)*) => {{
        let checker = construct_checker!($($matcher)*);
        check_with(checker)
    }};
}

#[test]
fn test_add() {
    test_case(
        "a + b",
        check!(add {
            lhs: ("a"),
            rhs: ("b"),
        }),
    );
}

#[test]
fn test_add_add() {
    test_case(
        "a + b + c",
        check!(add {
            lhs: (add {
                lhs: ("a"),
                rhs: ("b"),
            }),
            rhs: ("c"),
        }),
    );
}

#[test]
fn test_complex_math() {
    test_case(
        "a + sin x * c * d + (1 - 2)",
        check!(add {
            lhs: (add {
                lhs: ("a"),
                rhs: (mul {
                    lhs: (mul {
                        lhs: (call {
                            f: ("sin"),
                            args: ("x"),
                        }),
                        rhs: ("c"),
                    }),
                    rhs: ("d"),
                }),
            }),
            rhs: (scoped {
                e: (sub {
                    lhs: ("1"),
                    rhs: ("2"),
                }),
            }),
        }),
    );
}

#[test]
fn test_very_long_file() {
    let mut source = String::new();
    for _ in 0..1_000 {
        source += "a;"
    }
    test_case(&source, |result| assert!(result.is_ok()));
}
