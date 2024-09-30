use eyre::{eyre, Context as _};
pub use kast_ast as ast;
pub use kast_util::*;
use std::path::{Path, PathBuf};
use std::sync::Arc;

mod compiler;
mod id;
mod inference;
mod interpreter;
mod ir;
mod ty;
mod value;

pub use id::*;
use inference::Inferrable;
use ir::*;
pub use kast_ast::{Ast, Token};
pub use ty::*;
pub use value::*;

pub struct Kast {
    syntax: ast::Syntax,
    compiler: compiler::State,
    pub interpreter: interpreter::State,
}

impl Kast {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            syntax: ast::read_syntax(SourceFile {
                contents: std::fs::read_to_string(std_path().join("syntax.ks")).unwrap(),
                filename: "std/syntax.ks".into(),
            })
            .expect("failed to parse std syntax"),
            compiler: compiler::State::new(),
            interpreter: interpreter::State::new(),
        }
    }
}

pub fn std_path() -> PathBuf {
    match std::env::var_os("KAST_STD") {
        Some(path) => path.into(),
        None => match option_env!("CARGO_MANIFEST_DIR") {
            Some(path) => Path::new(path).join("std"),
            None => panic!("kast standard library not found"),
        },
    }
}
