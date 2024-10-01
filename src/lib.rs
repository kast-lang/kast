#![allow(async_fn_in_trait)]
use eyre::{eyre, Context as _};
use futures::future::BoxFuture;
use futures::prelude::*;
pub use id::*;
use inference::Inferrable;
use ir::*;
pub use kast_ast as ast;
pub use kast_ast::{Ast, Token};
pub use kast_util::*;
use scope::Scope;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
pub use ty::*;
pub use value::*;

mod compiler;
mod id;
mod inference;
mod interpreter;
mod ir;
mod scope;
mod ty;
mod value;

#[derive(Clone)]
pub struct Kast {
    /// Am I a background task? :)
    spawned: bool,
    executor: Arc<async_executor::Executor<'static>>,
    syntax: ast::Syntax,
    compiler: compiler::State,
    pub interpreter: interpreter::State,
}

impl Kast {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            spawned: false,
            executor: Arc::new(async_executor::Executor::new()),
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
