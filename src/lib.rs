use async_trait::async_trait;
use eyre::{eyre, Context as _};
use futures::future::BoxFuture;
use futures::prelude::*;
pub use id::*;
use inference::Inferrable;
use ir::*;
pub use kast_ast as ast;
pub use kast_ast::{Ast, Token};
pub use kast_util::*;
use refmap::RefMap;
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
    executor: Arc<async_executor::Executor<'static>>,
    syntax: ast::Syntax,
    compiler: compiler::State,
    pub interpreter: interpreter::State,
}

impl Kast {
    fn only_std_syntax(
        executor: Option<Arc<async_executor::Executor<'static>>>,
    ) -> eyre::Result<Self> {
        let mut kast = Self {
            executor: executor.unwrap_or_else(|| Arc::new(async_executor::Executor::new())),
            syntax: ast::Syntax::empty(),
            compiler: compiler::State::new(),
            interpreter: interpreter::State::new(),
        };
        fn import(kast: &mut Kast, path: impl AsRef<Path>) -> eyre::Result<Value> {
            let source = SourceFile {
                contents: std::fs::read_to_string(path.as_ref())?,
                filename: path.as_ref().into(),
            };
            kast.eval_source(source, None)
        }
        // TODO only eval once?
        let syntax = import(&mut kast, std_path().join("syntax.ks"))
            .expect("failed to import std syntax")
            .expect_syntax()
            .expect("std/syntax.ks must evaluate to syntax");
        for definition in &*syntax {
            // println!("std syntax: {}", definition.name);
            kast.syntax
                .insert(definition.clone())
                .expect("Failed to add std syntax");
        }
        Ok(kast)
    }
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut kast = Self::only_std_syntax(None).expect("failed to init with only std syntax");
        let std = kast
            .import(std_path().join("lib.ks"))
            .expect("std lib import failed");
        kast.add_local("std", std);
        kast
    }

    pub fn import(&self, path: impl AsRef<Path>) -> eyre::Result<Value> {
        let mut kast = Self::only_std_syntax(Some(self.executor.clone())).unwrap();
        let source = SourceFile {
            contents: std::fs::read_to_string(path.as_ref())?,
            filename: path.as_ref().into(),
        };
        kast.eval_source(source, None)
    }

    pub fn eval_file(&mut self, path: impl AsRef<Path>) -> eyre::Result<Value> {
        let source = SourceFile {
            contents: std::fs::read_to_string(path.as_ref())?,
            filename: path.as_ref().into(),
        };
        self.eval_source(source, None)
    }

    fn spawn_clone(&self) -> Self {
        let mut kast = self.clone();
        kast.interpreter.spawned = true;
        kast
    }

    fn advance_executor(&self) {
        while self.executor.try_tick() {}
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
