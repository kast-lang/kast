use async_trait::async_trait;
use cast::*;
use eyre::{eyre, Context as _};
use futures::future::BoxFuture;
use futures::prelude::*;
pub use id::*;
use inference::Inferrable;
use ir::*;
pub use kast_ast as ast;
pub use kast_ast::{Ast, Token};
pub use kast_util::*;
use ordered_float::OrderedFloat;
use scope::Scope;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use try_hash::TryHash;
pub use ty::*;
pub use value::*;

mod cast;
mod compiler;
mod contexts;
mod id;
pub mod inference;
mod interpreter;
mod ir;
mod scope;
mod ty;
mod value;

#[derive(Clone)]
pub struct Kast {
    /// Am I a background task? :)
    executor: Parc<async_executor::Executor<'static>>,
    syntax: ast::Syntax,
    pub interpreter: interpreter::State,
    cache: Parc<Cache>,
}

pub trait SubstituteBindings {
    fn substitute_bindings(self, kast: &Kast) -> Self;
}

enum ImportMode {
    Normal,
    OnlyStdSyntax,
    FromScratch,
}

struct Cache {
    compiler: compiler::Cache,
    imports: Mutex<HashMap<PathBuf, Option<Value>>>,
}

impl Default for Cache {
    fn default() -> Self {
        Self {
            compiler: compiler::Cache::new(),
            imports: Default::default(),
        }
    }
}

impl Kast {
    fn from_scratch(cache: Option<Parc<Cache>>) -> Self {
        Self {
            executor: Parc::new(async_executor::Executor::new()),
            syntax: ast::Syntax::empty(),
            interpreter: interpreter::State::new(),
            cache: cache.unwrap_or_default(),
        }
    }
    fn only_std_syntax(cache: Option<Parc<Cache>>) -> Self {
        let mut kast = Self::from_scratch(cache);
        let syntax = kast
            .import_impl(std_path().join("syntax.ks"), ImportMode::FromScratch)
            .expect("failed to import std syntax")
            .expect_syntax_module()
            .expect("std/syntax.ks must evaluate to syntax");
        for definition in &*syntax {
            tracing::trace!("std syntax: {}", definition.name);
            kast.cache.compiler.register_syntax(definition);
            kast.syntax
                .insert(definition.clone())
                .expect("Failed to add std syntax");
        }
        kast
    }
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self::new_normal(None)
    }
    fn new_normal(cache: Option<Parc<Cache>>) -> Self {
        let mut kast = Self::only_std_syntax(cache);
        let std = kast
            .import_impl(std_path().join("lib.ks"), ImportMode::OnlyStdSyntax)
            .expect("std lib import failed");
        kast.add_local("std", std);
        kast
    }

    pub fn import(&self, path: impl AsRef<Path>) -> eyre::Result<Value> {
        self.import_impl(path, ImportMode::Normal)
    }

    fn import_impl(&self, path: impl AsRef<Path>, mode: ImportMode) -> eyre::Result<Value> {
        let path = path.as_ref().canonicalize()?;
        tracing::trace!("importing {path:?}");
        if let Some(value) = self.cache.imports.lock().unwrap().get(&path) {
            let value = value.clone().ok_or_else(|| eyre!("recursive imports???"))?;
            return Ok(value);
        }
        tracing::trace!("importing {path:?} for the first time");
        self.cache
            .imports
            .lock()
            .unwrap()
            .insert(path.clone(), None);
        let mut kast = match mode {
            ImportMode::Normal => Self::new_normal(Some(self.cache.clone())),
            ImportMode::OnlyStdSyntax => Self::only_std_syntax(Some(self.cache.clone())),
            ImportMode::FromScratch => Self::from_scratch(Some(self.cache.clone())),
        };
        let source = SourceFile {
            contents: std::fs::read_to_string(&path)?,
            filename: path.clone(),
        };
        let value = kast.eval_source(source, None)?;
        self.cache
            .imports
            .lock()
            .unwrap()
            .insert(path.clone(), Some(value.clone()));
        tracing::trace!("{path:?} has been imported");
        Ok(value)
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
