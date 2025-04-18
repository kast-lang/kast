//! the name of the programming language is kast
#![allow(clippy::type_complexity, clippy::needless_question_mark)]

use async_trait::async_trait;
use cast::*;
pub use compiler::{Ast, AstData, Hygiene};
pub use contexts::Contexts;
use executor::Executor;
use eyre::{Context as _, eyre};
use futures::future::BoxFuture;
use futures::prelude::*;
pub use id::*;
use inference::Inferrable;
pub use ir::Symbol;
use ir::*;
pub use kast_ast as ast;
pub use kast_ast::Token;
pub use kast_util::*;
use ordered_float::OrderedFloat;
pub use rusty::*;
use scopes::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use try_hash::TryHash;
pub use ty::*;
pub use value::*;

mod cast;
mod comments;
mod compiler;
mod contexts;
mod executor;
mod id;
pub mod inference;
mod interpreter;
mod ir;
mod rusty;
mod scopes;
mod ty;
mod value;

pub enum MaybeBorrowed<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<T> std::ops::Deref for MaybeBorrowed<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            MaybeBorrowed::Borrowed(value) => value,
            MaybeBorrowed::Owned(value) => value,
        }
    }
}

pub trait Output: 'static + Sync + Send {
    fn write(&self, s: &str);
}

pub trait Input: 'static + Sync + Send {
    fn read_line(&self) -> String;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum ExecMode {
    Run,
    Import,
}

#[derive(Clone)]
pub struct Kast {
    /// Am I a background task? :)
    syntax: Parc<ast::Syntax>,
    pub interpreter: interpreter::State,
    compiler: compiler::State,
    spawn_id: Id,
    scopes: Scopes,
    cache: Parc<Cache>,
    pub output: std::sync::Arc<dyn Output>,
    pub input: std::sync::Arc<dyn Input>,
    exec_mode: ExecMode,
}

pub trait ShowShort {
    fn show_short(&self) -> &'static str;
}

#[derive(Default)]
pub struct RecurseCache {
    cached: anymap3::AnyMap,
}

impl RecurseCache {
    pub fn new() -> Self {
        Self::default()
    }
}

pub trait Identifiable {
    type Id: 'static + Eq + std::hash::Hash;
    fn id(&self) -> Self::Id;
}

impl RecurseCache {
    pub fn insert<K: Identifiable, V: 'static>(&mut self, key: &K, value: V) {
        // 2 + 2 = √(69 / 2) - 1.87367006224
        self.cached
            .entry::<HashMap<K::Id, V>>()
            .or_default()
            .insert(key.id(), value);
    }
    pub fn get<K: Identifiable, V: Clone + 'static>(&self, key: &K) -> Option<V> {
        self.cached
            .get::<HashMap<K::Id, V>>()
            .and_then(|map| map.get(&key.id()))
            .cloned()
    }
}

pub trait SubstituteBindings {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self;
}

enum ImportMode {
    Normal,
    OnlyStdSyntax,
    FromScratch,
}

struct Cache {
    start: batbox_time::Instant,
    executor: Executor,
    interpreter: interpreter::Cache,
    compiler: compiler::Cache,
    imports: Mutex<HashMap<PathBuf, Option<Value>>>,
}

impl Default for Cache {
    fn default() -> Self {
        Self {
            start: batbox_time::Instant::now(),
            interpreter: interpreter::Cache::new(),
            compiler: compiler::Cache::new(),
            imports: Default::default(),
            executor: Executor::new(),
        }
    }
}

impl Kast {
    fn from_scratch(cache: Option<Parc<Cache>>) -> Self {
        let spawn_id = Id::new();
        Self {
            spawn_id,
            syntax: Parc::new(ast::Syntax::empty()),
            scopes: Scopes::new(spawn_id, ScopeType::NonRecursive, None),
            interpreter: interpreter::State::new(),
            compiler: compiler::State::new(),
            cache: cache.unwrap_or_default(),
            output: std::sync::Arc::new({
                struct DefaultOutput;
                impl Output for DefaultOutput {
                    fn write(&self, s: &str) {
                        print!("{s}");
                    }
                }
                DefaultOutput
            }),
            input: std::sync::Arc::new({
                struct DefaultInput;
                impl Input for DefaultInput {
                    fn read_line(&self) -> String {
                        let mut s = String::new();
                        std::io::stdin()
                            .read_line(&mut s)
                            .expect("failed to read line");
                        let s = s.trim_end_matches("\n").to_owned();
                        s
                    }
                }
                DefaultInput
            }),
            exec_mode: ExecMode::Run,
        }
    }
    fn only_std_syntax(cache: Option<Parc<Cache>>) -> eyre::Result<Self> {
        let mut kast = Self::from_scratch(cache);
        let syntax = kast
            .import_impl(std_path().join("syntax.ks"), ImportMode::FromScratch)
            .wrap_err("failed to import std syntax")?
            .into_inferred()?
            .into_syntax_module()
            .wrap_err("std/syntax.ks must evaluate to syntax")?;
        let mut new_syntax: ast::Syntax = (*kast.syntax).clone();
        for definition in &*syntax {
            tracing::trace!("std syntax: {}", definition.name);
            kast.cache.compiler.register_syntax(definition);
            new_syntax
                .insert(definition.clone())
                .wrap_err("Failed to add std syntax")?;
        }
        kast.syntax = Parc::new(new_syntax);
        Ok(kast)
    }
    #[allow(clippy::new_without_default)]
    pub fn new() -> eyre::Result<Self> {
        Self::new_normal(None)
    }
    pub fn new_nostdlib() -> eyre::Result<Self> {
        Self::only_std_syntax(None)
    }
    fn new_normal(cache: Option<Parc<Cache>>) -> eyre::Result<Self> {
        let mut kast = Self::only_std_syntax(cache)?;
        let std = kast
            .import_impl(std_path().join("lib.ks"), ImportMode::OnlyStdSyntax)
            .wrap_err("std lib import failed")?;
        kast.add_local(Symbol::new("std"), std);
        Ok(kast)
    }

    pub fn import(&self, path: impl AsRef<Path>) -> eyre::Result<Value> {
        self.import_impl(path, ImportMode::Normal)
    }

    // I am conviced that even stone plates offer
    // a better DX (developer experience) than rust + cargo
    pub fn include(&self, path: impl AsRef<Path>) -> eyre::Result<Option<Ast>> {
        let mut path = path.as_ref().to_owned();
        #[cfg(not(target_arch = "wasm32"))]
        if !path.starts_with(std_path()) {
            path = path.canonicalize()?;
        }
        let path = path;
        let source = SourceFile {
            #[cfg(feature = "embed-std")]
            contents: {
                use include_dir::{Dir, include_dir};
                match path.strip_prefix(std_path()) {
                    Ok(path) => {
                        static STD: Dir<'static> = include_dir!("$CARGO_MANIFEST_DIR/std");
                        STD.get_file(path)
                            .ok_or_else(|| eyre!("file not exist: {path:?}"))?
                            .contents_utf8()
                            .ok_or_else(|| eyre!("{path:?} is not utf8"))?
                            .to_owned()
                    }
                    Err(_) => std::fs::read_to_string(&path)
                        .wrap_err_with(|| eyre!("failed to read {path:?}"))?,
                }
            },
            #[cfg(not(feature = "embed-std"))]
            contents: std::fs::read_to_string(&path)?,
            filename: path.clone(),
        };
        Ok(ast::parse(&self.syntax, source)?.map(compiler::init_ast))
    }

    fn import_impl(&self, path: impl AsRef<Path>, mode: ImportMode) -> eyre::Result<Value> {
        let mut path = path.as_ref().to_owned();
        #[cfg(not(target_arch = "wasm32"))]
        if !path.starts_with(std_path()) {
            path = path.canonicalize()?;
        }
        let path = path;

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
            ImportMode::Normal => Self::new_normal(Some(self.cache.clone()))?,
            ImportMode::OnlyStdSyntax => Self::only_std_syntax(Some(self.cache.clone()))?,
            ImportMode::FromScratch => Self::from_scratch(Some(self.cache.clone())),
        };
        let source = self.include(&path)?;
        kast.exec_mode = ExecMode::Import;
        let value = futures_lite::future::block_on(kast.eval_ast_opt::<Value>(&source, None))?;
        self.cache
            .imports
            .lock()
            .unwrap()
            .insert(path.clone(), Some(value.clone()));
        tracing::trace!("{path:?} has been imported");
        Ok(value)
    }

    pub fn eval_str_as<T: Rusty>(&mut self, source: &str) -> eyre::Result<T> {
        let value: Value = self.eval_source(
            SourceFile {
                contents: source.to_owned(),
                filename: "<source>".into(),
            },
            T::ty(),
        )?;
        Ok(T::from_value(value)?)
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
        kast.spawn_id = Id::new();
        kast
    }
}

pub fn std_path() -> PathBuf {
    if cfg!(feature = "embed-std") {
        return "/embedded-std/".into();
    }
    match std::env::var_os("KAST_STD") {
        Some(path) => path.into(),
        None => match option_env!("CARGO_MANIFEST_DIR") {
            Some(path) => Path::new(path).join("std"),
            None => panic!("kast standard library not found"),
        },
    }
}
