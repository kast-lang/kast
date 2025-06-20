//! the name of the programming language is kast
#![allow(clippy::type_complexity, clippy::needless_question_mark)]
#![recursion_limit = "256"]

use async_trait::async_trait;
use eyre::{eyre, Context as _};
use futures::future::BoxFuture;
use futures::prelude::*;
use inference::Inferrable;
pub use kast_ast as ast;
pub use kast_ast::Token;
use kast_derive_macros as derive_macros;
pub use kast_inference as inference;
pub use kast_util::*;
use ordered_float::OrderedFloat;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use try_hash::TryHash;

mod cast;
mod comments;
mod compiler;
mod contexts;
mod id;
mod interpreter;
pub mod ir;
#[cfg(feature = "javascript")]
pub mod javascript;
mod name;
mod place;
mod rusty;
mod scopes;
mod target;
mod ty;
mod value;

use self::cast::*;
pub use self::compiler::{Ast, AstData, Hygiene};
pub use self::contexts::Contexts;
pub use self::id::*;
pub use self::ir::Symbol;
pub use self::ir::*;
pub use self::name::*;
pub use self::place::*;
pub use self::rusty::*;
use self::scopes::*;
pub use self::ty::*;
pub use self::value::*;
use kast_util::executor::Executor;
pub use target::*;

pub enum MaybeBorrowed<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<T: PartialEq> PartialEq for MaybeBorrowed<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        T::eq(self, other)
    }
}

impl<T: PartialOrd> PartialOrd for MaybeBorrowed<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        T::partial_cmp(self, other)
    }
}

impl<T: Ord> Ord for MaybeBorrowed<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        T::cmp(self, other)
    }
}

impl<T: Eq> Eq for MaybeBorrowed<'_, T> {}

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
    current_name: Name,
    cache: Parc<Cache>,
    pub output: std::sync::Arc<dyn Output>,
    pub input: std::sync::Arc<dyn Input>,
    exec_mode: ExecMode,
}

pub trait SubstituteBindings {
    type Target;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target;
}

// TODO merge into impl SubstituteBindings for MaybeNotInferred
pub fn substitute_bindings_inferrable<T: Inferrable + SubstituteBindings<Target = T>>(
    var: inference::MaybeNotInferred<T>,
    kast: &Kast,
    cache: &mut RecurseCache,
) -> inference::MaybeNotInferred<T> {
    let inferred = match var.inferred() {
        Ok(inferred) => inferred,
        Err(_) => {
            return var;
        }
    };
    if let Some(result) = cache.get(var.var()) {
        return result;
    }
    cache.insert(var.var(), var.clone());
    let result: inference::MaybeNotInferred<T> = inferred.substitute_bindings(kast, cache).into();
    cache.insert(var.var(), result.clone());
    result
}

impl<T: SubstituteBindings> SubstituteBindings for Option<T> {
    type Target = Option<T::Target>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        self.map(|value| value.substitute_bindings(kast, cache))
    }
}

impl<T: SubstituteBindings> SubstituteBindings for Box<T> {
    type Target = Box<T::Target>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Box::new(T::substitute_bindings(*self, kast, cache))
    }
}

impl<T: Clone + SubstituteBindings> SubstituteBindings for Parc<T> {
    type Target = Parc<T::Target>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Parc::new(T::substitute_bindings((*self).clone(), kast, cache))
    }
}

impl<T: SubstituteBindings> SubstituteBindings for Tuple<T> {
    type Target = Tuple<T::Target>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        self.map(|value| value.substitute_bindings(kast, cache))
    }
}

impl<T: SubstituteBindings> SubstituteBindings for Vec<T> {
    type Target = Vec<T::Target>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        self.into_iter()
            .map(|value| value.substitute_bindings(kast, cache))
            .collect()
    }
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
    target_symbol: Symbol,
    target_dependent_scopes: Scopes,
}

impl Default for Cache {
    fn default() -> Self {
        let target_symbol = Symbol {
            name: "target".into(),
            span: Span {
                start: Position::ZERO,
                end: Position::ZERO,
                filename: file!().into(),
            },
            id: Id::new(),
        };
        Self {
            start: batbox_time::Instant::now(),
            interpreter: interpreter::Cache::new(),
            compiler: compiler::Cache::new(),
            imports: Default::default(),
            executor: Executor::new(),
            target_dependent_scopes: {
                let scopes = Scopes::new(
                    Id::new(), // TODO what id?
                    ScopeType::NonRecursive,
                    None,
                );
                scopes.compiler.insert(
                    &target_symbol.name,
                    &target_symbol.span,
                    ValueShape::Binding(Parc::new(Binding {
                        symbol: target_symbol.clone(),
                        ty: TypeShape::Target.into(),
                        mutability: Mutability::ReadOnly,
                        compiler_scope: scopes.compiler.clone(),
                    }))
                    .into(),
                );
                scopes
            },
            target_symbol,
        }
    }
}

impl Kast {
    fn from_scratch(path: impl AsRef<Path>, cache: Option<Parc<Cache>>) -> Self {
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
                        s.trim_end_matches("\n").to_owned()
                    }
                }
                DefaultInput
            }),
            exec_mode: ExecMode::Run,
            current_name: Name::new(NamePart::File(path.as_ref().to_owned())),
        }
    }
    fn only_std_syntax(path: impl AsRef<Path>, cache: Option<Parc<Cache>>) -> eyre::Result<Self> {
        let mut kast = Self::from_scratch(path, cache);
        let syntax = kast
            .import_impl(
                std_path().join("syntax.ks"),
                Some("std.syntax"),
                ImportMode::FromScratch,
            )
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
    pub fn new(path: impl AsRef<Path>) -> eyre::Result<Self> {
        Self::new_normal(path, None)
    }
    pub fn new_nostdlib(path: impl AsRef<Path>) -> eyre::Result<Self> {
        Self::only_std_syntax(path, None)
    }
    fn new_normal(path: impl AsRef<Path>, cache: Option<Parc<Cache>>) -> eyre::Result<Self> {
        let mut kast = Self::only_std_syntax(path, cache)?;
        let std_path = std_path().join("lib.ks");
        let std = kast
            .import_impl(&std_path, Some("std"), ImportMode::OnlyStdSyntax)
            .wrap_err("std lib import failed")?;
        kast.add_local(
            kast.new_symbol(
                "std",
                Span {
                    start: Position::ZERO,
                    end: Position::ZERO,
                    filename: std_path,
                },
            ),
            std,
        );
        Ok(kast)
    }

    pub fn import(&self, path: impl AsRef<Path>) -> eyre::Result<Value> {
        self.import_impl(path, None, ImportMode::Normal)
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
                use include_dir::{include_dir, Dir};
                match path.strip_prefix(std_path()) {
                    Ok(path) => {
                        static STD: Dir<'static> = include_dir!("$CARGO_MANIFEST_DIR/../../std");
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

    fn import_impl(
        &self,
        path: impl AsRef<Path>,
        name: Option<&str>,
        mode: ImportMode,
    ) -> eyre::Result<Value> {
        let mut path = path.as_ref().to_owned();
        #[cfg(not(target_arch = "wasm32"))]
        if !path.starts_with(std_path()) {
            path = path.canonicalize()?;
        }
        let path = path;

        let name = name
            .or(path
                .file_name()
                .and_then(|name| name.to_str())
                .and_then(|s| s.strip_suffix(".ks")))
            .unwrap_or("_");

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
            ImportMode::Normal => Self::new_normal(name, Some(self.cache.clone()))?,
            ImportMode::OnlyStdSyntax => Self::only_std_syntax(name, Some(self.cache.clone()))?,
            ImportMode::FromScratch => Self::from_scratch(name, Some(self.cache.clone())),
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

    pub fn parse_ast_file(&self, path: impl AsRef<Path>) -> eyre::Result<Option<Ast>> {
        let source = SourceFile {
            contents: std::fs::read_to_string(path.as_ref())?,
            filename: path.as_ref().into(),
        };
        self.parse_ast(source)
    }

    pub fn parse_ast(&self, source: SourceFile) -> eyre::Result<Option<Ast>> {
        let ast = ast::parse(&self.syntax, source)?;
        Ok(match ast {
            Some(ast) => {
                // sounds like a hack
                let ast = compiler::init_ast(ast);
                Some(ast)
            }
            None => None,
        })
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

    pub fn new_symbol(&self, name: impl Into<String>, span: Span) -> Symbol {
        self.scopes.compiler.new_symbol(name, span)
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
