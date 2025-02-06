use std::sync::{atomic::AtomicUsize, Arc};

use super::*;

mod scope;

use scope::*;
pub use scope::{Locals, ScopeType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InterpreterScope(Parc<Scope>);

impl InterpreterScope {
    pub fn insert(&self, symbol: &Symbol, value: Value) {
        self.0.insert(symbol.clone(), value);
    }
    /// TODO dont expose Locals?
    pub fn inspect<R>(&self, f: impl FnOnce(&Locals) -> R) -> R {
        self.0.inspect(f)
    }
    pub fn syntax_definitions(&self) -> &Mutex<Vec<Parc<ast::SyntaxDefinition>>> {
        &self.0.syntax_definitions
    }
    pub fn get(&self, symbol: &Symbol) -> Option<PlaceRef> {
        let (_symbol, value) = self
            .0
            .lookup(Lookup::Id(symbol.id()), None)
            .now_or_never()
            .unwrap()?;
        Some(value)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CompilerScope(Parc<Scope>);

impl CompilerScope {
    pub fn id(&self) -> Id {
        self.0.id
    }
    pub fn insert(&self, name: &str, value: Value) {
        tracing::trace!("inserting {name:?} into {:?}", self.0.id);
        self.0.insert(Symbol::new(name), value);
    }
    pub async fn lookup(&self, name: &str, hygiene: Hygiene, spawn_id: Id) -> Option<Value> {
        tracing::trace!("lookup {name:?} in {:?}", self.0.id);
        match hygiene {
            Hygiene::DefSite => {
                let (_symbol, value) = self.0.lookup(Lookup::Name(name), Some(spawn_id)).await?;
                Some(value.clone_value().unwrap())
            }
        }
    }
}

pub struct Scopes {
    pub id: Id,
    /// we get values for symbols by id
    pub interpreter: InterpreterScope,
    /// we get values by names
    pub compiler: CompilerScope,
    refcount: Arc<AtomicUsize>,
    is_weak: bool,
}

impl Clone for Scopes {
    fn clone(&self) -> Self {
        self.clone_impl(false)
    }
}

impl Scopes {
    fn clone_impl(&self, is_weak: bool) -> Self {
        let is_weak = self.is_weak || is_weak;
        if !is_weak {
            self.refcount
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        }
        Self {
            id: self.id,
            interpreter: self.interpreter.clone(),
            compiler: self.compiler.clone(),
            is_weak,
            refcount: self.refcount.clone(),
        }
    }
    pub fn new(spawn_id: Id, ty: ScopeType, parent: Option<Scopes>) -> Self {
        let (iparent, cparent) = match parent {
            Some(parent) => (
                Some(parent.interpreter.0.clone()),
                Some(parent.compiler.0.clone()),
            ),
            None => (None, None),
        };
        Self {
            id: Id::new(),
            interpreter: InterpreterScope(Parc::new(Scope::new(spawn_id, ty, iparent))),
            compiler: CompilerScope(Parc::new(Scope::new(spawn_id, ty, cparent))),
            is_weak: false,
            refcount: Arc::new(AtomicUsize::new(1)),
        }
    }
    pub fn enter_def_site(&self, def_site: CompilerScope) -> Self {
        tracing::trace!("entering def site: {:?}", def_site.0.id);
        Self {
            id: Id::new(),
            interpreter: self.interpreter.clone(),
            compiler: def_site,
            // compiler: CompilerScope(Parc::new(Scope::new(self.interpreter.0.ty, Some(def_site.0)))),
            is_weak: true,
            refcount: self.refcount.clone(),
        }
    }
    pub fn weak_ref(&self) -> Self {
        self.clone_impl(true)
    }
}

impl Drop for Scopes {
    fn drop(&mut self) {
        if !self.is_weak
            && self
                .refcount
                .fetch_sub(1, std::sync::atomic::Ordering::SeqCst)
                == 1
        {
            self.interpreter.0.close();
            self.compiler.0.close();
        }
    }
}
