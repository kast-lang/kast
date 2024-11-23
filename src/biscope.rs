use super::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BiScope {
    pub id: Id,
    pub evaluation: Parc<Scope>,
    pub lexical: Parc<Scope>,
}

impl BiScope {
    pub fn new(ty: ScopeType, parent: Option<BiScope>) -> Self {
        let (eval_parent, lexical_parent) = match parent {
            Some(parent) => (Some(parent.evaluation), Some(parent.lexical)),
            None => (None, None),
        };
        Self {
            id: Id::new(),
            evaluation: Parc::new(Scope::new(ty, eval_parent)),
            lexical: Parc::new(Scope::new(ty, lexical_parent)),
        }
    }
    fn hygienic_scope(&self, hygiene: Hygiene) -> &Parc<Scope> {
        match hygiene {
            Hygiene::CallSite => &self.evaluation,
            Hygiene::DefSite => &self.lexical,
        }
    }
    pub async fn lookup(
        &self,
        name: &str,
        hygiene: Hygiene,
        do_await: bool,
    ) -> Option<(Symbol, Value)> {
        self.hygienic_scope(hygiene)
            .get_impl(Lookup::Name(name), do_await)
            .await
    }
    pub async fn get(&self, binding: &Binding) -> Option<Value> {
        self.hygienic_scope(binding.hygiene)
            .get_impl(Lookup::Id(binding.symbol.id()), false) // TODO kast.spawned?
            .await
            .map(|(_symbol, value)| value)
    }
    pub fn insert(&self, binding: &Binding, value: Value) {
        self.hygienic_scope(binding.hygiene)
            .insert(binding.symbol.clone(), value);
    }
    pub fn insert_symbol(&self, symbol: Symbol, value: Value) {
        self.evaluation.insert(symbol.clone(), value.clone());
        self.lexical.insert(symbol, value)
    }
    pub fn extend(&self, values: impl IntoIterator<Item = (Parc<Binding>, Value)>) {
        for (binding, value) in values {
            self.insert(&binding, value);
        }
    }
    pub fn close(&self) {
        self.evaluation.close();
        self.lexical.close();
    }
}

impl Kast {
    pub async fn lookup(&self, name: &str, hygiene: Hygiene) -> Option<(Symbol, Value)> {
        self.scope
            .lookup(name, hygiene, self.interpreter.spawned)
            .await
    }
}
