use super::*;

#[derive(TryHash, PartialEq, Eq, Clone)]
struct Key(#[try_hash] Type);

impl Key {
    fn new(ty: Type) -> eyre::Result<Self> {
        let err = eyre!("context ty: {ty}");
        let key = Self(ty);
        key.try_hash(&mut std::hash::DefaultHasher::new())
            .map_err(|e| eyre!(e))
            .wrap_err(err)?;
        Ok(key)
    }
}

impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.try_hash(state).unwrap()
    }
}

// idk how i ended up with this approach
#[derive(Clone)]
pub struct State {
    runtime_contexts: HashMap<Key, Value>,
    /// Contexts that are going to be brought in using "with" exprs at runtime
    compile_contexts: HashSet<Key>,
}

pub fn default_file_system() -> Value {
    let mut context = Tuple::empty();
    context.add_named(
        "read_file",
        ValueShape::NativeFunction(NativeFunction {
            name: "read_file".into(),
            r#impl: (std::sync::Arc::new(|_kast, _fn_ty, path: Value| {
                async move {
                    let path = path.expect_inferred()?.expect_string()?;
                    let contents = std::fs::read_to_string(path)?;
                    Ok(ValueShape::String(contents).into())
                }
                .boxed()
            }) as std::sync::Arc<NativeFunctionImpl>)
                .into(),
            ty: FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
        })
        .into(),
    );
    ValueShape::Tuple(TupleValue::new(context)).into()
}

pub fn default_number_type() -> Value {
    let mut context = Tuple::empty();
    context.add_named(
        "default_number_type",
        ValueShape::NativeFunction(NativeFunction {
            name: "default_number_type".to_owned(),
            r#impl: (std::sync::Arc::new(|_kast, _fn_ty, s: Value| {
                async move {
                    let _s = s.expect_inferred()?.expect_string()?;
                    Ok(ValueShape::Type(Type::new_not_inferred()).into())
                }
                .boxed()
            }) as std::sync::Arc<NativeFunctionImpl>)
                .into(),
            ty: FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Type.into(),
            },
        })
        .into(),
    );
    ValueShape::Tuple(TupleValue::new(context)).into()
}

pub fn output_context() -> Value {
    let write_type = FnType {
        arg: TypeShape::String.into(),
        contexts: Contexts::empty(),
        result: TypeShape::Unit.into(),
    };
    let mut context_type = Tuple::empty();
    context_type.add_named(
        "write",
        TypeShape::Function(Box::new(write_type.clone())).into(),
    );
    let context_type = TypeShape::Tuple(context_type).into();
    let mut context = Tuple::empty();
    context.add_named(
        "write",
        ValueShape::NativeFunction(NativeFunction {
            name: "print".to_owned(),
            r#impl: (std::sync::Arc::new(|kast: Kast, _fn_ty, s: Value| {
                async move {
                    let s = s.expect_inferred()?.expect_string()?;
                    kast.output.write(s);
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            }) as std::sync::Arc<NativeFunctionImpl>)
                .into(),
            ty: write_type,
        })
        .into(),
    );
    let context: Value = ValueShape::Tuple(TupleValue::new(context)).into();
    assert_eq!(context.ty(), context_type);
    context
}

impl State {
    pub fn empty() -> Self {
        Self {
            runtime_contexts: Default::default(),
            compile_contexts: Default::default(),
        }
    }
    pub fn default() -> Self {
        let mut contexts = Self::empty();
        contexts.insert_runtime(output_context()).unwrap();
        contexts.insert_runtime(default_number_type()).unwrap();
        contexts.insert_runtime(default_file_system()).unwrap();
        contexts
    }
    pub fn insert_compile(&mut self, context_ty: Type) -> eyre::Result<()> {
        tracing::trace!("inserted comptime context: {context_ty}");
        let key = Key::new(context_ty)?;
        // jonathan blow was here
        // i am innocent
        self.compile_contexts.insert(key);
        Ok(())
    }
    pub fn insert_runtime(&mut self, context: Value) -> eyre::Result<()> {
        let key = Key::new(context.ty())?;
        self.compile_contexts.insert(key.clone()); // TODO maybe not?
        self.runtime_contexts.insert(key, context);
        Ok(())
    }
    pub fn get_runtime(&self, ty: Type) -> eyre::Result<Option<Value>> {
        let key = Key::new(ty)?;
        Ok(self.runtime_contexts.get(&key).cloned())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct ContextsData {
    // TODO multiset?
    pub types: std::collections::BTreeSet<Type>,
    pub growable: bool,
}

impl ContextsData {
    pub fn check_available(&self, state: &State) -> eyre::Result<()> {
        // When I wrote this code, only God & I understood what it did. Now... only God knows.
        // Why? Why?! WHY?! OH thats why!
        for ty in &self.types {
            if !state.compile_contexts.contains(&Key::new(ty.clone())?) {
                eyre::bail!("{ty} context is not available");
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Contexts(#[try_hash] inference::MaybeNotInferred<ContextsData>);

impl Contexts {
    pub fn var(&self) -> &inference::Var<ContextsData> {
        self.0.var()
    }
}

impl TryHash for ContextsData {
    type Error = eyre::Report;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        if self.growable {
            eyre::bail!("cant hash growable contexts");
        }
        for ty in &self.types {
            ty.try_hash(hasher)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ContextsData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, ty) in self.types.iter().enumerate() {
            if index != 0 {
                write!(f, " | ")?;
            }
            write!(f, "{ty}")?;
        }
        if self.growable {
            if !self.types.is_empty() {
                write!(f, " | ")?;
            }
            write!(f, "_")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Contexts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Contexts {
    pub fn empty() -> Self {
        Self::from_list([])
    }
    pub fn from_list(list: impl IntoIterator<Item = Type>) -> Self {
        Self(inference::MaybeNotInferred::new_set(ContextsData {
            types: list.into_iter().collect(),
            growable: false,
        }))
    }
    pub fn is_empty(&self) -> bool {
        let data = self.0.inferred().unwrap();
        !data.growable && data.types.is_empty()
    }
    pub fn new_not_inferred() -> Self {
        Self(inference::MaybeNotInferred::new_set(ContextsData {
            types: Default::default(),
            growable: true,
        }))
    }
}

impl ContextsData {
    pub fn extend_with(&mut self, that: &Self) -> eyre::Result<()> {
        // TODO multiple occurences
        for ty in &that.types {
            if !self.types.contains(ty) {
                if !self.growable {
                    eyre::bail!("context not listed: {ty}");
                }
                self.types.insert(ty.clone());
            }
        }
        Ok(())
    }
}

impl SubstituteBindings for ContextsData {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            types: self
                .types
                .clone()
                .into_iter()
                .map(|ty| ty.substitute_bindings(kast, cache))
                .collect(),
            growable: self.growable,
        }
    }
}

impl SubstituteBindings for Contexts {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self(inference::MaybeNotInferred::new_set(
            self.0.inferred().unwrap().substitute_bindings(kast, cache),
        ))
    }
}

impl Inferrable for ContextsData {
    fn make_same(mut a: Self, mut b: Self) -> eyre::Result<Self> {
        #![allow(unused_mut)]
        // a.extend_with(&b)?;
        // b.extend_with(&a)?;
        Ok(Self {
            types: a.types,
            growable: a.growable && b.growable,
        })
    }
}

impl Inferrable for Contexts {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let mut a = a;
        a.0.make_same(b.0)?;
        Ok(a)
    }
}
