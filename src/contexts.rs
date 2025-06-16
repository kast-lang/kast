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
}

pub fn default_file_system() -> Value {
    let mut context = Tuple::empty();
    context.add_named(
        "read_file",
        ValueShape::NativeFunction(NativeFunction::new(
            Name::new(NamePart::Str("native.read_file".into())),
            "read_file",
            FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |_kast, _fn_ty, path: Value| {
                async move {
                    let path = path.into_inferred()?.as_str()?.to_owned();
                    let contents = std::fs::read_to_string(path)?;
                    Ok(ValueShape::String(contents).into())
                }
                .boxed()
            },
        ))
        .into(),
    );
    let context_ty = TypeShape::Tuple(TupleType {
        name: None.into(),
        fields: context.as_ref().map(|field: &Value| field.ty()),
    })
    .into();
    let context: Value = ValueShape::Tuple(TupleValue::new_unnamed(context, context_ty)).into();
    context
}

pub fn default_number_type() -> Value {
    let mut context = Tuple::empty();
    context.add_named(
        "default_number_type",
        ValueShape::NativeFunction(NativeFunction::new(
            Name::new(NamePart::Str("native.default_number_type".into())),
            "default_number_type",
            FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Type.into(),
            },
            |_kast, _fn_ty, s: Value| {
                async move {
                    let _s = s.into_inferred()?.as_str()?;
                    Ok(ValueShape::Type(Type::new_not_inferred("default number type")).into())
                }
                .boxed()
            },
        ))
        .into(),
    );
    let context_ty = TypeShape::Tuple(TupleType {
        name: None.into(),
        fields: context.as_ref().map(|field: &Value| field.ty()),
    })
    .into();
    let context: Value = ValueShape::Tuple(TupleValue::new_unnamed(context, context_ty)).into();
    context
}

pub fn default_output() -> Value {
    let write_type = FnType {
        arg: TypeShape::Ref(TypeShape::String.into()).into(),
        contexts: Contexts::empty(),
        result: TypeShape::Unit.into(),
    };
    let mut context_type_fields = Tuple::empty();
    context_type_fields.add_named(
        "write",
        TypeShape::Function(Box::new(write_type.clone())).into(),
    );
    let context_type = TypeShape::Tuple(TupleType {
        name: Some(Name::new(NamePart::Str("output".into()))).into(),
        fields: context_type_fields,
    })
    .into();
    let mut context = Tuple::empty();
    context.add_named(
        "write",
        ValueShape::NativeFunction(NativeFunction::new(
            Name::new(NamePart::Str("native.print".into())),
            "print",
            write_type,
            |kast: Kast, _fn_ty, s: Value| {
                async move {
                    let s = s.into_inferred()?;
                    let s = s.into_ref()?;
                    let s = s.read_value()?;
                    let s = s.as_inferred()?;
                    let s = s.as_str()?;
                    kast.output.write(s);
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            },
        ))
        .into(),
    );
    let context: Value = ValueShape::Tuple(TupleValue::new_unnamed(context, context_type)).into();
    context
}

pub fn default_input() -> Value {
    let mut context = Tuple::empty();
    context.add_named(
        "read_line",
        ValueShape::NativeFunction(NativeFunction::new(
            Name::new(NamePart::Str("native.read_line".into())),
            "read_line",
            FnType {
                arg: TypeShape::Unit.into(),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |kast: Kast, _fn_ty, _arg: Value| {
                async move {
                    let s = kast.input.read_line();
                    Ok(ValueShape::String(s).into())
                }
                .boxed()
            },
        ))
        .into(),
    );
    let context_ty = TypeShape::Tuple(TupleType {
        name: None.into(),
        fields: context.as_ref().map(|field: &Value| field.ty()),
    })
    .into();
    let context: Value = ValueShape::Tuple(TupleValue::new_unnamed(context, context_ty)).into();
    context
}

impl State {
    pub fn empty() -> Self {
        Self {
            runtime_contexts: Default::default(),
        }
    }
    pub fn default() -> Self {
        let mut contexts = Self::empty();
        contexts.insert_runtime(default_output()).unwrap();
        contexts.insert_runtime(default_input()).unwrap();
        contexts.insert_runtime(default_number_type()).unwrap();
        contexts.insert_runtime(default_file_system()).unwrap();
        contexts
    }
    pub fn insert_runtime(&mut self, context: Value) -> eyre::Result<()> {
        let key = Key::new(context.ty())?;
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

#[derive(Debug, Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Contexts(#[try_hash] pub inference::MaybeNotInferred<ContextsData>);

impl Contexts {
    pub fn var(&self) -> &inference::Var<ContextsData> {
        self.0.var()
    }
    pub fn single(kast: &Kast, ty: Type) -> eyre::Result<Self> {
        let this = inference::MaybeNotInferred::new_set(ContextsData {
            types: Default::default(),
            growable: true,
        });
        let also_this = this.clone();
        kast.cache.executor.spawn(async move {
            ty.await_fully_inferred(&mut RecurseCache::new()).await?;
            let mut current = also_this.inferred().unwrap();
            current.extend_with_types([&ty])?;
            also_this.var().set(current)?;
            Ok(())
        });
        Ok(Self(this))
    }
    pub fn remove_injected(contexts: &Self, ty: &Type) -> eyre::Result<Self> {
        let this = inference::MaybeNotInferred::new_set(ContextsData {
            types: Default::default(),
            growable: true,
        });
        let update = {
            let contexts = contexts.clone();
            let ty = ty.clone();
            let this = this.clone();
            move || -> eyre::Result<inference::CheckResult> {
                // TODO fully_inferred?
                if ty.inferred().is_err() {
                    return Ok(inference::CheckResult::RunAgain);
                };
                let Ok(mut contexts) = contexts.0.inferred() else {
                    return Ok(inference::CheckResult::RunAgain);
                };
                let _was_removed = contexts.types.remove(&ty);
                this.var().set(contexts)?;
                Ok(inference::CheckResult::RunAgain)
            }
        };
        contexts.var().add_check({
            let update = update.clone();
            move |_| update()
        })?;
        ty.var().add_check(move |_| update())?;
        Ok(Self(this))
    }
    pub fn merge<'a>(iter: impl IntoIterator<Item = &'a Self>) -> eyre::Result<Self> {
        let this = inference::MaybeNotInferred::new_set(ContextsData {
            types: Default::default(),
            growable: true,
        });
        let update = {
            let this = this.clone();
            move |updated: &ContextsData| -> eyre::Result<inference::CheckResult> {
                let mut current = this.inferred().unwrap();
                current.extend_with(updated)?;
                this.var().set(current)?;
                Ok(inference::CheckResult::RunAgain)
            }
        };
        for elem in iter {
            elem.var().add_check(update.clone())?;
        }
        Ok(Self(this))
    }
    pub fn merge_two(a: &Self, b: &Self) -> eyre::Result<Self> {
        Self::merge([a, b])
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
        Self::from_list([], false)
    }
    pub fn empty_growable() -> Self {
        Self::from_list([], true)
    }
    pub fn from_list(list: impl IntoIterator<Item = Type>, growable: bool) -> Self {
        Self(inference::MaybeNotInferred::new_set(ContextsData {
            types: list.into_iter().collect(),
            growable,
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
    pub fn extend_with_types<'a>(
        &mut self,
        types: impl IntoIterator<Item = &'a Type>,
    ) -> eyre::Result<()> {
        // TODO multiple occurences
        for ty in types {
            if !catch_panic(|| self.types.contains(ty))? {
                if !self.growable {
                    eyre::bail!("context not listed: {ty}");
                }
                self.types.insert(ty.clone());
            }
        }
        Ok(())
    }
    pub fn extend_with(&mut self, that: &Self) -> eyre::Result<()> {
        self.extend_with_types(that.types.iter())
    }
}

impl SubstituteBindings for ContextsData {
    type Target = Self;
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
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self(inference::MaybeNotInferred::new_set(
            self.0.inferred().unwrap().substitute_bindings(kast, cache),
        ))
    }
}

#[async_trait]
impl Inferrable for ContextsData {
    fn make_same(mut a: Self, mut b: Self) -> eyre::Result<Self> {
        #![allow(unused_mut)]
        a.extend_with(&b)?;
        b.extend_with(&a)?;
        Ok(Self {
            types: a.types,
            growable: a.growable && b.growable,
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        for r#type in &self.types {
            r#type.await_fully_inferred(cache).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl Inferrable for Contexts {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let mut a = a;
        a.0.make_same(b.0)?;
        Ok(a)
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        self.0.await_fully_inferred(cache).await
    }
}
