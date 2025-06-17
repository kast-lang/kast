use super::*;

/// Partially inferred type, so we know the shape of it
#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub enum TypeShape {
    Unit,
    Bool,
    Int32,
    Int64,
    Float64,
    Char,
    String,
    List(#[try_hash] Type),
    Variant(#[try_hash] VariantType),
    Tuple(#[try_hash] TupleType),
    Function(#[try_hash] Box<FnType>),
    Template(MaybeCompiledFn),
    Macro(#[try_hash] Box<FnType>),
    Multiset,
    Contexts,
    Ast,
    Expr,
    #[allow(clippy::enum_variant_names)]
    Type,
    SyntaxModule,
    SyntaxDefinition,
    UnwindHandle(#[try_hash] Type),
    Binding(Parc<Binding>),
    Symbol,
    HashMap(#[try_hash] HashMapType),
    Ref(#[try_hash] Type),
    NewType {
        id: Id,
        #[try_hash]
        inner: Type,
    },
    Target,
}

impl std::fmt::Debug for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl ShowShort for TypeShape {
    fn show_short(&self) -> &'static str {
        match self {
            TypeShape::Unit => "unit",
            TypeShape::Bool => "bool",
            TypeShape::Int32 => "int32",
            TypeShape::Int64 => "int64",
            TypeShape::Float64 => "float64",
            TypeShape::Char => "char",
            TypeShape::String => "string",
            TypeShape::List(_) => "list",
            TypeShape::Variant(_) => "variant",
            TypeShape::Tuple(_) => "tuple",
            TypeShape::Function(_) => "function",
            TypeShape::Template(_) => "template",
            TypeShape::Macro(_) => "macro",
            TypeShape::Multiset => "multiset",
            TypeShape::Contexts => "contexts",
            TypeShape::Ast => "ast",
            TypeShape::Type => "type",
            TypeShape::SyntaxModule => "syntax module",
            TypeShape::SyntaxDefinition => "syntax def",
            TypeShape::UnwindHandle(_) => "unwind handle",
            TypeShape::Binding(_) => "binding",
            TypeShape::Symbol => "symbol",
            TypeShape::HashMap(_) => "hash_map",
            TypeShape::Ref(_) => "&",
            TypeShape::NewType { .. } => "newtype",
            TypeShape::Expr => "expr",
            TypeShape::Target => "target",
        }
    }
}

pub type Type = inference::MaybeNotInferred<TypeShape>;

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct HashMapType {
    #[try_hash]
    pub key: Type,
    #[try_hash]
    pub value: Type,
}

#[async_trait]
impl Inferrable for HashMapType {
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        let Self { key, value } = self;
        key.await_fully_inferred(cache).await?;
        value.await_fully_inferred(cache).await?;
        Ok(())
    }

    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            key: Inferrable::make_same(a.key, b.key)?,
            value: Inferrable::make_same(a.value, b.value)?,
        })
    }
}

impl std::fmt::Display for HashMapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HashMap[{}, {}]", self.key, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct TupleType {
    #[try_hash]
    pub name: inference::MaybeNotInferred<Option<Name>>,
    #[try_hash]
    pub fields: Tuple<Type>,
}

impl SubstituteBindings for TupleType {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Self {
            name: substitute_bindings_inferrable(self.name, kast, cache),
            fields: self.fields.map(|ty| ty.substitute_bindings(kast, cache)),
        }
    }
}

#[async_trait]
impl Inferrable for TupleType {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            name: Inferrable::make_same(a.name, b.name).context("names are different")?,
            fields: Inferrable::make_same(a.fields, b.fields)?,
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        let Self { name, fields } = self;
        name.await_fully_inferred(cache).await?;
        fields.await_fully_inferred(cache).await?;
        Ok(())
    }
}

impl std::fmt::Display for TupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name.inferred() {
            Ok(None) => self.fields.show("::").fmt(f),
            Ok(Some(name)) => name.fmt(f),
            Err(_) => {
                write!(f, "_ ")?;
                self.fields.show("::").fmt(f)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct VariantType {
    #[try_hash]
    pub name: Name,
    #[try_hash]
    pub variants: Vec<VariantTypeVariant>,
}

#[async_trait]
impl Inferrable for VariantType {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            name: Inferrable::make_same(a.name, b.name)?,
            variants: Inferrable::make_same(a.variants, b.variants)?,
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        let Self { name, variants } = self;
        name.await_fully_inferred(cache).await?;
        variants.await_fully_inferred(cache).await?;
        Ok(())
    }
}

impl SubstituteBindings for VariantType {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> VariantType {
        Self {
            name: self.name.substitute_bindings(kast, cache),
            variants: self
                .variants
                .into_iter()
                .map(|variant| VariantTypeVariant {
                    name: variant.name.clone(),
                    value: variant.value.map(|ty| ty.substitute_bindings(kast, cache)),
                })
                .collect(),
        }
    }
}

impl std::fmt::Display for VariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if true {
            return self.name.fmt(f);
        }
        for (index, variant) in self.variants.iter().enumerate() {
            if index != 0 && !f.alternate() {
                write!(f, " | ")?;
            }
            if f.alternate() {
                write!(f, "| ")?;
            }
            variant.fmt(f)?;
            if f.alternate() {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct VariantTypeVariant {
    pub name: String,
    #[try_hash]
    pub value: Option<Box<Type>>,
}

#[async_trait]
impl Inferrable for VariantTypeVariant {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            name: {
                if a.name != b.name {
                    eyre::bail!("{} != {}", a.name, b.name)
                }
                a.name
            },
            value: Inferrable::make_same(a.value, b.value)?,
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        let Self { name: _, value } = self;
        value.await_fully_inferred(cache).await?;
        Ok(())
    }
}

impl std::fmt::Display for VariantTypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)?;
        if let Some(value) = &self.value {
            write!(f, " {value}")?;
        }
        Ok(())
    }
}

impl TypeShape {
    pub fn expect_hash_map(self) -> Result<HashMapType, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::HashMap(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: "HashMap",
            }),
        }
    }
    pub fn expect_function(self) -> Result<FnType, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Function(ty) => Ok(*ty),
            _ => Err(ExpectError {
                value: self,
                expected: "function",
            }),
        }
    }
    pub fn expect_tuple(self) -> Result<TupleType, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Tuple(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: "tuple",
            }),
        }
    }
    pub fn expect_variant(self) -> Result<VariantType, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Variant(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: "variant",
            }),
        }
    }
    pub fn expect_template(self) -> Result<MaybeCompiledFn, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Template(t) => Ok(t),
            _ => Err(ExpectError {
                value: self,
                expected: "template",
            }),
        }
    }
}

#[async_trait]
impl Inferrable for TypeShape {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        macro_rules! fail {
            () => {
                eyre::bail!("expected type {a}, got {b}")
            };
        }
        Ok(match (a.clone(), b.clone()) {
            (Self::UnwindHandle(a), Self::UnwindHandle(b)) => {
                Self::UnwindHandle(Inferrable::make_same(a, b)?)
            }
            (Self::UnwindHandle(_), _) => fail!(),
            (Self::Unit, Self::Unit) => Self::Unit,
            (Self::Unit, _) => fail!(),
            (Self::Contexts, Self::Contexts) => Self::Contexts,
            (Self::Contexts, _) => fail!(),
            (Self::Bool, Self::Bool) => Self::Bool,
            (Self::Bool, _) => fail!(),
            (Self::Int32, Self::Int32) => Self::Int32,
            (Self::Int32, _) => fail!(),
            (Self::Int64, Self::Int64) => Self::Int64,
            (Self::Int64, _) => fail!(),
            (Self::Float64, Self::Float64) => Self::Float64,
            (Self::Float64, _) => fail!(),
            (Self::Char, Self::Char) => Self::Char,
            (Self::Char, _) => fail!(),
            (Self::String, Self::String) => Self::String,
            (Self::String, _) => fail!(),
            (Self::List(a), Self::List(b)) => Self::List(Inferrable::make_same(a, b)?),
            (Self::List(_), _) => fail!(),
            (Self::Tuple(a), Self::Tuple(b)) => Self::Tuple(Inferrable::make_same(a, b)?),
            (Self::Tuple(_), _) => fail!(),
            (Self::Function(a), Self::Function(b)) => {
                Self::Function(Box::new(Inferrable::make_same(*a, *b)?))
            }
            (Self::Function(_), _) => fail!(),
            (Self::Template(a), Self::Template(b)) if a == b => Self::Template(a),
            (Self::Template(_), _) => fail!(),
            (Self::Macro(a), Self::Macro(b)) => {
                Self::Macro(Box::new(Inferrable::make_same(*a, *b)?))
            }
            (Self::Macro(_), _) => fail!(),
            (Self::Multiset, Self::Multiset) => Self::Multiset,
            (Self::Multiset, _) => fail!(),
            (Self::Ast, Self::Ast) => Self::Ast,
            (Self::Ast, _) => fail!(),
            (Self::Expr, Self::Expr) => Self::Expr,
            (Self::Expr, _) => fail!(),
            (Self::Type, Self::Type) => Self::Type,
            (Self::Type, _) => fail!(),
            (Self::SyntaxModule, Self::SyntaxModule) => Self::SyntaxModule,
            (Self::SyntaxModule, _) => fail!(),
            (Self::SyntaxDefinition, Self::SyntaxDefinition) => Self::SyntaxDefinition,
            (Self::SyntaxDefinition, _) => fail!(),
            (Self::Binding(a), Self::Binding(b)) if a == b => Self::Binding(a),
            (Self::Binding(_), _) => fail!(),
            (Self::Variant(a), Self::Variant(b)) => Self::Variant(Inferrable::make_same(a, b)?),
            (Self::Variant(_), _) => fail!(),
            (Self::Symbol, Self::Symbol) => Self::Symbol,
            (Self::Symbol, _) => fail!(),
            (Self::HashMap(a), Self::HashMap(b)) => Self::HashMap(Inferrable::make_same(a, b)?),
            (Self::HashMap(_), _) => fail!(),
            (Self::Ref(a), Self::Ref(b)) => Self::Ref(Inferrable::make_same(a, b)?),
            (Self::Ref(_), _) => fail!(),
            (
                Self::NewType {
                    id: id_a,
                    inner: inner_a,
                },
                Self::NewType {
                    id: id_b,
                    inner: inner_b,
                },
            ) if id_a == id_b => Self::NewType {
                id: id_a,
                inner: Inferrable::make_same(inner_a, inner_b)?,
            },
            (Self::NewType { .. }, _) => fail!(),
            (Self::Target, Self::Target) => Self::Target,
            (Self::Target, _) => fail!(),
        })
    }

    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        match self {
            TypeShape::Unit
            | TypeShape::Bool
            | TypeShape::Int32
            | TypeShape::Int64
            | TypeShape::Float64
            | TypeShape::Char
            | TypeShape::String
            | TypeShape::Target
            | TypeShape::Multiset
            | TypeShape::Contexts
            | TypeShape::Ast
            | TypeShape::Expr
            | TypeShape::Type
            | TypeShape::SyntaxModule
            | TypeShape::SyntaxDefinition
            | TypeShape::Symbol => {}
            TypeShape::UnwindHandle(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Binding(_) => {} // TODO check
            TypeShape::HashMap(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Ref(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::NewType { id: _, inner } => inner.await_fully_inferred(cache).await?,
            TypeShape::List(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Variant(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Tuple(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Function(ty) => ty.await_fully_inferred(cache).await?,
            TypeShape::Template(_) => {} // TODO
            TypeShape::Macro(ty) => ty.await_fully_inferred(cache).await?,
        }
        Ok(())
    }
}

impl std::fmt::Display for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnwindHandle(ty) => write!(f, "unwindable block handle {ty}"),
            Self::Unit => write!(f, "()"),
            Self::Bool => write!(f, "bool"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Float64 => write!(f, "float64"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),
            Self::List(ty) => write!(f, "list[{ty}]"),
            Self::Tuple(tuple) => tuple.fmt(f),
            Self::Function(ty) => ty.fmt(f),
            Self::Template(_template) => write!(f, "template"),
            Self::Macro(ty) => write!(f, "macro {ty:#}"),
            Self::Multiset => write!(f, "multiset"),
            Self::Ast => write!(f, "ast"),
            Self::Expr => write!(f, "expr"),
            Self::Type => write!(f, "type"),
            Self::Contexts => write!(f, "contexts"),
            Self::SyntaxModule => write!(f, "syntax module"),
            Self::SyntaxDefinition => write!(f, "syntax definition"),
            Self::Binding(binding) => binding.fmt(f),
            Self::Variant(ty) => ty.fmt(f),
            Self::Symbol => write!(f, "symbol"),
            Self::HashMap(map) => map.fmt(f),
            Self::Ref(ty) => write!(f, "&{ty}"),
            Self::NewType { id, inner } => write!(f, "newtype({id}, {inner})"),
            Self::Target => write!(f, "target"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct FnType {
    #[try_hash]
    pub arg: Type,
    #[try_hash]
    pub contexts: Contexts,
    #[try_hash]
    pub result: Type,
}

impl FnType {
    pub fn new_not_inferred() -> Self {
        Self {
            arg: Type::new_not_inferred("fn arg"),
            contexts: Contexts::new_not_inferred(),
            result: Type::new_not_inferred("fn result"),
        }
    }
}

impl std::fmt::Display for FnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.arg, self.result)?;
        if !self.contexts.is_empty() {
            write!(f, " with {}", self.contexts)?;
        }
        Ok(())
    }
}

#[async_trait]
impl Inferrable for FnType {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            arg: Inferrable::make_same(a.arg, b.arg)?,
            contexts: Inferrable::make_same(a.contexts, b.contexts)?,
            result: Inferrable::make_same(a.result, b.result)?,
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        let Self {
            arg,
            contexts,
            result,
        } = self;
        arg.await_fully_inferred(cache).await?;
        contexts.await_fully_inferred(cache).await?;
        result.await_fully_inferred(cache).await?;
        Ok(())
    }
}

impl SubstituteBindings for HashMapType {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            key: self.key.substitute_bindings(kast, cache),
            value: self.value.substitute_bindings(kast, cache),
        }
    }
}

impl SubstituteBindings for TypeShape {
    type Target = inference::MaybeNotInferred<TypeShape>;
    fn substitute_bindings(
        self,
        kast: &Kast,
        cache: &mut RecurseCache,
    ) -> <Self as SubstituteBindings>::Target {
        let result = match self {
            TypeShape::Unit
            | TypeShape::Bool
            | TypeShape::Int32
            | TypeShape::Int64
            | TypeShape::Float64
            | TypeShape::Char
            | TypeShape::String
            | TypeShape::Ast
            | TypeShape::Expr
            | TypeShape::Multiset
            | TypeShape::Contexts
            | TypeShape::Type
            | TypeShape::Symbol
            | TypeShape::Target
            | TypeShape::SyntaxModule
            | TypeShape::SyntaxDefinition => self,
            TypeShape::List(a) => TypeShape::List(a.substitute_bindings(kast, cache)),
            TypeShape::Variant(ty) => TypeShape::Variant(ty.substitute_bindings(kast, cache)),
            TypeShape::UnwindHandle(ty) => {
                TypeShape::UnwindHandle(ty.substitute_bindings(kast, cache))
            }
            TypeShape::Tuple(tuple) => TypeShape::Tuple(tuple.substitute_bindings(kast, cache)),
            TypeShape::Function(f) => {
                TypeShape::Function(Box::new((*f).substitute_bindings(kast, cache)))
            }
            TypeShape::Template(t) => TypeShape::Template(t),
            TypeShape::Macro(f) => {
                TypeShape::Macro(Box::new((*f).substitute_bindings(kast, cache)))
            }
            TypeShape::Binding(ref binding) => match kast.scopes.interpreter.get(&binding.symbol) {
                Some(value) => {
                    return value
                        .clone_value()
                        .unwrap()
                        .inferred()
                        .unwrap()
                        .into_type()
                        .unwrap_or_else(|e| {
                            panic!("{} expected to be a type: {e}", binding.symbol.name())
                        });
                }
                None => self,
            },
            TypeShape::HashMap(map) => TypeShape::HashMap(map.substitute_bindings(kast, cache)),
            TypeShape::Ref(ty) => TypeShape::Ref(ty.substitute_bindings(kast, cache)),
            TypeShape::NewType { .. } => self, // TODO maybe need to sub?
        };
        result.into()
    }
}

impl<T: Inferrable + SubstituteBindings<Target = inference::MaybeNotInferred<T>>> SubstituteBindings
    for inference::MaybeNotInferred<T>
{
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        self.map(
            |inferred, cache| inferred.substitute_bindings(kast, cache),
            cache,
        )
    }
}

impl SubstituteBindings for FnType {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            arg: self.arg.substitute_bindings(kast, cache),
            contexts: self.contexts.substitute_bindings(kast, cache),
            result: self.result.substitute_bindings(kast, cache),
        }
    }
}
