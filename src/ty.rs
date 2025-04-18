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
    Variant(#[try_hash] Vec<VariantType>),
    Tuple(#[try_hash] Tuple<Type>),
    Function(#[try_hash] Box<FnType>),
    Template(MaybeCompiledFn),
    Macro(#[try_hash] Box<FnType>),
    Multiset,
    Contexts,
    Ast,
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

impl std::fmt::Display for HashMapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HashMap[{}, {}]", self.key, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct VariantType {
    pub name: String,
    #[try_hash]
    pub value: Option<Box<Type>>,
}

impl std::fmt::Display for VariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)?;
        if let Some(value) = &self.value {
            write!(f, " {value}")?;
        }
        Ok(())
    }
}

impl TypeShape {
    pub fn expect_function(self) -> Result<FnType, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Function(ty) => Ok(*ty),
            _ => Err(ExpectError {
                value: self,
                expected: "function",
            }),
        }
    }
    pub fn expect_tuple(self) -> Result<Tuple<Type>, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(ExpectError {
                value: self,
                expected: "tuple",
            }),
        }
    }
    pub fn expect_variant(self) -> Result<Vec<VariantType>, ExpectError<TypeShape, &'static str>> {
        match self {
            Self::Variant(variants) => Ok(variants),
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
            (Self::Tuple(a), Self::Tuple(b)) => {
                let mut result = Tuple::empty();
                for (name, (a, b)) in a.zip(b)?.into_iter() {
                    let value = Inferrable::make_same(a, b)?;
                    result.add(name, value);
                }
                Self::Tuple(result)
            }
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
            (Self::Type, Self::Type) => Self::Type,
            (Self::Type, _) => fail!(),
            (Self::SyntaxModule, Self::SyntaxModule) => Self::SyntaxModule,
            (Self::SyntaxModule, _) => fail!(),
            (Self::SyntaxDefinition, Self::SyntaxDefinition) => Self::SyntaxDefinition,
            (Self::SyntaxDefinition, _) => fail!(),
            (Self::Binding(a), Self::Binding(b)) if a == b => Self::Binding(a),
            (Self::Binding(_), _) => fail!(),
            (Self::Variant(a), Self::Variant(b)) => {
                if a == b {
                    Self::Variant(a)
                } else {
                    // TODO
                    fail!()
                }
            }
            (Self::Variant(_), _) => fail!(),
            (Self::Symbol, Self::Symbol) => Self::Symbol,
            (Self::Symbol, _) => fail!(),
            (Self::HashMap(a), Self::HashMap(b)) => Self::HashMap(HashMapType {
                key: Inferrable::make_same(a.key, b.key)?,
                value: Inferrable::make_same(a.value, b.value)?,
            }),
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
        })
    }
}

impl std::fmt::Display for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnwindHandle(ty) => write!(f, "unwindable block handle {ty}"),
            Self::Unit => write!(f, "unit"),
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
            Self::Macro(ty) => write!(f, "macro {ty}"),
            Self::Multiset => write!(f, "multiset"),
            Self::Ast => write!(f, "ast"),
            Self::Type => write!(f, "type"),
            Self::Contexts => write!(f, "contexts"),
            Self::SyntaxModule => write!(f, "syntax module"),
            Self::SyntaxDefinition => write!(f, "syntax definition"),
            Self::Binding(binding) => binding.fmt(f),
            Self::Variant(variants) => {
                for (index, variant) in variants.iter().enumerate() {
                    if index != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "| {variant}")?;
                }
                Ok(())
            }
            Self::Symbol => write!(f, "symbol"),
            Self::HashMap(map) => map.fmt(f),
            Self::Ref(ty) => write!(f, "&{ty}"),
            Self::NewType { id, inner } => write!(f, "newtype({id}, {inner})"),
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

impl Inferrable for FnType {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            arg: Inferrable::make_same(a.arg, b.arg)?,
            contexts: Inferrable::make_same(a.contexts, b.contexts)?,
            result: Inferrable::make_same(a.result, b.result)?,
        })
    }
}

impl SubstituteBindings for HashMapType {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            key: self.key.substitute_bindings(kast, cache),
            value: self.value.substitute_bindings(kast, cache),
        }
    }
}

impl SubstituteBindings for Type {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        let inferred = match self.inferred() {
            Ok(inferred) => inferred,
            Err(_) => {
                return self;
            }
        };
        if let Some(result) = cache.get(self.var()) {
            return result;
        }
        cache.insert(self.var(), self.clone());
        tracing::trace!("subbing {}", inferred.show_short());
        let result = match inferred {
            TypeShape::Unit
            | TypeShape::Bool
            | TypeShape::Int32
            | TypeShape::Int64
            | TypeShape::Float64
            | TypeShape::Char
            | TypeShape::String
            | TypeShape::Ast
            | TypeShape::Multiset
            | TypeShape::Contexts
            | TypeShape::Type
            | TypeShape::Symbol
            | TypeShape::SyntaxModule
            | TypeShape::SyntaxDefinition => self.clone(),
            TypeShape::List(a) => TypeShape::List(a.substitute_bindings(kast, cache)).into(),
            TypeShape::Variant(variants) => TypeShape::Variant(
                variants
                    .into_iter()
                    .map(|variant| VariantType {
                        name: variant.name.clone(),
                        value: variant
                            .value
                            .map(|ty| Box::new(ty.substitute_bindings(kast, cache))),
                    })
                    .collect(),
            )
            .into(),
            TypeShape::UnwindHandle(ty) => {
                TypeShape::UnwindHandle(ty.substitute_bindings(kast, cache)).into()
            }
            TypeShape::Tuple(tuple) => {
                TypeShape::Tuple(tuple.map(|ty| ty.substitute_bindings(kast, cache))).into()
            }
            TypeShape::Function(f) => {
                TypeShape::Function(Box::new((*f).substitute_bindings(kast, cache))).into()
            }
            TypeShape::Template(t) => TypeShape::Template(t).into(),
            TypeShape::Macro(f) => {
                TypeShape::Macro(Box::new((*f).substitute_bindings(kast, cache))).into()
            }
            TypeShape::Binding(binding) => match kast.scopes.interpreter.get(&binding.symbol) {
                Some(value) => value
                    .clone_value()
                    .unwrap()
                    .inferred()
                    .unwrap()
                    .into_type()
                    .unwrap_or_else(|e| {
                        panic!("{} expected to be a type: {e}", binding.symbol.name())
                    }),
                None => TypeShape::Binding(binding.clone()).into(),
            },
            TypeShape::HashMap(map) => {
                TypeShape::HashMap(map.substitute_bindings(kast, cache)).into()
            }
            TypeShape::Ref(ty) => TypeShape::Ref(ty.substitute_bindings(kast, cache)).into(),
            TypeShape::NewType { .. } => self.clone(), // TODO maybe need to sub?
        };
        cache.insert(self.var(), result.clone());
        tracing::trace!("subbed as {result}");
        result
    }
}

impl SubstituteBindings for FnType {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            arg: self.arg.substitute_bindings(kast, cache),
            contexts: self.contexts.substitute_bindings(kast, cache),
            result: self.result.substitute_bindings(kast, cache),
        }
    }
}
