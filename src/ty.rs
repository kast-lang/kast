use super::*;

/// Partially inferred type, so we know the shape of it
#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub enum TypeShape {
    Unit,
    Bool,
    Int32,
    Int64,
    Float64,
    String,
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
}

impl ShowShort for TypeShape {
    fn show_short(&self) -> &'static str {
        match self {
            TypeShape::Unit => "unit",
            TypeShape::Bool => "bool",
            TypeShape::Int32 => "int32",
            TypeShape::Int64 => "int64",
            TypeShape::Float64 => "float64",
            TypeShape::String => "string",
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
        }
    }
}

pub type Type = inference::MaybeNotInferred<TypeShape>;

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
                eyre::bail!("expected {a}, got {b}")
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
            (Self::String, Self::String) => Self::String,
            (Self::String, _) => fail!(),
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
            Self::String => write!(f, "string"),
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

impl SubstituteBindings for Type {
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        let inferred = match self.inferred() {
            Ok(inferred) => inferred,
            Err(_) => {
                return self;
            }
        };
        if !cache.insert(self.var()) {
            return self;
        }
        tracing::trace!("subbing {}", inferred.show_short());
        let result = match inferred {
            TypeShape::Unit
            | TypeShape::Bool
            | TypeShape::Int32
            | TypeShape::Int64
            | TypeShape::Float64
            | TypeShape::String
            | TypeShape::Ast
            | TypeShape::Multiset
            | TypeShape::Contexts
            | TypeShape::Type
            | TypeShape::Symbol
            | TypeShape::SyntaxModule
            | TypeShape::SyntaxDefinition => self.clone(),
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
            TypeShape::Binding(binding) => {
                match kast.scope.get(&binding).now_or_never().flatten() {
                    Some(value) => value.expect_type().unwrap_or_else(|e| {
                        panic!("{} expected to be a type: {e}", binding.symbol.name())
                    }),
                    None => TypeShape::Binding(binding.clone()).into(),
                }
            }
        };
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
