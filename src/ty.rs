use super::*;

/// Partially inferred type, so we know the shape of it
#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
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
    Ast,
    #[allow(clippy::enum_variant_names)]
    Type,
    SyntaxModule,
    SyntaxDefinition,
    Binding(Parc<Binding>),
}

pub type Type = inference::MaybeNotInferred<TypeShape>;

#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
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
            (Self::Unit, Self::Unit) => Self::Unit,
            (Self::Unit, _) => fail!(),
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
        })
    }
}

impl std::fmt::Display for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
pub struct FnType {
    #[try_hash]
    pub arg: Type,
    #[try_hash]
    pub result: Type,
}

impl std::fmt::Display for FnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.arg, self.result)
    }
}

impl Inferrable for FnType {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            arg: Inferrable::make_same(a.arg, b.arg)?,
            result: Inferrable::make_same(a.result, b.result)?,
        })
    }
}

impl SubstituteBindings for Type {
    fn substitute_bindings(self, kast: &Kast) -> Self {
        match self.inferred() {
            Ok(inferred) => match inferred {
                TypeShape::Unit
                | TypeShape::Bool
                | TypeShape::Int32
                | TypeShape::Int64
                | TypeShape::Float64
                | TypeShape::String
                | TypeShape::Ast
                | TypeShape::Multiset
                | TypeShape::Type
                | TypeShape::SyntaxModule
                | TypeShape::SyntaxDefinition => self.clone(),
                TypeShape::Variant(variants) => TypeShape::Variant(
                    variants
                        .into_iter()
                        .map(|variant| VariantType {
                            name: variant.name.clone(),
                            value: variant
                                .value
                                .map(|ty| Box::new(ty.substitute_bindings(kast))),
                        })
                        .collect(),
                )
                .into(),
                TypeShape::Tuple(tuple) => {
                    TypeShape::Tuple(tuple.map(|ty| ty.substitute_bindings(kast))).into()
                }
                TypeShape::Function(f) => {
                    TypeShape::Function(Box::new((*f).substitute_bindings(kast))).into()
                }
                TypeShape::Template(t) => TypeShape::Template(t).into(),
                TypeShape::Macro(f) => {
                    TypeShape::Macro(Box::new((*f).substitute_bindings(kast))).into()
                }
                TypeShape::Binding(binding) => {
                    match kast.interpreter.get_nowait(binding.name.raw()) {
                        Some(value) => value.expect_type().unwrap_or_else(|e| {
                            panic!("{} expected to be a type: {e}", binding.name.raw())
                        }),
                        None => TypeShape::Binding(binding.clone()).into(),
                    }
                }
            },
            Err(_var) => self,
        }
    }
}

impl SubstituteBindings for FnType {
    fn substitute_bindings(self, kast: &Kast) -> Self {
        Self {
            arg: self.arg.substitute_bindings(kast),
            result: self.result.substitute_bindings(kast),
        }
    }
}
