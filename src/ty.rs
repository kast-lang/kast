use super::*;

#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
pub enum InferredType {
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

#[derive(Debug, Clone)]
pub struct Type(inference::Var<InferredType>);

impl From<InferredType> for Type {
    fn from(value: InferredType) -> Self {
        Self({
            let var = inference::Var::new();
            var.set(value).unwrap();
            var
        })
    }
}

impl From<inference::Var<InferredType>> for Type {
    fn from(value: inference::Var<InferredType>) -> Self {
        Self(value)
    }
}

impl TryHash for Type {
    type Error = eyre::Report;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        match self.inferred() {
            Ok(ty) => ty.try_hash(hasher).map_err(|e| eyre!(e))?,
            Err(_) => eyre::bail!("type is not inferred, fail to hash"),
        }
        Ok(())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self.0.get(), other.0.get()) {
            (Some(a), Some(b)) => a == b,
            _ => self.0.is_same_as(&other.0),
        }
    }
}

impl Eq for Type {}

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

impl Type {
    pub fn expect_inferred(&self, ty: InferredType) -> eyre::Result<()> {
        self.0.set(ty)
    }
    pub fn new_not_inferred() -> Self {
        Self(inference::Var::new())
    }
    /// Get actual value (if it is an inference var)
    pub fn inferred(&self) -> Result<InferredType, &inference::Var<InferredType>> {
        self.0.get().map(|value| (*value).clone()).ok_or(&self.0)
    }
    pub fn make_same(&mut self, other: Self) -> eyre::Result<()> {
        *self = Inferrable::make_same(self.clone(), other)?;
        Ok(())
    }
}

impl InferredType {
    pub fn expect_variant(
        self,
    ) -> Result<Vec<VariantType>, ExpectError<InferredType, &'static str>> {
        match self {
            Self::Variant(variants) => Ok(variants),
            _ => Err(ExpectError {
                value: self,
                expected: "variant",
            }),
        }
    }
    pub fn expect_template(
        self,
    ) -> Result<MaybeCompiledFn, ExpectError<InferredType, &'static str>> {
        match self {
            Self::Template(t) => Ok(t),
            _ => Err(ExpectError {
                value: self,
                expected: "template",
            }),
        }
    }
}

impl Inferrable for Type {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        a.0.make_same(&b.0)?;
        Ok(a)
    }
}

impl Inferrable for InferredType {
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

impl std::fmt::Display for InferredType {
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.get() {
            Some(inferred) => inferred.fmt(f),
            None => write!(f, "<not inferred>"),
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

impl Kast {
    pub fn substitute_type_bindings(&self, ty: &Type) -> Type {
        match ty.inferred() {
            Ok(inferred) => match inferred {
                InferredType::Unit
                | InferredType::Bool
                | InferredType::Int32
                | InferredType::Int64
                | InferredType::Float64
                | InferredType::String
                | InferredType::Ast
                | InferredType::Multiset
                | InferredType::Type
                | InferredType::SyntaxModule
                | InferredType::SyntaxDefinition => ty.clone(),
                InferredType::Variant(variants) => InferredType::Variant(
                    variants
                        .iter()
                        .map(|variant| VariantType {
                            name: variant.name.clone(),
                            value: variant
                                .value
                                .as_ref()
                                .map(|ty| Box::new(self.substitute_type_bindings(ty))),
                        })
                        .collect(),
                )
                .into(),
                InferredType::Tuple(tuple) => {
                    InferredType::Tuple(tuple.as_ref().map(|ty| self.substitute_type_bindings(ty)))
                        .into()
                }
                InferredType::Function(f) => InferredType::Function(Box::new(FnType {
                    arg: self.substitute_type_bindings(&f.arg),
                    result: self.substitute_type_bindings(&f.result),
                }))
                .into(),
                InferredType::Template(t) => InferredType::Template(t).into(),
                InferredType::Macro(f) => InferredType::Macro(Box::new(FnType {
                    arg: self.substitute_type_bindings(&f.arg),
                    result: self.substitute_type_bindings(&f.result),
                }))
                .into(),
                InferredType::Binding(binding) => {
                    match self.interpreter.get_nowait(binding.name.raw()) {
                        Some(value) => value.expect_type().unwrap_or_else(|e| {
                            panic!("{} expected to be a type: {e}", binding.name.raw())
                        }),
                        None => InferredType::Binding(binding.clone()).into(),
                    }
                }
            },
            Err(_var) => ty.clone(),
        }
    }
}
