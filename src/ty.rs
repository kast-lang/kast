use super::*;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int32,
    String,
    Tuple(Tuple<Type>),
    Function(Box<FnType>),
    Macro(Box<FnType>),
    Ast,
    #[allow(clippy::enum_variant_names)]
    Type,

    Infer(inference::Var<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self.inferred(), other.inferred()) {
            (Ok(a), Ok(b)) => match (a, b) {
                (Type::Infer(_), _) | (_, Type::Infer(_)) => unreachable!(),

                (Self::Unit, Self::Unit) => true,
                (Self::Unit, _) => false,
                (Self::Bool, Self::Bool) => true,
                (Self::Bool, _) => false,
                (Self::Int32, Self::Int32) => true,
                (Self::Int32, _) => false,
                (Self::String, Self::String) => true,
                (Self::String, _) => false,
                (Self::Tuple(a), Self::Tuple(b)) => a == b,
                (Self::Tuple(_), _) => false,
                (Self::Function(a), Self::Function(b)) => a == b,
                (Self::Function(_), _) => false,
                (Self::Macro(a), Self::Macro(b)) => a == b,
                (Self::Macro(_), _) => false,
                (Self::Ast, Self::Ast) => true,
                (Self::Ast, _) => false,
                (Self::Type, Self::Type) => true,
                (Self::Type, _) => false,
            },
            (Err(a), Err(b)) => a.is_same_as(b),
            (Ok(_), Err(_)) | (Err(_), Ok(_)) => false,
        }
    }
}

impl Eq for Type {}

impl Type {
    /// Get actual value (if it is an inference var)
    pub fn inferred(&self) -> Result<Self, &inference::Var<Type>> {
        if let Self::Infer(var) = self {
            var.get().map(|value| (*value).clone()).ok_or(var)
        } else {
            Ok(self.clone())
        }
    }
    pub fn make_same(&mut self, other: Self) -> eyre::Result<()> {
        *self = Inferrable::make_same(self.clone(), other)?;
        Ok(())
    }
}

impl Inferrable for Type {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        macro_rules! fail {
            () => {
                eyre::bail!("expected {a}, got {b}")
            };
        }
        Ok(match (a.inferred(), b.inferred()) {
            (Ok(a), Ok(b)) => match (a, b) {
                (Type::Infer(_), _) | (_, Type::Infer(_)) => unreachable!(),

                (Type::Unit, Type::Unit) => Type::Unit,
                (Type::Unit, _) => fail!(),
                (Type::Bool, Type::Bool) => Type::Bool,
                (Type::Bool, _) => fail!(),
                (Type::Int32, Type::Int32) => Type::Int32,
                (Type::Int32, _) => fail!(),
                (Type::String, Type::String) => Type::String,
                (Type::String, _) => fail!(),
                (Type::Tuple(a), Type::Tuple(b)) => {
                    let mut result = Tuple::empty();
                    for (name, (a, b)) in a.zip(b)?.into_iter() {
                        let value = Inferrable::make_same(a, b)?;
                        result.add(name, value);
                    }
                    Type::Tuple(result)
                }
                (Type::Tuple(_), _) => fail!(),
                (Type::Function(a), Type::Function(b)) => {
                    Type::Function(Box::new(Inferrable::make_same(*a, *b)?))
                }
                (Type::Function(_), _) => fail!(),
                (Type::Macro(a), Type::Macro(b)) => {
                    Type::Macro(Box::new(Inferrable::make_same(*a, *b)?))
                }
                (Type::Macro(_), _) => fail!(),
                (Type::Ast, Type::Ast) => Type::Ast,
                (Type::Ast, _) => fail!(),
                (Type::Type, Type::Type) => Type::Type,
                (Type::Type, _) => fail!(),
            },
            (Ok(a), Err(b)) => {
                b.set(a.clone())?;
                a
            }
            (Err(a), Ok(b)) => {
                a.set(b.clone())?;
                b
            }
            (Err(a), Err(b)) => {
                a.make_same(b)?;
                Type::Infer(a.clone())
            }
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int32 => write!(f, "int32"),
            Type::String => write!(f, "string"),
            Type::Tuple(tuple) => tuple.fmt(f),
            Type::Function(ty) => ty.fmt(f),
            Type::Macro(ty) => write!(f, "macro {ty}"),
            Type::Ast => write!(f, "ast"),
            Type::Type => write!(f, "type"),
            Type::Infer(var) => match var.get() {
                Some(inferred) => inferred.fmt(f),
                None => write!(f, "<not inferred>"),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub arg: Type,
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
