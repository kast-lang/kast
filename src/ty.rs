use super::*;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int32,
    String,
    Function(Box<FnType>),
    #[allow(clippy::enum_variant_names)]
    Type,

    Infer(inference::Var<Type>),
}

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
                (Type::Function(a), Type::Function(b)) => {
                    Type::Function(Box::new(Inferrable::make_same(*a, *b)?))
                }
                (Type::Function(_), _) => fail!(),
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
            Type::Function(ty) => ty.fmt(f),
            Type::Type => write!(f, "type"),
            Type::Infer(var) => match var.get() {
                Some(inferred) => inferred.fmt(f),
                None => write!(f, "<not inferred>"),
            },
        }
    }
}

#[derive(Debug, Clone)]
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
