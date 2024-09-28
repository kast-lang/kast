use super::*;

#[derive(Clone)]
pub enum Type {
    Unit,
    Bool,
    Int32,
    String,
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
    pub fn make_same(&mut self, other: Self) {
        *self = inference::Inferrable::make_same(self.clone(), other);
    }
}

impl inference::Inferrable for Type {
    fn make_same(a: Self, b: Self) -> Self {
        macro_rules! fail {
            () => {
                panic!("type check error")
            };
        }
        match (a.inferred(), b.inferred()) {
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
                (Type::Type, Type::Type) => Type::Type,
                (Type::Type, _) => fail!(),
            },
            (Ok(a), Err(b)) => {
                b.set(a.clone());
                a
            }
            (Err(a), Ok(b)) => {
                a.set(b.clone());
                b
            }
            (Err(a), Err(b)) => {
                a.make_same(b);
                Type::Infer(a.clone())
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int32 => write!(f, "int32"),
            Type::String => write!(f, "string"),
            Type::Type => write!(f, "type"),
            Type::Infer(var) => todo!(),
        }
    }
}
