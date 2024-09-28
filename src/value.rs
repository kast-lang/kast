use super::*;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int32(i32),
    String(String),
    Type(Type),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Value::Bool(value) => value.fmt(f),
            Value::Int32(value) => value.fmt(f),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Type(ty) => ty.fmt(f),
        }
    }
}

impl Value {
    /// Get this value AS a type
    pub fn into_ty(self) -> Result<Type, Self> {
        match self {
            Self::Type(ty) => Ok(ty),
            _ => Err(self),
        }
    }
    /// Get the type OF this value
    pub fn ty(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Int32(_) => Type::Int32,
            Value::String(_) => Type::String,
            Value::Type(_) => Type::Type,
        }
    }
    pub fn expect_string(self) -> Result<String, Self> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(self),
        }
    }
}
