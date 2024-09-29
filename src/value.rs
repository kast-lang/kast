use super::*;

#[derive(Debug, Clone)]
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
            Value::Type(ty) => {
                write!(f, "type ")?;
                ty.fmt(f)
            }
        }
    }
}

impl Value {
    /// Get this value AS a type
    pub fn expect_type(self) -> Result<Type, ExpectError> {
        match self {
            Self::Type(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected_ty: Type::Type,
            }),
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
}

#[derive(Debug, thiserror::Error)]
#[error("{value} is not {expected_ty}")]
pub struct ExpectError {
    value: Value,
    expected_ty: Type,
}

impl Value {
    pub fn expect_string(self) -> Result<String, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self,
                expected_ty: Type::String,
            }),
        }
    }
}
