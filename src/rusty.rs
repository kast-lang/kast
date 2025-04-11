use super::*;

pub trait Rusty: Sized {
    fn ty() -> Option<Type>;
    fn from_value(value: Value) -> eyre::Result<Self>;
    fn into_value(self) -> Value;
}

impl Rusty for () {
    fn ty() -> Option<Type> {
        Some(TypeShape::Unit.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_unit()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Unit.into()
    }
}

impl Rusty for bool {
    fn ty() -> Option<Type> {
        Some(TypeShape::Bool.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_bool()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Bool(self).into()
    }
}

impl Rusty for i32 {
    fn ty() -> Option<Type> {
        Some(TypeShape::Int32.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_int32()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Int32(self).into()
    }
}

impl Rusty for i64 {
    fn ty() -> Option<Type> {
        Some(TypeShape::Int64.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_int64()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Int64(self).into()
    }
}

impl Rusty for f64 {
    fn ty() -> Option<Type> {
        Some(TypeShape::Float64.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_float64()?.into())
    }
    fn into_value(self) -> Value {
        ValueShape::Float64(OrderedFloat(self)).into()
    }
}

impl Rusty for OrderedFloat<f64> {
    fn ty() -> Option<Type> {
        Some(TypeShape::Float64.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_float64()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Float64(self).into()
    }
}

impl Rusty for char {
    fn ty() -> Option<Type> {
        Some(TypeShape::Char.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_char()?)
    }
    fn into_value(self) -> Value {
        ValueShape::Char(self).into()
    }
}

impl Rusty for String {
    fn ty() -> Option<Type> {
        Some(TypeShape::String.into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value.into_inferred()?.into_string()?)
    }
    fn into_value(self) -> Value {
        ValueShape::String(self).into()
    }
}

impl<T: Rusty> Rusty for Vec<T> {
    fn ty() -> Option<Type> {
        Some(TypeShape::List(T::ty().unwrap_or_else(|| Type::new_not_inferred("Vec<_>"))).into())
    }
    fn from_value(value: Value) -> eyre::Result<Self> {
        Ok(value
            .into_inferred()?
            .into_list()?
            .values
            .into_iter()
            .map(T::from_value)
            .collect::<Result<_, eyre::Report>>()?)
    }
    fn into_value(self) -> Value {
        ValueShape::List(ListValue {
            values: self.into_iter().map(T::into_value).collect(),
            element_ty: T::ty().unwrap_or_else(|| Type::new_not_inferred("Vec<_>")),
        })
        .into()
    }
}
