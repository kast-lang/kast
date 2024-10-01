use super::*;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int32(i32),
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
    Binding(Arc<Binding>),
    Type(Type),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Unit, _) => false,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Bool(_), _) => false,
            (Self::Int32(a), Self::Int32(b)) => a == b,
            (Self::Int32(_), _) => false,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::String(_), _) => false,
            (Self::NativeFunction(a), Self::NativeFunction(b)) => {
                Arc::ptr_eq(&a.r#impl, &b.r#impl) && a.ty == b.ty
            }
            (Self::NativeFunction(_), _) => false,
            (Self::Binding(a), Self::Binding(b)) => Arc::ptr_eq(a, b),
            (Self::Binding(_), _) => false,
            (Self::Function(a), Self::Function(b)) => a == b,
            (Self::Function(_), _) => false,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::Type(_), _) => false,
        }
    }
}

impl Eq for Value {}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Value::Bool(value) => value.fmt(f),
            Value::Int32(value) => value.fmt(f),
            Value::String(s) => write!(f, "{s:?}"),
            Value::NativeFunction(function) => function.fmt(f),
            Value::Binding(binding) => binding.fmt(f),
            Value::Function(_function) => write!(f, "<function>"),
            Value::Type(ty) => {
                write!(f, "type ")?;
                ty.fmt(f)
            }
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
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
            Value::Binding(_b) => todo!(),
            Value::Function(f) => Type::Function(Box::new(f.ty.clone())),
            Value::NativeFunction(f) => Type::Function(Box::new(f.ty.clone())),
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

#[derive(Clone)]
pub struct Function {
    pub id: Id,
    pub ty: FnType,
    pub captured: Arc<Scope>,
    pub compiled: MaybeCompiledFn,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Function {}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub r#impl: Arc<dyn Fn(FnType, Value) -> eyre::Result<Value> + Send + Sync>,
    pub ty: FnType,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn {:?}>", self.name)
    }
}

impl std::fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
