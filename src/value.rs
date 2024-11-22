use super::*;

#[derive(Clone, PartialEq, Eq, TryHash)]
pub enum Value {
    Unit,
    Bool(bool),
    Int32(i32),
    Int64(i64),
    Float64(OrderedFloat<f64>),
    String(String),
    Tuple(#[try_hash] Tuple<Value>),
    Function(#[try_hash] TypedFunction),
    Template(Function),
    Macro(#[try_hash] TypedFunction),
    NativeFunction(#[try_hash] NativeFunction),
    Binding(Parc<Binding>),
    Variant(#[try_hash] VariantValue),
    Multiset(#[try_hash] Vec<Value>),
    Contexts(#[try_hash] Contexts),
    Ast(AstValue),
    Type(#[try_hash] Type),
    SyntaxModule(Parc<Vec<Parc<ast::SyntaxDefinition>>>),
    SyntaxDefinition(Parc<ast::SyntaxDefinition>),
    UnwindHandle(#[try_hash] UnwindHandle),
}

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct UnwindHandle {
    pub sender: Parc<Mutex<async_oneshot::Sender<Value>>>,
    #[try_hash]
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AstValue {
    pub value: Ast,
    pub captured: Parc<Scope>,
}

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct VariantValue {
    pub name: String,
    #[try_hash]
    pub value: Option<Box<Value>>,
    #[try_hash]
    pub ty: Type,
}

impl std::fmt::Display for VariantValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}", self.name)?;
        if let Some(value) = &self.value {
            write!(f, " {value}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Variant(value) => write!(f, "{value}"),
            Value::Bool(value) => value.fmt(f),
            Value::Int32(value) => value.fmt(f),
            Value::Int64(value) => value.fmt(f),
            Value::Float64(value) => value.fmt(f),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Multiset(values) => {
                for (index, value) in values.iter().enumerate() {
                    if index != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "| ")?;
                    write!(f, "{value}")?;
                }
                Ok(())
            }
            Value::Contexts(contexts) => contexts.fmt(f),
            Value::Tuple(tuple) => tuple.fmt(f),
            Value::NativeFunction(function) => function.fmt(f),
            Value::Binding(binding) => binding.fmt(f),
            Value::Function(_function) => write!(f, "<function>"),
            Value::Template(_template) => write!(f, "<template>"),
            Value::Macro(_macro) => write!(f, "<macro>"),
            Value::Ast(AstValue { value, captured: _ }) => value.fmt(f),
            Value::Type(ty) => {
                write!(f, "type ")?;
                ty.fmt(f)
            }
            Value::SyntaxModule(_definitions) => write!(f, "<syntax module>"),
            Value::SyntaxDefinition(_definition) => write!(f, "<syntax definition>"),
            Value::UnwindHandle(_) => write!(f, "<unwind handle>"),
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
            Self::Binding(binding) => {
                binding.ty.expect_inferred(TypeShape::Type).unwrap(); // TODO dont unwrap
                Ok(TypeShape::Binding(binding).into())
            }
            Self::Type(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Type,
            }),
        }
    }
    /// Get the type OF this value
    pub fn ty(&self) -> Type {
        match self {
            Value::Unit => TypeShape::Unit.into(),
            Value::Multiset(_) => TypeShape::Multiset.into(),
            Value::Contexts(_) => TypeShape::Contexts.into(),
            Value::Variant(value) => value.ty.clone(),
            Value::Bool(_) => TypeShape::Bool.into(),
            Value::Int32(_) => TypeShape::Int32.into(),
            Value::Int64(_) => TypeShape::Int64.into(),
            Value::Float64(_) => TypeShape::Float64.into(),
            Value::String(_) => TypeShape::String.into(),
            Value::Tuple(tuple) => TypeShape::Tuple(tuple.as_ref().map(|field| field.ty())).into(),
            Value::Binding(binding) => binding.ty.clone(), // TODO not sure, maybe Type::Binding?
            Value::Function(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            Value::Template(t) => TypeShape::Template(t.compiled.clone()).into(),
            Value::Macro(f) => TypeShape::Macro(Box::new(f.ty.clone())).into(),
            Value::NativeFunction(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            Value::Ast(_) => TypeShape::Ast.into(),
            Value::Type(_) => TypeShape::Type.into(),
            Value::SyntaxModule(_) => TypeShape::SyntaxModule.into(),
            Value::SyntaxDefinition(_) => TypeShape::SyntaxDefinition.into(),
            Value::UnwindHandle(handle) => TypeShape::UnwindHandle(handle.ty.clone()).into(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{value} is not {expected}")]
pub struct ExpectError<V = Value, Expected = TypeShape> {
    pub value: V,
    pub expected: Expected,
}

impl Value {
    pub fn expect_unwind_handle(self) -> Result<UnwindHandle, ExpectError<Value, &'static str>> {
        match self {
            Self::UnwindHandle(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "unwind handle",
            }),
        }
    }
    pub fn expect_macro(self) -> Result<TypedFunction, ExpectError<Value, &'static str>> {
        match self {
            Self::Macro(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "macro",
            }),
        }
    }
    pub fn expect_variant(self) -> Result<VariantValue, ExpectError<Value, &'static str>> {
        match self {
            Self::Variant(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "variant",
            }),
        }
    }
    pub fn expect_tuple(self) -> Result<Tuple<Value>, ExpectError<Value, &'static str>> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(ExpectError {
                value: self,
                expected: "tuple",
            }),
        }
    }
    pub fn into_contexts(self) -> Result<Contexts, ExpectError> {
        match self {
            Self::Unit => Ok(Contexts::empty()),
            Self::Contexts(contexts) => Ok(contexts),
            Self::Type(ty) => Ok(Contexts::from_list([ty])),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Contexts,
            }),
        }
    }
    pub fn expect_syntax_definition(self) -> Result<Parc<ast::SyntaxDefinition>, ExpectError> {
        match self {
            Self::SyntaxDefinition(def) => Ok(def),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::SyntaxModule,
            }),
        }
    }
    pub fn expect_syntax_module(
        self,
    ) -> Result<Parc<Vec<Parc<ast::SyntaxDefinition>>>, ExpectError> {
        match self {
            Self::SyntaxModule(syntax) => Ok(syntax),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::SyntaxModule,
            }),
        }
    }
    pub fn expect_unit(self) -> Result<(), ExpectError> {
        match self {
            Self::Unit => Ok(()),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Unit,
            }),
        }
    }
    pub fn expect_string(self) -> Result<String, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::String,
            }),
        }
    }
    pub fn expect_int32(self) -> Result<i32, ExpectError> {
        match self {
            Self::Int32(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Int32,
            }),
        }
    }
    pub fn expect_int64(self) -> Result<i64, ExpectError> {
        match self {
            Self::Int64(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Int64,
            }),
        }
    }
    pub fn expect_bool(self) -> Result<bool, ExpectError> {
        match self {
            Self::Bool(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Bool,
            }),
        }
    }
    pub fn expect_ast(self) -> Result<AstValue, ExpectError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Ast,
            }),
        }
    }
    pub fn expect_function(self) -> Result<TypedFunction, ExpectError<Value, &'static str>> {
        match self {
            Self::Function(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: "function",
            }),
        }
    }
    pub fn expect_template(self) -> Result<Function, ExpectError<Value, &'static str>> {
        match self {
            Self::Template(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: "template",
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct TypedFunction {
    #[try_hash]
    pub ty: FnType,
    pub f: Function,
}

impl std::ops::Deref for TypedFunction {
    type Target = Function;
    fn deref(&self) -> &Self::Target {
        &self.f
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub id: Id,
    pub captured: Parc<Scope>,
    pub compiled: MaybeCompiledFn,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {:?}>", self.id)
    }
}

pub type NativeFunctionImpl = dyn Fn(Kast, FnType, Value) -> eyre::Result<Value> + Send + Sync;

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct NativeFunction {
    pub name: String,
    pub r#impl: Parc<NativeFunctionImpl>,
    #[try_hash]
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
