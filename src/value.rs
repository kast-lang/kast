use super::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Unit,
    Bool(bool),
    Int32(i32),
    Int64(i64),
    Float64(OrderedFloat<f64>),
    String(String),
    Tuple(Tuple<Value>),
    Function(TypedFunction),
    Template(Function),
    Macro(TypedFunction),
    NativeFunction(NativeFunction),
    Binding(Parc<Binding>),
    Variant(VariantValue),
    Multiset(Vec<Value>),
    Ast(Ast),
    Type(Type),
    SyntaxModule(Parc<Vec<Parc<ast::SyntaxDefinition>>>),
    SyntaxDefinition(Parc<ast::SyntaxDefinition>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariantValue {
    pub name: String,
    pub value: Option<Box<Value>>,
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
            Value::Tuple(tuple) => tuple.fmt(f),
            Value::NativeFunction(function) => function.fmt(f),
            Value::Binding(binding) => binding.fmt(f),
            Value::Function(_function) => write!(f, "<function>"),
            Value::Template(_template) => write!(f, "<template>"),
            Value::Macro(_macro) => write!(f, "<macro>"),
            Value::Ast(ast) => ast.fmt(f),
            Value::Type(ty) => {
                write!(f, "type ")?;
                ty.fmt(f)
            }
            Value::SyntaxModule(_definitions) => write!(f, "<syntax module>"),
            Value::SyntaxDefinition(_definition) => write!(f, "<syntax definition>"),
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
            Self::Unit => Ok(InferredType::Unit.into()), // TODO this is a hack (maybe, maybe not?)
            Self::Binding(binding) => {
                binding.ty.expect_inferred(InferredType::Type).unwrap(); // TODO dont unwrap
                Ok(InferredType::Binding(binding).into())
            }
            Self::Type(ty) => Ok(ty),
            Self::Tuple(tuple) => {
                let mut ty = Tuple::empty();
                for (name, value) in tuple {
                    ty.add(name, value.expect_type()?);
                }
                Ok(InferredType::Tuple(ty).into())
            }
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Type,
            }),
        }
    }
    /// Get the type OF this value
    pub fn ty(&self) -> Type {
        match self {
            Value::Unit => InferredType::Unit.into(),
            Value::Multiset(_) => InferredType::Multiset.into(),
            Value::Variant(value) => value.ty.clone(),
            Value::Bool(_) => InferredType::Bool.into(),
            Value::Int32(_) => InferredType::Int32.into(),
            Value::Int64(_) => InferredType::Int64.into(),
            Value::Float64(_) => InferredType::Float64.into(),
            Value::String(_) => InferredType::String.into(),
            Value::Tuple(tuple) => {
                InferredType::Tuple(tuple.as_ref().map(|field| field.ty())).into()
            }
            Value::Binding(binding) => binding.ty.clone(), // TODO not sure, maybe Type::Binding?
            Value::Function(f) => InferredType::Function(Box::new(f.ty.clone())).into(),
            Value::Template(t) => InferredType::Template(t.compiled.clone()).into(),
            Value::Macro(f) => InferredType::Macro(Box::new(f.ty.clone())).into(),
            Value::NativeFunction(f) => InferredType::Function(Box::new(f.ty.clone())).into(),
            Value::Ast(_) => InferredType::Ast.into(),
            Value::Type(_) => InferredType::Type.into(),
            Value::SyntaxModule(_) => InferredType::SyntaxModule.into(),
            Value::SyntaxDefinition(_) => InferredType::SyntaxDefinition.into(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{value} is not {expected}")]
pub struct ExpectError<V = Value, Expected = InferredType> {
    pub value: V,
    pub expected: Expected,
}

impl Value {
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
    pub fn expect_syntax_definition(self) -> Result<Parc<ast::SyntaxDefinition>, ExpectError> {
        match self {
            Self::SyntaxDefinition(def) => Ok(def),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::SyntaxModule,
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
                expected: InferredType::SyntaxModule,
            }),
        }
    }
    pub fn expect_unit(self) -> Result<(), ExpectError> {
        match self {
            Self::Unit => Ok(()),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Unit,
            }),
        }
    }
    pub fn expect_string(self) -> Result<String, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::String,
            }),
        }
    }
    pub fn expect_int32(self) -> Result<i32, ExpectError> {
        match self {
            Self::Int32(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Int32,
            }),
        }
    }
    pub fn expect_int64(self) -> Result<i64, ExpectError> {
        match self {
            Self::Int64(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Int64,
            }),
        }
    }
    pub fn expect_bool(self) -> Result<bool, ExpectError> {
        match self {
            Self::Bool(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Bool,
            }),
        }
    }
    pub fn expect_ast(self) -> Result<Ast, ExpectError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Ast,
            }),
        }
    }
    pub fn expect_function(self) -> Result<TypedFunction, ExpectError> {
        match self {
            Self::Function(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: InferredType::Function(Box::new(FnType {
                    arg: Type::new_not_inferred(),
                    result: Type::new_not_inferred(),
                })),
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypedFunction {
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

pub type NativeFunctionImpl = dyn Fn(FnType, Value) -> eyre::Result<Value> + Send + Sync;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NativeFunction {
    pub name: String,
    pub r#impl: Parc<NativeFunctionImpl>,
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
