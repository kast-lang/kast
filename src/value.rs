use super::*;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int32(i32),
    String(String),
    Tuple(Tuple<Value>),
    Function(TypedFunction),
    Template(Function),
    Macro(TypedFunction),
    NativeFunction(NativeFunction),
    Binding(Arc<Binding>),
    Ast(Ast),
    Type(Type),
    SyntaxModule(Arc<Vec<Arc<ast::SyntaxDefinition>>>),
    SyntaxDefinition(Arc<ast::SyntaxDefinition>),
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
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (Self::Tuple(_), _) => false,
            (Self::NativeFunction(a), Self::NativeFunction(b)) => {
                Arc::ptr_eq(&a.r#impl, &b.r#impl) && a.ty == b.ty
            }
            (Self::NativeFunction(_), _) => false,
            (Self::Binding(a), Self::Binding(b)) => Arc::ptr_eq(a, b),
            (Self::Binding(_), _) => false,
            (Self::Function(a), Self::Function(b)) => a == b,
            (Self::Function(_), _) => false,
            (Self::Template(a), Self::Template(b)) => a == b,
            (Self::Template(_), _) => false,
            (Self::Macro(a), Self::Macro(b)) => a == b,
            (Self::Macro(_), _) => false,
            (Self::Ast(a), Self::Ast(b)) => a == b,
            (Self::Ast(_), _) => false,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::Type(_), _) => false,
            (Self::SyntaxModule(a), Self::SyntaxModule(b)) => Arc::ptr_eq(a, b),
            (Self::SyntaxModule(_), _) => false,
            (Self::SyntaxDefinition(a), Self::SyntaxDefinition(b)) => Arc::ptr_eq(a, b),
            (Self::SyntaxDefinition(_), _) => false,
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
            Self::Unit => Ok(Type::Unit), // TODO this is a hack (maybe, maybe not?)
            Self::Binding(binding) => {
                binding.ty.clone().make_same(Type::Type).unwrap(); // TODO dont unwrap
                Ok(Type::Binding(binding))
            }
            Self::Type(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: Type::Type,
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
            Value::Tuple(tuple) => Type::Tuple(tuple.as_ref().map(|field| field.ty())),
            Value::Binding(binding) => binding.ty.clone(), // TODO not sure, maybe Type::Binding?
            Value::Function(f) => Type::Function(Box::new(f.ty.clone())),
            Value::Template(t) => Type::Template(t.compiled.clone()),
            Value::Macro(f) => Type::Macro(Box::new(f.ty.clone())),
            Value::NativeFunction(f) => Type::Function(Box::new(f.ty.clone())),
            Value::Ast(_) => Type::Ast,
            Value::Type(_) => Type::Type,
            Value::SyntaxModule(_) => Type::SyntaxModule,
            Value::SyntaxDefinition(_) => Type::SyntaxDefinition,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{value} is not {expected}")]
pub struct ExpectError<V = Value, Expected = Type> {
    pub value: V,
    pub expected: Expected,
}

impl Value {
    pub fn expect_syntax_definition(self) -> Result<Arc<ast::SyntaxDefinition>, ExpectError> {
        match self {
            Self::SyntaxDefinition(def) => Ok(def),
            _ => Err(ExpectError {
                value: self,
                expected: Type::SyntaxModule,
            }),
        }
    }
    pub fn expect_syntax_module(self) -> Result<Arc<Vec<Arc<ast::SyntaxDefinition>>>, ExpectError> {
        match self {
            Self::SyntaxModule(syntax) => Ok(syntax),
            _ => Err(ExpectError {
                value: self,
                expected: Type::SyntaxModule,
            }),
        }
    }
    pub fn expect_unit(self) -> Result<(), ExpectError> {
        match self {
            Self::Unit => Ok(()),
            _ => Err(ExpectError {
                value: self,
                expected: Type::Unit,
            }),
        }
    }
    pub fn expect_string(self) -> Result<String, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self,
                expected: Type::String,
            }),
        }
    }
    pub fn expect_ast(self) -> Result<Ast, ExpectError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            _ => Err(ExpectError {
                value: self,
                expected: Type::Ast,
            }),
        }
    }
    pub fn expect_function(self) -> Result<TypedFunction, ExpectError> {
        match self {
            Self::Function(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: Type::Function(Box::new(FnType {
                    arg: Type::Infer(inference::Var::new()),
                    result: Type::Infer(inference::Var::new()),
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

#[derive(Clone)]
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

impl PartialEq for TypedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypedFunction {}

#[derive(Clone)]
pub struct Function {
    pub id: Id,
    pub captured: Arc<Scope>,
    pub compiled: MaybeCompiledFn,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {:?}>", self.id)
    }
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
