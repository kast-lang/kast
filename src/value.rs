use super::*;

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub enum ValueShape {
    Unit,
    Bool(bool),
    Int32(i32),
    Int64(i64),
    Float64(OrderedFloat<f64>),
    Char(char),
    String(String),
    List(#[try_hash] ListValue),
    Tuple(#[try_hash] TupleValue),
    Function(#[try_hash] TypedFunction),
    Template(#[try_hash] Function),
    Macro(#[try_hash] TypedFunction),
    NativeFunction(#[try_hash] NativeFunction),
    Binding(Parc<Binding>),
    Variant(#[try_hash] VariantValue),
    Multiset(#[try_hash] Vec<Value>),
    Contexts(#[try_hash] Contexts),
    Ast(Ast),
    Expr(Parc<Expr>),
    Type(#[try_hash] Type),
    SyntaxModule(Parc<Vec<Parc<ast::SyntaxDefinition>>>),
    SyntaxDefinition(Parc<ast::SyntaxDefinition>),
    UnwindHandle(#[try_hash] UnwindHandle),
    Symbol(Symbol),
    HashMap(#[try_hash] HashMapValue),
    Ref(PlaceRef),
}

impl SubstituteBindings for ValueShape {
    type Target = inference::MaybeNotInferred<ValueShape>;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        let result = match self {
            ValueShape::Unit
            | ValueShape::Bool(_)
            | ValueShape::Int32(_)
            | ValueShape::Int64(_)
            | ValueShape::Float64(_)
            | ValueShape::Char(_)
            | ValueShape::String(_)
            | ValueShape::List(_)
            | ValueShape::Tuple(_)
            | ValueShape::Function(_)
            | ValueShape::Template(_)
            | ValueShape::Macro(_)
            | ValueShape::NativeFunction(_)
            | ValueShape::Variant(_)
            | ValueShape::Multiset(_)
            | ValueShape::Contexts(_)
            | ValueShape::Ast(_)
            | ValueShape::Expr(_)
            | ValueShape::SyntaxModule(_)
            | ValueShape::SyntaxDefinition(_)
            | ValueShape::UnwindHandle(_)
            | ValueShape::Symbol(_)
            | ValueShape::HashMap(_)
            | ValueShape::Ref(_) => self,
            ValueShape::Type(ty) => ValueShape::Type(ty.substitute_bindings(kast, cache)),
            ValueShape::Binding(ref binding) => {
                match kast.scopes.interpreter.get(&binding.symbol) {
                    Some(value) => return value.clone_value().unwrap().r#impl.into(),
                    None => self,
                }
            }
        };
        result.into()
    }
}

#[derive(Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TupleValue {
    #[try_hash]
    pub name: Option<Name>,
    #[try_hash]
    pub inner: Tuple<OwnedPlace>,
    #[try_hash]
    pub ty: Type,
}

impl std::ops::Deref for TupleValue {
    type Target = Tuple<OwnedPlace>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl TupleValue {
    pub fn new(name: Name, values: Tuple<Value>, ty: Type) -> Self {
        Self::new_impl(Some(name), values, ty)
    }
    fn new_impl(name: Option<Name>, values: Tuple<Value>, ty: Type) -> Self {
        Self {
            name,
            inner: values.map(|value| OwnedPlace::new(value, Mutability::Nested)),
            ty,
        }
    }
    pub fn new_unnamed(values: Tuple<Value>, ty: Type) -> Self {
        Self::new_impl(None, values, ty)
    }
    pub fn into_values(self) -> Tuple<Value> {
        self.inner.map(|place| place.into_value().unwrap())
    }
    pub fn values(&self) -> eyre::Result<Tuple<PlaceReadGuard<'_, Value>>> {
        self.inner.as_ref().try_map(|place| place.read_value())
    }
}

impl std::fmt::Display for TupleValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl Kast {
    pub fn is_copy(&self, ty: &Type) -> eyre::Result<bool> {
        if let Ok(ty) = ty.inferred() {
            let is_copy: Option<bool> = match ty {
                TypeShape::Unit
                | TypeShape::Bool
                | TypeShape::Int32
                | TypeShape::Int64
                | TypeShape::Float64
                | TypeShape::Type
                | TypeShape::Ref(_)
                | TypeShape::Symbol
                | TypeShape::Char => Some(true),
                TypeShape::UnwindHandle(_) => Some(true),
                TypeShape::String | TypeShape::List(_) | TypeShape::HashMap(_) => Some(false),
                TypeShape::Variant(_) => None,
                TypeShape::Tuple(tuple) => {
                    for field in tuple.fields.values() {
                        if !self.is_copy(field)? {
                            return Ok(false);
                        }
                    }
                    Some(true)
                }
                TypeShape::Function(_) => Some(true),
                TypeShape::Template(_) => Some(true),
                TypeShape::Macro(_) => Some(true),
                TypeShape::Multiset => None,
                TypeShape::Contexts => None,
                TypeShape::Ast => None,
                TypeShape::Expr => None,
                TypeShape::SyntaxModule => None,
                TypeShape::SyntaxDefinition => None,
                TypeShape::Binding(_) => None,
                TypeShape::NewType { .. } => None,
            };
            if let Some(result) = is_copy {
                return Ok(result);
            }
        }
        // this part is kinda like redstone
        let copy_trait = self.cache.interpreter.natives.get_named(
            self.current_name.clone(),
            "Copy",
            Type::new_not_inferred("copy"),
        )?;
        let Some(copy_trait) = copy_trait else {
            // Before Copy is defined everything is Copy
            // TODO maybe not needed because of logic above?
            return Ok(true);
        };
        Ok(self
            .cache
            .compiler
            .casts
            .lock()
            .unwrap()
            .cast(ValueShape::Type(ty.clone()).into(), &copy_trait)?
            .is_ok())
    }
}

// TODO not public?
#[derive(Debug, Clone, PartialEq, Eq, TryHash, Ord, PartialOrd)]
pub enum ValueImpl {
    Inferrable(#[try_hash] inference::MaybeNotInferred<ValueShape>),
    NonInferrable(#[try_hash] ValueShape),
}

impl From<ValueImpl> for inference::MaybeNotInferred<ValueShape> {
    fn from(value: ValueImpl) -> Self {
        match value {
            ValueImpl::Inferrable(var) => var,
            ValueImpl::NonInferrable(value) => value.into(),
        }
    }
}

impl SubstituteBindings for ValueImpl {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        match self {
            Self::NonInferrable(value) => Self::Inferrable(value.substitute_bindings(kast, cache)),
            Self::Inferrable(var) => Self::Inferrable(var.substitute_bindings(kast, cache)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct Value {
    #[try_hash]
    pub r#impl: ValueImpl, // TODO not public
    #[try_hash]
    pub ty: Type,
}

impl SubstituteBindings for Value {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        Self {
            r#impl: self.r#impl.substitute_bindings(kast, cache),
            ty: self.ty.substitute_bindings(kast, cache),
        }
    }
}

impl Value {
    pub fn new_not_inferred(description: &str) -> Self {
        Self::new_not_inferred_of_ty(
            description,
            Type::new_not_inferred(&format!("type of: {description}")),
        )
    }
    pub fn new_not_inferred_of_ty(description: &str, ty: Type) -> Self {
        Self {
            r#impl: ValueImpl::Inferrable(inference::MaybeNotInferred::new_not_inferred(
                description,
            )),
            ty,
        }
        .init()
        .unwrap()
    }

    pub fn mutate(&mut self) -> eyre::Result<&mut ValueShape> {
        match &mut self.r#impl {
            ValueImpl::Inferrable(_) => eyre::bail!("trying to mutate inferrable"),
            ValueImpl::NonInferrable(value) => Ok(value),
        }
    }

    fn init(self) -> eyre::Result<Self> {
        // println!("initializing {self:?}");
        match &self.r#impl {
            ValueImpl::NonInferrable(_) => {}
            ValueImpl::Inferrable(inferrable) => {
                let inferrable = inferrable.clone();
                self.ty.var().add_check(move |ty| {
                    if let Some(shape) = ty.infer_value_shape() {
                        inferrable.infer_as(shape)?;
                    }
                    Ok(inference::CheckResult::Completed)
                })?;
            }
        }
        // println!("done");
        Ok(self)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct HashableValue(pub Value);

impl std::hash::Hash for HashableValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.try_hash(state).expect("failed to hash");
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct HashMapValue {
    pub values: HashMap<HashableValue, OwnedPlace>,
    pub ty: HashMapType,
}

impl PartialOrd for HashMapValue {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!("Maybe not do?")
    }
}
impl Ord for HashMapValue {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl TryHash for HashMapValue {
    type Error = eyre::Report;

    fn try_hash(&self, _hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        eyre::bail!("hashmaps are not hashable")
    }
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct ListValue {
    #[try_hash]
    pub values: Vec<OwnedPlace>,
    #[try_hash]
    pub element_ty: Type,
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct UnwindHandle {
    pub sender: Parc<Mutex<async_oneshot::Sender<Value>>>,
    #[try_hash]
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct VariantValue {
    pub name: String,
    #[try_hash]
    pub value: Option<OwnedPlace>,
    #[try_hash]
    pub ty: Type,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.r#impl {
            ValueImpl::Inferrable(value) => value.fmt(f),
            ValueImpl::NonInferrable(value) => value.fmt(f),
        }
    }
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

impl std::fmt::Display for ListValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (index, value) in self.values.iter().enumerate() {
            if index != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{value}")?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl std::fmt::Display for ValueShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueShape::Unit => write!(f, "()"),
            ValueShape::Variant(value) => write!(f, "{value}"),
            ValueShape::Bool(value) => value.fmt(f),
            ValueShape::Int32(value) => value.fmt(f),
            ValueShape::Int64(value) => value.fmt(f),
            ValueShape::Float64(value) => value.fmt(f),
            ValueShape::Char(c) => write!(f, "{c:?}"),
            ValueShape::String(s) => write!(f, "{s:?}"),
            ValueShape::List(list) => list.fmt(f),
            ValueShape::Multiset(values) => {
                for (index, value) in values.iter().enumerate() {
                    if index != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "| ")?;
                    write!(f, "{value}")?;
                }
                Ok(())
            }
            ValueShape::Contexts(contexts) => contexts.fmt(f),
            ValueShape::Tuple(tuple) => tuple.fmt(f),
            ValueShape::NativeFunction(function) => function.fmt(f),
            ValueShape::Binding(binding) => binding.fmt(f),
            ValueShape::Function(fun) => write!(f, "{fun}"),
            ValueShape::Template(fun) => write!(f, "{fun}"),
            ValueShape::Macro(fun) => write!(f, "macro {fun}"),
            ValueShape::Ast(ast) => {
                write!(f, "{ast}")?;
                if let Some(scope) = &ast.data().def_site {
                    write!(f, " with def site id={}", scope.id())?;
                }
                Ok(())
            }
            ValueShape::Expr(expr) => {
                write!(f, "{expr}")?;
                Ok(())
            }
            ValueShape::Type(ty) => {
                // write!(f, "type ")?;
                ty.fmt(f)
            }
            ValueShape::SyntaxModule(_definitions) => write!(f, "<syntax module>"),
            ValueShape::SyntaxDefinition(_definition) => write!(f, "<syntax definition>"),
            ValueShape::UnwindHandle(_) => write!(f, "<unwind handle>"),
            ValueShape::Symbol(symbol) => write!(f, "symbol {symbol}"),
            ValueShape::HashMap(map) => map.fmt(f),
            // copying strings will not work, ever
            ValueShape::Ref(r) => {
                write!(f, "&")?;
                r.read().expect("failed to read for Display &").fmt(f)
            }
        }
    }
}

impl std::fmt::Display for HashMapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HashMap[")?;
        for (index, (key, value)) in self.values.iter().enumerate() {
            if index != 0 {
                write!(f, ", ")?;
            }
            let HashableValue(key) = key;
            write!(f, "{key} = {value}")?;
        }
        write!(f, "]")
    }
}

impl std::fmt::Debug for ValueShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl Value {
    /// Get this value AS a type
    pub fn into_type(self) -> eyre::Result<Type> {
        self.ty.infer_as(TypeShape::Type)?;
        Ok(match self.r#impl {
            ValueImpl::Inferrable(inferrable) => match inferrable.inferred() {
                Ok(inferred) => inferred.into_type()?,
                Err(_var) => {
                    unreachable!()
                }
            },
            ValueImpl::NonInferrable(value) => value.into_type()?,
        })
    }
    pub fn into_inferred(self) -> eyre::Result<ValueShape> {
        match self.r#impl {
            ValueImpl::Inferrable(inferrable) => inferrable.expect_inferred(),
            ValueImpl::NonInferrable(value) => Ok(value),
        }
    }
    pub fn as_inferred(&self) -> eyre::Result<MaybeBorrowed<'_, ValueShape>> {
        Ok(match &self.r#impl {
            ValueImpl::Inferrable(inferrable) => {
                MaybeBorrowed::Owned(inferrable.expect_inferred()?)
            }
            ValueImpl::NonInferrable(value) => MaybeBorrowed::Borrowed(value),
        })
    }
}

impl From<ValueShape> for Value {
    fn from(value: ValueShape) -> Self {
        Value {
            ty: value.ty(),
            r#impl: ValueImpl::NonInferrable(value),
        }
        .init()
        .unwrap()
    }
}

impl ValueShape {
    /// Get this value AS a type
    pub fn into_type(self) -> Result<Type, ExpectError> {
        match self {
            Self::Binding(binding) => {
                binding.ty.infer_as(TypeShape::Type).unwrap(); // TODO dont unwrap
                Ok(TypeShape::Binding(binding).into())
            }
            Self::Type(ty) => Ok(ty),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Type,
            }),
        }
    }
    pub fn ty(&self) -> Type {
        match self {
            ValueShape::Unit => TypeShape::Unit.into(),
            ValueShape::Multiset(_) => TypeShape::Multiset.into(),
            ValueShape::Contexts(_) => TypeShape::Contexts.into(),
            ValueShape::Variant(value) => value.ty.clone(),
            ValueShape::Bool(_) => TypeShape::Bool.into(),
            ValueShape::Int32(_) => TypeShape::Int32.into(),
            ValueShape::Int64(_) => TypeShape::Int64.into(),
            ValueShape::Float64(_) => TypeShape::Float64.into(),
            ValueShape::Char(_) => TypeShape::Char.into(),
            ValueShape::String(_) => TypeShape::String.into(),
            ValueShape::List(list) => TypeShape::List(list.element_ty.clone()).into(),
            ValueShape::Tuple(tuple) => tuple.ty.clone(),
            ValueShape::Binding(binding) => binding.ty.clone(), // TODO not sure, maybe Type::Binding?
            ValueShape::Function(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            ValueShape::Template(t) => TypeShape::Template(t.compiled.clone()).into(),
            ValueShape::Macro(f) => TypeShape::Macro(Box::new(f.ty.clone())).into(),
            ValueShape::NativeFunction(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            ValueShape::Ast(_) => TypeShape::Ast.into(),
            ValueShape::Expr(_) => TypeShape::Expr.into(),
            ValueShape::Type(_) => TypeShape::Type.into(),
            ValueShape::SyntaxModule(_) => TypeShape::SyntaxModule.into(),
            ValueShape::SyntaxDefinition(_) => TypeShape::SyntaxDefinition.into(),
            ValueShape::UnwindHandle(handle) => TypeShape::UnwindHandle(handle.ty.clone()).into(),
            ValueShape::Symbol(_) => TypeShape::Symbol.into(),
            ValueShape::HashMap(map) => TypeShape::HashMap(map.ty.clone()).into(),
            ValueShape::Ref(r) => TypeShape::Ref(r.place.ty.clone()).into(),
        }
    }
}

impl Value {
    pub fn inferred(self) -> Option<ValueShape> {
        match self.r#impl {
            ValueImpl::Inferrable(inferrable) => inferrable.inferred().ok(),
            ValueImpl::NonInferrable(value) => Some(value),
        }
    }
    /// Get the type OF this value
    pub fn ty(&self) -> Type {
        self.ty.clone()
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{value} is not {expected}")]
pub struct ExpectError<V = ValueShape, Expected = TypeShape> {
    pub value: V,
    pub expected: Expected,
}

impl ValueShape {
    pub fn into_unwind_handle(self) -> Result<UnwindHandle, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::UnwindHandle(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "unwind handle",
            }),
        }
    }
    pub fn into_macro(self) -> Result<TypedFunction, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Macro(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "macro",
            }),
        }
    }
    pub fn as_variant(&self) -> Result<&VariantValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Variant(value) => Ok(value),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "variant",
            }),
        }
    }
    pub fn into_variant(self) -> Result<VariantValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Variant(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "variant",
            }),
        }
    }
    pub fn as_hash_map(&self) -> Result<&HashMapValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::HashMap(map) => Ok(map),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "HashMap",
            }),
        }
    }
    pub fn as_hash_map_mut(
        &mut self,
    ) -> Result<&mut HashMapValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::HashMap(map) => Ok(map),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "HashMap",
            }),
        }
    }
    pub fn into_hash_map(self) -> Result<HashMapValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::HashMap(map) => Ok(map),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "HashMap",
            }),
        }
    }
    pub fn as_tuple(&self) -> Result<&TupleValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "tuple",
            }),
        }
    }

    pub fn into_contexts(self) -> eyre::Result<Contexts> {
        match self {
            Self::Unit => Ok(Contexts::empty()),
            Self::Contexts(contexts) => Ok(contexts),
            Self::Type(ty) => Ok(Contexts::from_list([ty], false)),
            Self::Multiset(set) => Ok(Contexts::from_list(
                set.into_iter()
                    .map(|value| value.into_type())
                    .collect::<Result<Vec<_>, eyre::Report>>()?,
                false,
            )),
            Self::Binding(binding) => Ok(Contexts::from_list(
                [TypeShape::Binding(binding).into()],
                false,
            )),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Contexts,
            }
            .into()),
        }
    }
    pub fn into_syntax_definition(self) -> Result<Parc<ast::SyntaxDefinition>, ExpectError> {
        match self {
            Self::SyntaxDefinition(def) => Ok(def),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::SyntaxModule,
            }),
        }
    }
    pub fn into_syntax_module(self) -> Result<Parc<Vec<Parc<ast::SyntaxDefinition>>>, ExpectError> {
        match self {
            Self::SyntaxModule(syntax) => Ok(syntax),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::SyntaxModule,
            }),
        }
    }
    pub fn into_unit(self) -> Result<(), ExpectError> {
        match self {
            Self::Unit => Ok(()),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Unit,
            }),
        }
    }
    pub fn as_list(&self) -> Result<&ListValue, ExpectError> {
        match self {
            Self::List(list) => Ok(list),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: TypeShape::List(Type::new_not_inferred("whatever")),
            }),
        }
    }
    pub fn as_list_mut(&mut self) -> Result<&mut ListValue, ExpectError> {
        match self {
            Self::List(list) => Ok(list),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: TypeShape::List(Type::new_not_inferred("whatever")),
            }),
        }
    }
    pub fn into_list(self) -> Result<ListValue, ExpectError> {
        match self {
            Self::List(list) => Ok(list),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::List(Type::new_not_inferred("whatever")),
            }),
        }
    }
    pub fn into_char(self) -> Result<char, ExpectError> {
        match self {
            Self::Char(c) => Ok(c),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Char,
            }),
        }
    }
    pub fn as_str(&self) -> Result<&str, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: TypeShape::String,
            }),
        }
    }
    pub fn into_string(self) -> Result<String, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::String,
            }),
        }
    }
    pub fn into_int32(self) -> Result<i32, ExpectError> {
        match self {
            Self::Int32(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Int32,
            }),
        }
    }
    pub fn into_int64(self) -> Result<i64, ExpectError> {
        match self {
            Self::Int64(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Int64,
            }),
        }
    }
    pub fn into_float64(self) -> Result<OrderedFloat<f64>, ExpectError> {
        match self {
            Self::Float64(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Float64,
            }),
        }
    }
    pub fn as_ref(&self) -> Result<&PlaceRef, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Ref(r) => Ok(r),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "reference",
            }),
        }
    }
    pub fn into_ref(self) -> Result<PlaceRef, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Ref(r) => Ok(r),
            _ => Err(ExpectError {
                value: self,
                expected: "reference",
            }),
        }
    }
    pub fn into_bool(self) -> Result<bool, ExpectError> {
        match self {
            Self::Bool(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Bool,
            }),
        }
    }
    pub fn into_ast(self) -> Result<Ast, ExpectError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Ast,
            }),
        }
    }
    pub fn into_function(self) -> Result<TypedFunction, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Function(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: "function",
            }),
        }
    }
    pub fn into_template(self) -> Result<Function, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Template(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: "template",
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct TypedFunction {
    #[try_hash]
    pub ty: FnType,
    #[try_hash]
    pub f: Function,
}

impl std::fmt::Display for TypedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.f.fmt(f)
    }
}

impl std::ops::Deref for TypedFunction {
    type Target = Function;
    fn deref(&self) -> &Self::Target {
        &self.f
    }
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct Function {
    pub id: Id,
    pub span: Span,
    #[try_hash]
    pub name: Name,
    pub captured: Parc<Scopes>,
    pub compiled: MaybeCompiledFn,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{} at {}", self.name, self.span)
        write!(f, "{}", self.name)
    }
}

pub trait NativeFunctionClosure: Send + Sync + 'static {
    fn call(
        &self,
        kast: Kast,
        fn_type: FnType,
        arg: Value,
    ) -> BoxFuture<'static, eyre::Result<Value>>;
}

impl<F: Fn(Kast, FnType, Value) -> BoxFuture<'static, eyre::Result<Value>> + Send + Sync + 'static>
    NativeFunctionClosure for F
{
    fn call(
        &self,
        kast: Kast,
        fn_type: FnType,
        arg: Value,
    ) -> BoxFuture<'static, eyre::Result<Value>> {
        self(kast, fn_type, arg)
    }
}

#[derive(Clone, PartialEq, Eq, TryHash, PartialOrd, Ord)]
pub struct NativeFunction {
    pub native_name: String,
    #[try_hash]
    pub name: Name,
    #[try_hash]
    pub ty: FnType,
    pub r#impl: Parc<dyn NativeFunctionClosure>,
}

impl NativeFunction {
    pub fn new(
        name: Name,
        native_name: impl Into<String>,
        ty: FnType,
        r#impl: impl NativeFunctionClosure,
    ) -> Self {
        Self {
            name,
            native_name: native_name.into(),
            ty,
            r#impl: (std::sync::Arc::new(r#impl) as std::sync::Arc<dyn NativeFunctionClosure>)
                .into(),
        }
    }
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn {:?} (aka {})>", self.native_name, self.name)
    }
}

impl std::fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Inferrable for ListValue {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        if a.values.len() != b.values.len() {
            eyre::bail!("list length differ");
        }
        let element_ty = Inferrable::make_same(a.element_ty, b.element_ty)?;
        Ok(Self {
            values: a
                .values
                .into_iter()
                .zip(b.values.into_iter())
                .map(|(a, b)| {
                    Ok::<_, eyre::Report>(OwnedPlace::new(
                        Inferrable::make_same(a.into_value()?, b.into_value()?)?,
                        Mutability::Nested,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?,
            element_ty,
        })
    }
}

impl Inferrable for Value {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Self {
            r#impl: ValueImpl::Inferrable(match (a.r#impl, b.r#impl) {
                (ValueImpl::Inferrable(a), ValueImpl::Inferrable(b)) => {
                    Inferrable::make_same(a, b)?
                }
                (ValueImpl::Inferrable(a), ValueImpl::NonInferrable(b)) => {
                    a.infer_as(b)?;
                    a
                }
                (ValueImpl::NonInferrable(a), ValueImpl::Inferrable(b)) => {
                    b.infer_as(a)?;
                    b
                }
                (ValueImpl::NonInferrable(_), ValueImpl::NonInferrable(_)) => {
                    eyre::bail!("inferring non inferrables???")
                }
            }),
            ty: Inferrable::make_same(a.ty, b.ty)?,
        })
    }
}

impl Inferrable for ValueShape {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        macro_rules! fail {
            () => {
                eyre::bail!("expected value {a}, got {b}")
            };
        }
        Ok(match (a.clone(), b.clone()) {
            (ValueShape::Binding(binding), ValueShape::Type(ty))
            | (ValueShape::Type(ty), ValueShape::Binding(binding)) => match ty.inferred() {
                Err(_) => ValueShape::Type(TypeShape::Binding(binding).into()),
                Ok(_) => eyre::bail!("{binding} inferred as type {ty}????"),
            },

            (ValueShape::Unit, ValueShape::Unit) => ValueShape::Unit,
            (ValueShape::Unit, _) => fail!(),
            (ValueShape::Bool(a), ValueShape::Bool(b)) if a == b => ValueShape::Bool(a),
            (ValueShape::Bool(_), _) => fail!(),
            (ValueShape::Int32(a), ValueShape::Int32(b)) if a == b => ValueShape::Int32(a),
            (ValueShape::Int32(_), _) => fail!(),
            (ValueShape::Int64(a), ValueShape::Int64(b)) if a == b => ValueShape::Int64(a),
            (ValueShape::Int64(_), _) => fail!(),
            (ValueShape::Float64(a), ValueShape::Float64(b)) if a == b => ValueShape::Float64(a),
            (ValueShape::Float64(_), _) => fail!(),
            (ValueShape::Char(a), ValueShape::Char(b)) if a == b => ValueShape::Char(a),
            (ValueShape::Char(_), _) => fail!(),
            (ValueShape::String(a), ValueShape::String(b)) if a == b => ValueShape::String(a),
            (ValueShape::String(_), _) => fail!(),
            (ValueShape::List(a), ValueShape::List(b)) => {
                ValueShape::List(Inferrable::make_same(a, b)?)
            }
            (ValueShape::List(_), _) => fail!(),
            (ValueShape::Tuple(a), ValueShape::Tuple(b)) => {
                let ty = Inferrable::make_same(a.ty.clone(), b.ty.clone())?;
                let mut result = Tuple::empty();
                for (member, (a, b)) in a.into_values().zip(b.into_values())?.into_iter() {
                    let value = Inferrable::make_same(a, b)?;
                    result.add_member(member, value);
                }
                // TODO maybe named?
                ValueShape::Tuple(TupleValue::new_unnamed(result, ty))
            }
            (ValueShape::Tuple(_), _) => fail!(),
            (ValueShape::Function(_), _) => fail!(),
            (ValueShape::Template(_), _) => fail!(),
            (ValueShape::Macro(_), _) => fail!(),
            (ValueShape::NativeFunction(_), _) => fail!(),
            (ValueShape::Binding(_), _) => fail!(),
            (ValueShape::Variant(_), _) => fail!(),
            (ValueShape::Multiset(_), _) => fail!(),
            (ValueShape::Contexts(_), _) => fail!(),
            (ValueShape::Ast(_), _) => fail!(),
            (ValueShape::Expr(_), _) => fail!(),
            (ValueShape::Type(a), ValueShape::Type(b)) => {
                ValueShape::Type(Inferrable::make_same(a, b)?)
            }
            (ValueShape::Type(_), _) => fail!(),
            (ValueShape::SyntaxModule(_), _) => fail!(),
            (ValueShape::SyntaxDefinition(_), _) => fail!(),
            (ValueShape::UnwindHandle(_), _) => fail!(),
            (ValueShape::Symbol(_), _) => fail!(),
            (ValueShape::HashMap(_), _) => fail!(),
            (ValueShape::Ref(a), ValueShape::Ref(b)) if a == b => ValueShape::Ref(a),
            (ValueShape::Ref(_), _) => fail!(),
        })
    }
}

impl TypeShape {
    pub fn infer_value_shape(&self) -> Option<ValueShape> {
        Some(match self {
            TypeShape::Unit => ValueShape::Unit,
            TypeShape::Bool => return None,
            TypeShape::Int32 => return None,
            TypeShape::Int64 => return None,
            TypeShape::Float64 => return None,
            TypeShape::Char => return None,
            TypeShape::String => return None,
            TypeShape::List(_) => return None,
            TypeShape::Variant(_) => return None,
            TypeShape::Tuple(tuple) => {
                // TODO maybe named?
                ValueShape::Tuple(TupleValue::new_unnamed(
                    tuple
                        .fields
                        .clone()
                        .map(|ty| Value::new_not_inferred_of_ty("inferred field value shape", ty)),
                    self.clone().into(),
                ))
            }
            TypeShape::Function(_) => return None,
            TypeShape::Template(_) => return None,
            TypeShape::Macro(_) => return None,
            TypeShape::Multiset => return None,
            TypeShape::Contexts => return None,
            TypeShape::Ast => return None,
            TypeShape::Expr => return None,
            TypeShape::Type => {
                ValueShape::Type(Type::new_not_inferred("inferred value shape for type"))
            }
            TypeShape::SyntaxModule => return None,
            TypeShape::SyntaxDefinition => return None,
            TypeShape::UnwindHandle(_) => return None,
            TypeShape::Binding(_) => return None,
            TypeShape::Symbol => return None,
            TypeShape::HashMap(_) => return None,
            TypeShape::Ref(_) => return None,
            TypeShape::NewType { .. } => return None,
        })
    }
}
