use super::*;

#[derive(Clone, PartialEq, Eq, TryHash)]
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
    Template(Function),
    Macro(#[try_hash] TypedFunction),
    NativeFunction(#[try_hash] NativeFunction),
    Binding(Parc<Binding>),
    Variant(#[try_hash] VariantValue),
    Multiset(#[try_hash] Vec<Value>),
    Contexts(#[try_hash] Contexts),
    Ast(Ast),
    Type(#[try_hash] Type),
    SyntaxModule(Parc<Vec<Parc<ast::SyntaxDefinition>>>),
    SyntaxDefinition(Parc<ast::SyntaxDefinition>),
    UnwindHandle(#[try_hash] UnwindHandle),
    Symbol(Symbol),
    HashMap(#[try_hash] HashMapValue),
    Ref(PlaceRef),
}

#[derive(Clone, TryHash, PartialEq, Eq)]
pub struct TupleValue(#[try_hash] pub Tuple<OwnedPlace>);

impl std::ops::Deref for TupleValue {
    type Target = Tuple<OwnedPlace>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Tuple<Value>> for TupleValue {
    fn from(values: Tuple<Value>) -> Self {
        Self(values.map(OwnedPlace::new))
    }
}

impl TupleValue {
    pub fn new(values: Tuple<Value>) -> Self {
        values.into()
    }
    pub fn into_values(self) -> Tuple<Value> {
        self.0.map(|place| place.into_value().unwrap())
    }
}

impl std::fmt::Display for TupleValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

// TODO impl Drop
pub struct OwnedPlace(Parc<Place>);

impl std::ops::Deref for OwnedPlace {
    type Target = Place;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for OwnedPlace {
    fn eq(&self, other: &Self) -> bool {
        *self.0.state.lock().unwrap() == *other.0.state.lock().unwrap()
    }
}

impl Eq for OwnedPlace {}

impl TryHash for OwnedPlace {
    type Error = <PlaceState as TryHash>::Error;
    fn try_hash(&self, state: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        self.0.state.lock().unwrap().try_hash(state)
    }
}

impl Clone for OwnedPlace {
    fn clone(&self) -> Self {
        Self(Parc::new(Place::new_state(
            self.0.state.lock().unwrap().clone(),
        )))
    }
}

impl std::fmt::Display for OwnedPlace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl OwnedPlace {
    pub fn new(value: Value) -> Self {
        Self::new_impl(value.ty(), PlaceState::Occupied(value))
    }
    fn new_impl(ty: Type, inner: PlaceState) -> Self {
        Self(Parc::new(Place {
            id: Id::new(),
            ty,
            state: Mutex::new(inner),
        }))
    }
    pub fn new_uninitialized(ty: Type) -> Self {
        Self::new_impl(ty, PlaceState::Unintialized)
    }
    pub fn into_value(self) -> Result<Value, PlaceError> {
        self.0.take_value()
    }
    pub fn get_ref(&self) -> PlaceRef {
        PlaceRef {
            place: self.0.clone(),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct PlaceRef {
    pub place: Parc<Place>,
}

impl PlaceRef {
    pub fn new_temp(value: Value) -> Self {
        Self {
            place: Parc::new(Place::new(value)),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum PlaceError {
    #[error("place is uninitialized")]
    Unintialized,
    #[error("place has been moved out")]
    MovedOut,
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
                TypeShape::String | TypeShape::List(_) | TypeShape::HashMap(_) => Some(false),
                TypeShape::Variant(_) => None,
                TypeShape::Tuple(tuple) => {
                    for field in tuple.values() {
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
                TypeShape::SyntaxModule => None,
                TypeShape::SyntaxDefinition => None,
                TypeShape::UnwindHandle(_) => None,
                TypeShape::Binding(_) => None,
                TypeShape::NewType { .. } => None,
            };
            if let Some(result) = is_copy {
                return Ok(result);
            }
        }
        // this part is kinda like redstone
        let copy_trait = self
            .cache
            .interpreter
            .set_natives
            .lock()
            .unwrap()
            .get("Copy")
            .cloned();
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

// nothing is going to work
impl PlaceRef {
    pub fn inspect<R>(&self, f: impl FnOnce(&Value) -> R) -> Result<R, PlaceError> {
        self.place.inspect(f)
    }
    pub fn claim_value(&self, kast: &Kast) -> eyre::Result<Value> {
        if let Some(cloned_inferrable) = self.place.inspect(|value| match value.r#impl {
            ValueImpl::Inferrable(_) => Some(value.clone()),
            ValueImpl::NonInferrable(_) => None,
        })? {
            return Ok(cloned_inferrable);
        };
        Ok(if kast.is_copy(&self.place.ty)? {
            self.place.clone_value()?
        } else {
            self.place.take_value()?
        })
    }
    pub fn clone_value(&self) -> Result<Value, PlaceError> {
        self.place.clone_value()
    }
    pub fn assign(&self, new_value: Value) {
        self.place.assign(new_value)
    }
    pub fn mutate<R>(&self, f: impl FnOnce(&mut ValueShape) -> R) -> eyre::Result<R> {
        self.place.mutate(f)
    }
}

impl std::fmt::Display for PlaceRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{}", self.place.id)
    }
}

pub struct Place {
    pub id: Id,
    pub ty: Type,
    pub state: Mutex<PlaceState>,
}

// this wouldnt be so much code if i would code it in assembly
impl Place {
    pub fn new(value: Value) -> Self {
        Self::new_state(PlaceState::Occupied(value))
    }
    pub fn new_state(state: PlaceState) -> Self {
        Self {
            id: Id::new(),
            ty: state
                .ty()
                .unwrap_or_else(|| Type::new_not_inferred("place type")),
            state: Mutex::new(state),
        }
    }
    pub fn inspect<R>(&self, f: impl FnOnce(&Value) -> R) -> Result<R, PlaceError> {
        Ok(f(self.state.lock().unwrap().get()?))
    }
    pub fn take_value(&self) -> Result<Value, PlaceError> {
        Ok(self.state.lock().unwrap().take()?.clone())
    }
    pub fn clone_value(&self) -> Result<Value, PlaceError> {
        Ok(self.state.lock().unwrap().get()?.clone())
    }
    pub fn assign(&self, new_value: Value) {
        self.state.lock().unwrap().assign(new_value)
    }
    pub fn mutate<R>(&self, f: impl FnOnce(&mut ValueShape) -> R) -> eyre::Result<R> {
        Ok(f(self.state.lock().unwrap().get_mut()?))
    }
}

impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.state.lock().unwrap() {
            PlaceState::Unintialized => write!(f, "<uninitialized>"),
            PlaceState::Occupied(value) => value.fmt(f),
            PlaceState::MovedOut => write!(f, "<moved out>"),
        }
    }
}

#[derive(Clone, PartialEq, TryHash)]
pub enum PlaceState {
    Unintialized,
    // This place is taken
    Occupied(#[try_hash] Value),
    MovedOut,
}

impl PlaceState {
    pub fn ty(&self) -> Option<Type> {
        match self {
            PlaceState::Occupied(value) => Some(value.ty()),
            PlaceState::Unintialized | PlaceState::MovedOut => None,
        }
    }
    pub fn assign(&mut self, new_value: Value) {
        *self = Self::Occupied(new_value);
    }
    pub fn get(&self) -> Result<&Value, PlaceError> {
        match self {
            PlaceState::Unintialized => Err(PlaceError::Unintialized),
            PlaceState::Occupied(value) => Ok(value),
            PlaceState::MovedOut => Err(PlaceError::MovedOut),
        }
    }
    pub fn get_mut(&mut self) -> eyre::Result<&mut ValueShape> {
        match self {
            PlaceState::Unintialized => Err(PlaceError::Unintialized)?,
            PlaceState::Occupied(value) => Ok(value.mutate()?),
            PlaceState::MovedOut => Err(PlaceError::MovedOut)?,
        }
    }
    pub fn take(&mut self) -> Result<Value, PlaceError> {
        match self {
            PlaceState::Unintialized => Err(PlaceError::Unintialized),
            PlaceState::MovedOut => Err(PlaceError::MovedOut),
            PlaceState::Occupied(_) => {
                let PlaceState::Occupied(value) = std::mem::replace(self, PlaceState::MovedOut)
                else {
                    unreachable!()
                };
                Ok(value)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
enum ValueImpl {
    Inferrable(#[try_hash] inference::MaybeNotInferred<ValueShape>),
    NonInferrable(#[try_hash] ValueShape),
}

#[derive(Debug, Clone, PartialEq, Eq, TryHash)]
pub struct Value {
    #[try_hash]
    r#impl: ValueImpl,
    #[try_hash]
    pub ty: Type,
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

impl TryHash for HashMapValue {
    type Error = eyre::Report;

    fn try_hash(&self, _hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        eyre::bail!("hashmaps are not hashable")
    }
}

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct ListValue {
    #[try_hash]
    pub values: Vec<Value>,
    #[try_hash]
    pub element_ty: Type,
}

#[derive(Clone, PartialEq, Eq, TryHash)]
pub struct UnwindHandle {
    pub sender: Parc<Mutex<async_oneshot::Sender<Value>>>,
    #[try_hash]
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq, TryHash)]
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
            ValueShape::Function(_function) => write!(f, "<function>"),
            ValueShape::Template(_template) => write!(f, "<template>"),
            ValueShape::Macro(_macro) => write!(f, "<macro>"),
            ValueShape::Ast(ast) => {
                write!(f, "{ast}")?;
                if let Some(scope) = &ast.data().def_site {
                    write!(f, " with def site id={}", scope.id())?;
                }
                Ok(())
            }
            ValueShape::Type(ty) => {
                write!(f, "type ")?;
                ty.fmt(f)
            }
            ValueShape::SyntaxModule(_definitions) => write!(f, "<syntax module>"),
            ValueShape::SyntaxDefinition(_definition) => write!(f, "<syntax definition>"),
            ValueShape::UnwindHandle(_) => write!(f, "<unwind handle>"),
            ValueShape::Symbol(symbol) => write!(f, "symbol {symbol}"),
            ValueShape::HashMap(map) => map.fmt(f),
            ValueShape::Ref(r) => {
                write!(f, "&")?;
                r.inspect(|value| value.fmt(f)).expect("tried to fmt ref")
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
    pub fn expect_type(self) -> eyre::Result<Type> {
        self.ty.infer_as(TypeShape::Type)?;
        Ok(match self.r#impl {
            ValueImpl::Inferrable(inferrable) => match inferrable.inferred() {
                Ok(inferred) => inferred.expect_type()?,
                Err(_var) => {
                    unreachable!()
                }
            },
            ValueImpl::NonInferrable(value) => value.expect_type()?,
        })
    }
    pub fn expect_inferred(self) -> eyre::Result<ValueShape> {
        match self.r#impl {
            ValueImpl::Inferrable(inferrable) => inferrable.expect_inferred(),
            ValueImpl::NonInferrable(value) => Ok(value),
        }
    }
    pub fn expect_inferred_ref<R>(&self, f: impl FnOnce(&ValueShape) -> R) -> eyre::Result<R> {
        Ok(match &self.r#impl {
            ValueImpl::Inferrable(inferrable) => f(&inferrable.expect_inferred()?),
            ValueImpl::NonInferrable(value) => f(value),
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
    pub fn expect_type(self) -> Result<Type, ExpectError> {
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
            ValueShape::Tuple(tuple) => {
                TypeShape::Tuple(tuple.as_ref().map(|field| field.ty.clone())).into()
            }
            ValueShape::Binding(binding) => binding.ty.clone(), // TODO not sure, maybe Type::Binding?
            ValueShape::Function(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            ValueShape::Template(t) => TypeShape::Template(t.compiled.clone()).into(),
            ValueShape::Macro(f) => TypeShape::Macro(Box::new(f.ty.clone())).into(),
            ValueShape::NativeFunction(f) => TypeShape::Function(Box::new(f.ty.clone())).into(),
            ValueShape::Ast(_) => TypeShape::Ast.into(),
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
    pub fn expect_unwind_handle(
        self,
    ) -> Result<UnwindHandle, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::UnwindHandle(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "unwind handle",
            }),
        }
    }
    pub fn expect_macro(self) -> Result<TypedFunction, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Macro(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: "macro",
            }),
        }
    }
    pub fn expect_variant(&self) -> Result<&VariantValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Variant(value) => Ok(value),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "variant",
            }),
        }
    }
    pub fn expect_hash_map_mut(
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
    pub fn expect_hash_map(self) -> Result<HashMapValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::HashMap(map) => Ok(map),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "HashMap",
            }),
        }
    }
    pub fn expect_tuple(&self) -> Result<&TupleValue, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: "tuple",
            }),
        }
    }
    pub fn into_contexts(self) -> Result<Contexts, ExpectError> {
        match self {
            Self::Unit => Ok(Contexts::empty()),
            Self::Contexts(contexts) => Ok(contexts),
            Self::Type(ty) => Ok(Contexts::from_list([ty])),
            Self::Binding(binding) => Ok(Contexts::from_list([TypeShape::Binding(binding).into()])),
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
    pub fn expect_list_mut(&mut self) -> Result<&mut ListValue, ExpectError> {
        match self {
            Self::List(list) => Ok(list),
            _ => Err(ExpectError {
                value: self.clone(),
                expected: TypeShape::List(Type::new_not_inferred("whatever")),
            }),
        }
    }
    pub fn expect_list(self) -> Result<ListValue, ExpectError> {
        match self {
            Self::List(list) => Ok(list),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::List(Type::new_not_inferred("whatever")),
            }),
        }
    }
    pub fn expect_char(self) -> Result<char, ExpectError> {
        match self {
            Self::Char(c) => Ok(c),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Char,
            }),
        }
    }
    pub fn expect_string(&self) -> Result<&str, ExpectError> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(ExpectError {
                value: self.clone(),
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
    pub fn expect_float64(self) -> Result<OrderedFloat<f64>, ExpectError> {
        match self {
            Self::Float64(value) => Ok(value),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Float64,
            }),
        }
    }
    pub fn expect_ref(self) -> Result<PlaceRef, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Ref(r) => Ok(r),
            _ => Err(ExpectError {
                value: self,
                expected: "reference",
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
    pub fn expect_ast(self) -> Result<Ast, ExpectError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            _ => Err(ExpectError {
                value: self,
                expected: TypeShape::Ast,
            }),
        }
    }
    pub fn expect_function(self) -> Result<TypedFunction, ExpectError<ValueShape, &'static str>> {
        match self {
            Self::Function(f) => Ok(f),
            _ => Err(ExpectError {
                value: self,
                expected: "function",
            }),
        }
    }
    pub fn expect_template(self) -> Result<Function, ExpectError<ValueShape, &'static str>> {
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
    pub captured: Parc<Scopes>,
    pub compiled: MaybeCompiledFn,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {:?}>", self.id)
    }
}

pub type NativeFunctionImpl =
    dyn Fn(Kast, FnType, Value) -> BoxFuture<'static, eyre::Result<Value>> + Send + Sync;

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
                .map(|(a, b)| Inferrable::make_same(a, b))
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
                let mut result = Tuple::empty();
                for (name, (a, b)) in a.into_values().zip(b.into_values())?.into_iter() {
                    let value = Inferrable::make_same(a, b)?;
                    result.add(name, value);
                }
                ValueShape::Tuple(result.into())
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
            TypeShape::Tuple(tuple) => ValueShape::Tuple(
                tuple
                    .clone()
                    .map(|ty| Value::new_not_inferred_of_ty("inferred field value shape", ty))
                    .into(),
            ),
            TypeShape::Function(_) => return None,
            TypeShape::Template(_) => return None,
            TypeShape::Macro(_) => return None,
            TypeShape::Multiset => return None,
            TypeShape::Contexts => return None,
            TypeShape::Ast => return None,
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
