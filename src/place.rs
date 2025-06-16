use super::*;

// TODO impl Drop
pub struct OwnedPlace(Parc<Place>);

#[async_trait]
impl Inferrable for OwnedPlace {
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        self.0.read_value()?.await_fully_inferred(cache).await?;
        Ok(())
    }

    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let mutatibility = Inferrable::make_same(a.mutability, b.mutability)?;
        let value = Inferrable::make_same(a.into_value()?, b.into_value()?)?;
        Ok::<_, eyre::Report>(OwnedPlace::new(value, mutatibility))
    }
}

impl std::ops::Deref for OwnedPlace {
    type Target = Place;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for OwnedPlace {
    fn eq(&self, other: &Self) -> bool {
        *self.0.read().expect("can't read for eq") == *other.0.read().expect("can't read for eq")
    }
}

impl Eq for OwnedPlace {}

impl PartialOrd for OwnedPlace {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (*self.0.read().unwrap()).partial_cmp(&*other.0.read().unwrap())
    }
}

impl Ord for OwnedPlace {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self.0.read().unwrap()).cmp(&*other.0.read().unwrap())
    }
}

impl TryHash for OwnedPlace {
    type Error = <PlaceState as TryHash>::Error;
    fn try_hash(&self, state: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        self.0
            .read()
            .expect("can't read for hashing")
            .try_hash(state)
    }
}

impl Clone for OwnedPlace {
    fn clone(&self) -> Self {
        Self(Parc::new(Place::new(
            self.0.read().expect("can't read for clone").clone(),
            self.0.ty.clone(),
            self.0.mutability, // TODO is this correct?
        )))
    }
}

impl std::fmt::Display for OwnedPlace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl OwnedPlace {
    pub fn new(value: Value, mutability: Mutability) -> Self {
        let ty = value.ty();
        Self::new_impl(PlaceState::Occupied(value), ty, mutability)
    }
    pub fn new_temp(value: Value) -> Self {
        Self::new(value, Mutability::Mutable)
    }
    fn new_impl(state: PlaceState, ty: Type, mutability: Mutability) -> Self {
        Self(Parc::new(Place::new(state, ty, mutability)))
    }
    pub fn new_uninitialized(ty: Type, mutability: Mutability) -> Self {
        Self::new_impl(PlaceState::Unintialized, ty, mutability)
    }
    pub fn into_value(self) -> eyre::Result<Value> {
        self.0.take_value()
    }
    pub fn get_ref(&self) -> PlaceRef {
        PlaceRef {
            place: self.0.clone(),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlaceRef {
    pub place: Parc<Place>,
}

impl PlaceRef {
    pub fn new_temp(value: Value) -> Self {
        let ty = value.ty();
        Self {
            place: Parc::new(Place::new(
                PlaceState::Occupied(value),
                ty,
                Mutability::Mutable,
            )),
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

// nothing is going to work
impl PlaceRef {
    pub fn read(&self) -> eyre::Result<PlaceReadGuard<'_, PlaceState>> {
        self.place.read()
    }
    pub fn write(&self) -> eyre::Result<PlaceWriteGuard<'_, PlaceState>> {
        self.place.write()
    }
    pub fn read_value(&self) -> eyre::Result<PlaceReadGuard<'_, Value>> {
        self.place.read_value()
    }
    pub fn write_value(&self) -> eyre::Result<PlaceWriteGuard<'_, ValueShape>> {
        self.place.write_value()
    }
    pub fn claim_value(&self, kast: &Kast) -> eyre::Result<Value> {
        {
            let value = self.read_value()?;
            if let ValueImpl::Inferrable(_) = value.r#impl {
                return Ok(value.clone());
            }
        }
        Ok(if kast.is_copy(&self.place.ty)? {
            self.place.clone_value()?
        } else {
            self.place.take_value()?
        })
    }
    pub fn clone_value(&self) -> eyre::Result<Value> {
        self.place.clone_value()
    }
    pub fn assign(&self, new_value: Value) -> eyre::Result<()> {
        self.place.assign(new_value)
    }
}

impl std::fmt::Display for PlaceRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{}", self.place.id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mutability {
    ReadOnly,
    Mutable,
    Nested,
}

#[async_trait]
impl Inferrable for Mutability {
    async fn await_fully_inferred(&self, _cache: &mut RecurseCache) -> eyre::Result<()> {
        Ok(())
    }

    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        if a != b {
            eyre::bail!("{a:?} != {b:?}")
        }
        Ok(a)
    }
}

impl Mutability {
    pub fn check_can_read(&self) -> eyre::Result<()> {
        match self {
            Self::Nested => Ok(()),
            Self::ReadOnly | Self::Mutable => Ok(()),
        }
    }
    pub fn check_can_mutate(&self) -> eyre::Result<()> {
        match self {
            Self::Nested => Ok(()),
            Self::ReadOnly => Err(eyre!("place is readonly")),
            Self::Mutable => Ok(()),
        }
    }
}

pub struct Place {
    pub id: Id,
    pub ty: Type,
    pub mutability: Mutability,
    state: tokio::sync::RwLock<PlaceState>,
}

pub struct PlaceReadGuard<'a, T>(tokio::sync::RwLockReadGuard<'a, T>);

impl<T> std::ops::Deref for PlaceReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct PlaceWriteGuard<'a, T>(tokio::sync::RwLockMappedWriteGuard<'a, T>);

impl<T> std::ops::Deref for PlaceWriteGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::ops::DerefMut for PlaceWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// this wouldnt be so much code if i would code it in assembly
impl Place {
    pub fn new(state: PlaceState, ty: Type, mutability: Mutability) -> Self {
        Self {
            id: Id::new(),
            mutability,
            ty,
            state: tokio::sync::RwLock::new(state),
        }
    }
    fn read(&self) -> eyre::Result<PlaceReadGuard<'_, PlaceState>> {
        Ok(PlaceReadGuard(tokio::sync::RwLockReadGuard::map(
            self.state
                .try_read()
                .map_err(|_| eyre!("already borrowed mutably"))?,
            |x| x,
        )))
    }
    fn write(&self) -> eyre::Result<PlaceWriteGuard<'_, PlaceState>> {
        Ok(PlaceWriteGuard(tokio::sync::RwLockWriteGuard::map(
            self.state
                .try_write()
                .map_err(|_| eyre!("already borrowed"))?,
            |x| x,
        )))
    }
    pub fn read_value(&self) -> eyre::Result<PlaceReadGuard<'_, Value>> {
        self.mutability.check_can_read()?;
        let guard = self.read()?;
        match tokio::sync::RwLockReadGuard::try_map(guard.0, |state| state.get().ok()) {
            Ok(result) => Ok(PlaceReadGuard(result)),
            Err(guard) => Err(guard.get().unwrap_err().into()),
        }
    }
    pub fn write_value(&self) -> eyre::Result<PlaceWriteGuard<'_, ValueShape>> {
        self.mutability.check_can_mutate()?;
        let guard = self.write()?;
        match tokio::sync::RwLockMappedWriteGuard::try_map(guard.0, |state| state.get_mut().ok()) {
            Ok(result) => Ok(PlaceWriteGuard(result)),
            Err(guard) => Err(guard.get().unwrap_err().into()),
        }
    }
    pub fn take_value(&self) -> eyre::Result<Value> {
        self.mutability.check_can_read()?;
        Ok(self.write()?.take()?.clone())
    }
    pub fn clone_value(&self) -> eyre::Result<Value> {
        self.mutability.check_can_read()?;
        Ok(self.read()?.get()?.clone())
    }
    pub fn assign(&self, new_value: Value) -> eyre::Result<()> {
        let mut state = self.write()?;
        match *state {
            PlaceState::Unintialized => {}
            PlaceState::Occupied(_) | PlaceState::MovedOut => self.mutability.check_can_mutate()?,
        }
        state.assign(new_value);
        Ok(())
    }
}

impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.read().expect("Failed to read for Display") {
            PlaceState::Unintialized => write!(f, "<uninitialized>"),
            PlaceState::Occupied(value) => value.fmt(f),
            PlaceState::MovedOut => write!(f, "<moved out>"),
        }
    }
}

#[derive(Clone, PartialEq, TryHash, Eq, PartialOrd, Ord)]
pub enum PlaceState {
    Unintialized,
    // This place is taken
    Occupied(#[try_hash] Value),
    MovedOut,
}

impl std::fmt::Display for PlaceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlaceState::Unintialized => write!(f, "<uninitialized>"),
            PlaceState::Occupied(value) => value.fmt(f),
            PlaceState::MovedOut => write!(f, "<moved out>"),
        }
    }
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
