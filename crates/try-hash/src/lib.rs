pub use try_hash_derive::TryHash;

pub trait TryHash {
    type Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error>;
}

impl<T: ?Sized + TryHash> TryHash for Box<T> {
    type Error = T::Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        T::try_hash(self, hasher)
    }
}

impl<T: TryHash> TryHash for Option<T> {
    type Error = T::Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        std::hash::Hash::hash(&std::mem::discriminant(self), hasher);
        if let Some(value) = self {
            value.try_hash(hasher)?;
        }
        Ok(())
    }
}

impl<T: TryHash> TryHash for Vec<T> {
    type Error = T::Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        for elem in self {
            elem.try_hash(hasher)?;
        }
        Ok(())
    }
}

impl<T: TryHash> TryHash for std::sync::Arc<T> {
    type Error = T::Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        T::try_hash(self, hasher)
    }
}
