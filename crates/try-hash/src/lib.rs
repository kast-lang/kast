pub use try_hash_derive::TryHash;

pub trait TryHash {
    type Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error>;
}

impl<T> TryHash for T
where
    T: std::hash::Hash,
{
    type Error = Box<dyn std::error::Error>;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        <T as std::hash::Hash>::hash(self, hasher);
        Ok(())
    }
}
