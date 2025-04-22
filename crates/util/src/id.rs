pub trait Identifiable {
    type Id: 'static + Eq + std::hash::Hash;
    fn id(&self) -> Self::Id;
}
