use super::*;

#[derive(Clone, Default)]
pub enum DefaultValue<T> {
    #[default]
    None,
    Some(Arc<T>),
    Conflicted,
}

impl<T: Inferrable> DefaultValue<T> {
    pub fn get(&self) -> Option<Arc<T>> {
        match self {
            DefaultValue::None | DefaultValue::Conflicted => None,
            DefaultValue::Some(value) => Some(value.clone()),
        }
    }
    pub fn common(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::None, Self::None) => Self::None,
            (Self::None, Self::Some(value)) => Self::Some(value),
            (Self::Some(value), Self::None) => Self::Some(value),
            (Self::Conflicted, _) | (_, Self::Conflicted) => Self::Conflicted,
            (Self::Some(a), Self::Some(b)) => match T::make_same(T::clone(&a), T::clone(&b)) {
                Ok(value) => Self::Some(Arc::new(value)),
                Err(_) => Self::Conflicted,
            },
        }
    }
}
