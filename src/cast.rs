use super::*;

#[derive(TryHash, PartialEq, Eq, Clone)]
struct Key {
    #[try_hash]
    value: Value,
    #[try_hash]
    target: Value,
}

impl Key {
    fn new(value: Value, target: Value) -> eyre::Result<Self> {
        let err = eyre!("cast key: value = {}, target = {}", value, target);
        let key = Self { value, target };
        key.try_hash(&mut std::hash::DefaultHasher::new())
            .map_err(|e| eyre!(e))
            .wrap_err(err)?;
        Ok(key)
    }
}

impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.try_hash(state).unwrap()
    }
}

#[derive(Default)]
pub struct CastMap {
    map: HashMap<Key, Value>,
}

impl CastMap {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn impl_cast(&mut self, value: Value, target: Value, r#impl: Value) -> eyre::Result<()> {
        tracing::trace!("impl {value} as {target} = {impl} :: {}", r#impl.ty());
        self.map.insert(Key::new(value, target)?, r#impl);
        Ok(())
    }
    #[allow(clippy::only_used_in_recursion)]
    pub fn cast_to_ty(&self, value: Value) -> eyre::Result<Result<Type, Value>> {
        Ok(Ok(match value {
            Value::Unit => TypeShape::Unit.into(),
            Value::Tuple(tuple) => {
                let mut tuple_ty = Tuple::<Type>::empty();
                for (name, value) in tuple {
                    tuple_ty.add(
                        name,
                        self.cast_to_ty(value)?
                            .map_err(|value| eyre!("{value} is not a type"))?,
                    );
                }
                TypeShape::Tuple(tuple_ty).into()
            }
            _ => match value.expect_type() {
                Ok(value) => value,
                Err(e) => return Ok(Err(e.value)),
            },
        }))
    }
    pub fn cast(&self, value: Value, target: &Value) -> eyre::Result<Result<Value, Value>> {
        #[allow(clippy::single_match)]
        match target {
            Value::Type(ty) => match ty.inferred() {
                Ok(TypeShape::Type) => {
                    return self.cast_to_ty(value).map(|result| result.map(Value::Type));
                }
                _ => {}
            },
            _ => {}
        }
        let key = Key::new(value.clone(), target.clone())?;
        Ok(self.map.get(&key).cloned().ok_or(value))
    }
}
