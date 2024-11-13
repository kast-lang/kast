use super::*;

#[derive(Default)]
pub struct CastMap {
    map: HashMap<(Value, Value), Value>,
}

impl CastMap {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn impl_cast(&mut self, value: Value, target: Value, r#impl: Value) {
        self.map.insert((value, target), r#impl);
    }
    pub fn cast(&self, value: Value, target: &Value) -> Result<Value, Value> {
        #[allow(clippy::single_match)]
        match target {
            Value::Type(ty) => match ty.inferred() {
                Ok(InferredType::Type) => {
                    return value.expect_type().map(Value::Type).map_err(|e| e.value)
                }
                _ => {}
            },
            _ => {}
        }
        self.map
            .get(&(value.clone(), target.clone()))
            .cloned()
            .ok_or(value)
    }
}
