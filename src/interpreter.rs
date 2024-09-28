use std::{collections::HashMap, sync::Arc};

use super::*;

pub struct State {
    builtins: HashMap<&'static str, Value>,
    values: HashMap<Name, Value>,
}

impl State {
    pub fn new() -> Self {
        Self {
            builtins: {
                let mut map = HashMap::new();
                map.insert("bool", Value::Type(Type::Bool));
                map.insert("int32", Value::Type(Type::Int32));
                map.insert("string", Value::Type(Type::String));
                map
            },
            values: HashMap::new(),
        }
    }
}

impl Kast {
    pub fn eval_ast(&mut self, ast: Ast) -> Value {
        let expr = self.compile_expr(ast);
        self.eval(expr)
    }
    pub fn eval(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Binding { binding, data: _ } => self
                .interpreter
                .values
                .get(&binding.name)
                .expect("todo err binding not found")
                .clone(),
            Expr::Constant { value, data: _ } => value,
            Expr::Number { raw, data } => match data.ty.inferred() {
                Ok(Type::Int32) => {
                    Value::Int32(raw.parse().expect("todo err failed to parse number"))
                }
                Ok(other) => {
                    panic!("todo err {other} is not a number")
                }
                Err(_) => panic!("todo err number type could not be inferred"),
            },
            Expr::Native { name, data: _ } => {
                let name = self
                    .eval(*name)
                    .expect_string()
                    .map_err(|s| error_fmt!("{s} is not a string"))
                    .expect("todo err native name must be a string");
                match self.interpreter.builtins.get(name.as_str()) {
                    Some(value) => value.clone(),
                    None => panic!("todo err builtin {name:?} not found"),
                }
            }
            Expr::Let {
                pattern,
                value,
                data: _,
            } => {
                let value = self.eval(*value);
                let matches = pattern.r#match(value);
                for (binding, _value) in &matches {
                    // TODO do smth else
                    self.compiler
                        .locals
                        .insert(binding.name.raw().to_owned(), binding.clone());
                }
                self.interpreter.values.extend(
                    matches
                        .into_iter()
                        .map(|(binding, value)| (binding.name.clone(), value)),
                );
                Value::Unit
            }
        }
    }
}

impl Pattern {
    #[must_use]
    pub fn r#match(&self, value: Value) -> Vec<(Arc<Binding>, Value)> {
        let mut result = Vec::new();
        fn match_impl(pattern: &Pattern, value: Value, matches: &mut Vec<(Arc<Binding>, Value)>) {
            match pattern {
                Pattern::Binding { binding, data: _ } => {
                    matches.push((binding.clone(), value));
                }
            }
        }
        match_impl(self, value, &mut result);
        result
    }
}
