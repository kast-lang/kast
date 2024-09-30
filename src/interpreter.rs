use std::{collections::HashMap, sync::Arc};

use super::*;

pub struct State {
    builtins: HashMap<&'static str, Value>,
    scope: Scope,
}

struct Scope {
    parent: Option<Box<Scope>>,
    locals: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            parent: None,
            locals: HashMap::new(),
        }
    }
    fn get(&self, name: &str) -> Option<&Value> {
        if let Some(value) = self.locals.get(name) {
            return Some(value);
        }
        if let Some(parent) = &self.parent {
            if let Some(value) = parent.get(name) {
                return Some(value);
            }
        }
        None
    }
}

pub struct CompletionCandidate {
    pub name: String,
    pub ty: Type,
}

impl State {
    pub fn new() -> Self {
        Self {
            builtins: {
                let mut map = HashMap::new();
                map.insert("bool", Value::Type(Type::Bool));
                map.insert("int32", Value::Type(Type::Int32));
                map.insert("string", Value::Type(Type::String));
                map.insert("type", Value::Type(Type::Type));
                map.insert(
                    "print",
                    Value::NativeFunction(NativeFunction {
                        name: "print".to_owned(),
                        r#impl: Arc::new(|_fn_ty, s: Value| {
                            let s = s.expect_string()?;
                            println!("{s}");
                            Ok(Value::Unit)
                        }),
                        ty: FnType {
                            arg: Type::String,
                            result: Type::Unit,
                        },
                    }),
                );
                map
            },
            scope: Scope::new(),
        }
    }
    pub fn autocomplete<'a>(
        &'a self,
        s: &'a str,
    ) -> impl Iterator<Item = CompletionCandidate> + 'a {
        self.scope.locals.iter().filter_map(move |(name, value)| {
            if name.contains(s) {
                Some(CompletionCandidate {
                    name: name.clone(),
                    ty: value.ty(),
                })
            } else {
                None
            }
        })
    }
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.scope.get(name)
    }
    pub fn enter_scope(&mut self) {
        let parent = std::mem::replace(&mut self.scope, Scope::new());
        self.scope.parent = Some(Box::new(parent));
    }
    pub fn exit_scope(&mut self) {
        self.scope = *self.scope.parent.take().expect("no parent scope");
    }
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.scope.locals.insert(name.to_owned(), value);
    }
}

impl Kast {
    pub fn eval_source(
        &mut self,
        source: SourceFile,
        expected_ty: Option<Type>,
    ) -> eyre::Result<Value> {
        let ast = ast::parse(&self.syntax, source)?;
        match ast {
            Some(ast) => self.eval_ast(&ast, expected_ty),
            None => Ok(Value::Unit),
        }
    }
    pub fn eval_ast(&mut self, ast: &Ast, expected_ty: Option<Type>) -> eyre::Result<Value> {
        let mut expr: Expr = self.compile(ast)?;
        if let Some(ty) = expected_ty {
            expr.data_mut().ty.make_same(ty)?;
        }
        let result = self.eval(&expr)?;
        Ok(result)
    }
    pub fn eval(&mut self, expr: &Expr) -> eyre::Result<Value> {
        (|| {
            Ok(match expr {
                Expr::Scope { expr, data: _ } => {
                    self.interpreter.enter_scope();
                    let value = self.eval(expr)?;
                    self.interpreter.exit_scope();
                    value
                }
                Expr::Binding { binding, data: _ } => self
                    .interpreter
                    .get(binding.name.raw())
                    .ok_or_else(|| eyre!("{:?} not found", binding.name))?
                    .clone(),
                Expr::Then { a, b, data: _ } => {
                    self.eval(a)?;
                    self.eval(b)?
                }
                Expr::Constant { value, data: _ } => value.clone(),
                Expr::Number { raw, data } => match data.ty.inferred() {
                    Ok(Type::Int32) => Value::Int32(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int32"))?,
                    ),
                    Ok(other) => {
                        eyre::bail!("number literals can not be treated as {other}")
                    }
                    Err(_) => eyre::bail!("number literal type could not be inferred"),
                },
                Expr::Native { name, data } => {
                    let name = self.eval(name)?.expect_string()?;
                    match self.interpreter.builtins.get(name.as_str()) {
                        Some(value) => {
                            // TODO: mutate?
                            data.ty.clone().make_same(value.ty())?;
                            value.clone()
                        }
                        None => eyre::bail!("native {name:?} not found"),
                    }
                }
                Expr::Let {
                    pattern,
                    value,
                    data: _,
                } => {
                    let value = self.eval(value)?;
                    let matches = pattern.r#match(value);
                    self.interpreter.scope.locals.extend(
                        matches
                            .into_iter()
                            .map(|(binding, value)| (binding.name.raw().to_owned(), value)),
                    );
                    Value::Unit
                }
                Expr::Call { f, args, data: _ } => {
                    let f = self.eval(f)?;
                    let args = self.eval(args)?;
                    match f {
                        Value::NativeFunction(f) => (f.r#impl)(f.ty.clone(), args)?,
                        _ => eyre::bail!("{f} is not a function"),
                    }
                }
            })
        })()
        .wrap_err_with(|| format!("while evaluating {}", expr.show_short()))
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
