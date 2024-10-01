use super::*;

#[derive(Clone)]
pub struct State {
    builtins: Arc<HashMap<&'static str, Builtin>>,
    scope: Arc<Scope>,
}

type Builtin = Box<dyn Fn(Type) -> eyre::Result<Value> + Send + Sync>;

pub struct CompletionCandidate {
    pub name: String,
    pub ty: Type,
}

impl State {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            builtins: {
                let mut map = HashMap::<&str, Builtin>::new();
                let mut insert_ty = |name, ty: Type| {
                    map.insert(
                        name,
                        Box::new(move |mut expected: Type| {
                            expected.make_same(Type::Type)?;
                            Ok(Value::Type(ty.clone()))
                        }),
                    );
                };
                insert_ty("bool", Type::Bool);
                insert_ty("int32", Type::Int32);
                insert_ty("string", Type::String);
                insert_ty("type", Type::Type);
                map.insert(
                    "dbg",
                    Box::new(|mut expected: Type| {
                        let ty = FnType {
                            arg: Type::Infer(inference::Var::new()),
                            result: Type::Unit,
                        };
                        expected.make_same(Type::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "dbg".to_owned(),
                            r#impl: Arc::new(|fn_ty, value: Value| {
                                let ty = &fn_ty.arg;
                                assert_eq!(&value.ty(), ty);
                                println!("{value} :: {ty}");
                                Ok(Value::Unit)
                            }),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "print",
                    Box::new(|mut expected: Type| {
                        let ty = FnType {
                            arg: Type::String,
                            result: Type::Unit,
                        };
                        expected.make_same(Type::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "print".to_owned(),
                            r#impl: Arc::new(|_fn_ty, s: Value| {
                                let s = s.expect_string()?;
                                println!("{s}");
                                Ok(Value::Unit)
                            }),
                            ty,
                        }))
                    }),
                );
                Arc::new(map)
            },
            scope: Arc::new(Scope::new()),
        }
    }
    pub fn autocomplete<'a>(&'a self, s: &'a str) -> impl Iterator<Item = CompletionCandidate> {
        let locals = self.scope.locals.lock().unwrap();
        locals
            .iter()
            .filter_map(move |(name, value)| {
                if name.contains(s) {
                    Some(CompletionCandidate {
                        name: name.clone(),
                        ty: value.ty(),
                    })
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
    pub async fn get(&self, name: &str) -> Option<Value> {
        self.scope.get(name).await
    }
    pub fn enter_scope(&mut self) {
        tracing::trace!("entering scope");
        self.scope = Arc::new({
            let mut scope = Scope::new();
            scope.parent = Some(self.scope.clone());
            scope
        });
    }
    pub fn enter_recursive_scope(&mut self) {
        tracing::trace!("entering recursive scope");
        self.scope = Arc::new({
            let mut scope = Scope::recursive();
            scope.parent = Some(self.scope.clone());
            scope
        });
    }
    pub fn exit_scope(&mut self) {
        tracing::trace!("exit scope");
        self.scope.close();
        self.scope = self.scope.parent.clone().expect("no parent scope");
    }
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.scope
            .locals
            .lock()
            .unwrap()
            .insert(name.to_owned(), value);
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
            Some(ast) => futures::executor::block_on(self.eval_ast(&ast, expected_ty)), // TODO
            None => Ok(Value::Unit),
        }
    }
    pub async fn eval_ast(&mut self, ast: &Ast, expected_ty: Option<Type>) -> eyre::Result<Value> {
        let mut expr: Expr = self.compile(ast).await?;
        if let Some(ty) = expected_ty {
            expr.data_mut().ty.make_same(ty)?;
        }
        let result = self.eval(&expr).await?;
        Ok(result)
    }
    pub fn eval<'a>(&'a mut self, expr: &'a Expr) -> BoxFuture<'a, eyre::Result<Value>> {
        let r#impl = async move {
            tracing::debug!("evaluating {}", expr.show_short());
            let result = match expr {
                Expr::Recursive { body, data: _ } => {
                    self.interpreter.enter_recursive_scope();
                    let value = self.eval(body).await?;
                    self.interpreter.exit_scope();
                    value
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: _,
                } => Value::Function(Function {
                    id: Id::new(),
                    ty: ty.clone(),
                    captured: self.interpreter.scope.clone(),
                    compiled: compiled.clone(),
                }),
                Expr::Scope { expr, data: _ } => {
                    self.interpreter.enter_scope();
                    let value = self.eval(expr).await?;
                    self.interpreter.exit_scope();
                    value
                }
                Expr::Binding { binding, data: _ } => self
                    .interpreter
                    .get(binding.name.raw())
                    .await // TODO this should not be async?
                    .ok_or_else(|| eyre!("{:?} not found", binding.name))?
                    .clone(),
                Expr::Then { a, b, data: _ } => {
                    self.eval(a).await?;
                    self.eval(b).await?
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
                    let name = self.eval(name).await?.expect_string()?;
                    match self.interpreter.builtins.get(name.as_str()) {
                        Some(builtin) => builtin(data.ty.clone())?,
                        None => eyre::bail!("native {name:?} not found"),
                    }
                }
                Expr::Let {
                    pattern,
                    value,
                    data: _,
                } => {
                    let value = self.eval(value).await?;
                    let matches = pattern.r#match(value);
                    self.interpreter.scope.locals.lock().unwrap().extend(
                        matches
                            .into_iter()
                            .map(|(binding, value)| (binding.name.raw().to_owned(), value)),
                    );
                    Value::Unit
                }
                Expr::Call { f, arg, data: _ } => {
                    let f = self.eval(f).await?;
                    let arg = self.eval(arg).await?;
                    match f {
                        Value::NativeFunction(f) => (f.r#impl)(f.ty.clone(), arg)?,
                        Value::Function(f) => {
                            let mut new_scope = Scope::new();
                            new_scope.parent = Some(f.captured.clone());
                            while self.executor.try_tick() {}
                            let compiled: Arc<CompiledFn> = match &*f.compiled.lock().unwrap() {
                                Some(compiled) => compiled.clone(),
                                None => panic!("function is not compiled yet"),
                            };
                            new_scope.locals.lock().unwrap().extend(
                                compiled
                                    .arg
                                    .r#match(arg)
                                    .into_iter()
                                    .map(|(binding, value)| (binding.name.raw().to_owned(), value)),
                            );
                            let prev_scope =
                                std::mem::replace(&mut self.interpreter.scope, Arc::new(new_scope));
                            let value = self.eval(&compiled.body).await?;
                            self.interpreter.scope = prev_scope;
                            value
                        }
                        _ => eyre::bail!("{f} is not a function"),
                    }
                }
            };
            tracing::debug!("finished evaluating {}", expr.show_short());
            tracing::trace!("result = {result}");
            Ok(result)
        };
        async {
            r#impl
                .await
                .wrap_err_with(|| format!("while evaluating {}", expr.show_short()))
        }
        .boxed()
    }
}

impl Pattern {
    #[must_use]
    pub fn r#match(&self, value: Value) -> Vec<(Arc<Binding>, Value)> {
        let mut result = Vec::new();
        fn match_impl(pattern: &Pattern, value: Value, matches: &mut Vec<(Arc<Binding>, Value)>) {
            match pattern {
                Pattern::Unit { data: _ } => {
                    assert_eq!(value, Value::Unit);
                }
                Pattern::Binding { binding, data: _ } => {
                    matches.push((binding.clone(), value));
                }
            }
        }
        match_impl(self, value, &mut result);
        result
    }
}
