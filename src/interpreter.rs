use super::*;

#[derive(Clone)]
pub struct State {
    pub spawned: bool,
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
            spawned: false,
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
                insert_ty("ast", Type::Ast);
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
        self.scope.get_impl(name, self.spawned).await
    }
    pub fn get_nowait(&self, name: &str) -> Option<Value> {
        self.scope.get_nowait(name)
    }
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.scope
            .locals
            .lock()
            .unwrap()
            .insert(name.to_owned(), value);
    }
    pub fn scope_syntax_definitions(&self) -> Vec<Arc<ast::SyntaxDefinition>> {
        self.scope.syntax_definitions.lock().unwrap().clone()
    }
    pub fn insert_syntax(&mut self, definition: Arc<ast::SyntaxDefinition>) -> eyre::Result<()> {
        self.scope
            .syntax_definitions
            .lock()
            .unwrap()
            .push(definition);
        Ok(())
    }
}

impl Drop for Kast {
    fn drop(&mut self) {
        self.interpreter.scope.close();
        self.advance_executor();
    }
}

impl Kast {
    #[must_use]
    pub fn enter_recursive_scope(&self) -> Self {
        let mut inner = self.clone();
        inner.interpreter.scope = Arc::new({
            let mut scope = Scope::recursive();
            scope.parent = Some(self.interpreter.scope.clone());
            scope
        });
        inner
    }
    #[must_use]
    pub fn enter_scope(&self) -> Self {
        let mut inner = self.clone();
        inner.interpreter.scope = Arc::new({
            let mut scope = Scope::new();
            scope.parent = Some(self.interpreter.scope.clone());
            scope
        });
        inner
    }
    pub fn add_local(&mut self, name: &str, value: Value) {
        self.interpreter
            .scope
            .locals
            .lock()
            .unwrap()
            .insert(name.to_owned(), value);
    }
    pub fn eval_source(
        &mut self,
        source: SourceFile,
        expected_ty: Option<Type>,
    ) -> eyre::Result<Value> {
        let ast = ast::parse(&self.syntax, source)?;
        match ast {
            Some(ast) => futures_lite::future::block_on(self.eval_ast(&ast, expected_ty)), // TODO
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
                Expr::Use { namespace, data: _ } => {
                    let namespace = self.eval(namespace).await?;
                    match namespace {
                        Value::Tuple(namespace) => {
                            for (name, value) in namespace.into_iter() {
                                let name = name.ok_or_else(|| eyre!("cant use unnamed fields"))?;
                                self.add_local(name.as_str(), value);
                            }
                        }
                        _ => eyre::bail!("{namespace} is not a namespace"),
                    }
                    Value::Unit
                }
                Expr::Tuple { tuple, data: _ } => {
                    let mut result = Tuple::empty();
                    for (name, field) in tuple.as_ref() {
                        result.add(name, self.eval(field).await?);
                    }
                    Value::Tuple(result)
                }
                Expr::FieldAccess {
                    obj,
                    field,
                    data: _,
                } => {
                    let obj = self.eval(obj).await?;
                    match &obj {
                        Value::Tuple(fields) => match fields.get_named(field) {
                            Some(field_value) => field_value.clone(),
                            None => eyre::bail!("{obj} does not have field {field:?}"),
                        },
                        _ => eyre::bail!("{obj} is not smth that has fields"),
                    }
                }
                Expr::Recursive { body, data: _ } => {
                    let mut inner = self.enter_recursive_scope();
                    inner.eval(body).await?.expect_unit()?;
                    let mut fields = Tuple::empty();
                    for (name, value) in &*inner.interpreter.scope.locals.lock().unwrap() {
                        fields.add_named(name.clone(), value.clone());
                    }
                    Value::Tuple(fields)
                }
                Expr::Ast {
                    definition,
                    values,
                    data,
                } => {
                    let mut ast_values = Tuple::empty();
                    for (name, value) in values.as_ref().into_iter() {
                        let value = self.eval(value).await?;
                        let value = value.expect_ast()?;
                        ast_values.add(name, value);
                    }
                    Value::Ast(Ast::Complex {
                        definition: definition.clone(),
                        values: ast_values,
                        data: data.span.clone(),
                    })
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: _,
                } => Value::Function(TypedFunction {
                    ty: ty.clone(),
                    f: Function {
                        id: Id::new(),
                        captured: self.interpreter.scope.clone(),
                        compiled: compiled.clone(),
                    },
                }),
                Expr::Template { compiled, data: _ } => Value::Template(Function {
                    id: Id::new(),
                    captured: self.interpreter.scope.clone(),
                    compiled: compiled.clone(),
                }),
                Expr::Scope { expr, data: _ } => {
                    let mut inner = self.enter_scope();
                    inner.eval(expr).await?
                }
                Expr::Binding { binding, data: _ } => self
                    .interpreter
                    .get(binding.name.raw())
                    .await // TODO this should not be async?
                    .ok_or_else(|| eyre!("{:?} not found", binding.name))?
                    .clone(),
                Expr::Then { list, data: _ } => {
                    let mut value = Value::Unit;
                    for expr in list {
                        value = self.eval(expr).await?;
                    }
                    value
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
                    let actual_type = self.substitute_type_bindings(&data.ty);
                    let name = self.eval(name).await?.expect_string()?;
                    match self.interpreter.builtins.get(name.as_str()) {
                        Some(builtin) => builtin(actual_type)?,
                        None => eyre::bail!("native {name:?} not found"),
                    }
                }
                Expr::Let {
                    is_const_let: _,
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
                        Value::Function(f) => self.call_fn(f.f, arg).await?,
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

    pub async fn call_fn(&mut self, f: Function, arg: Value) -> eyre::Result<Value> {
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
        let prev_scope = std::mem::replace(&mut self.interpreter.scope, Arc::new(new_scope));
        let value = self.eval(&compiled.body).await?;
        self.interpreter.scope = prev_scope;
        Ok(value)
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
                Pattern::Tuple {
                    tuple: pattern,
                    data: _,
                } => {
                    let value = match value {
                        Value::Tuple(value) => value,
                        _ => {
                            let pattern = pattern.as_ref().map(|_| "_");
                            panic!("trying to pattern match tuple {pattern}, but got {value}");
                        }
                    };
                    for (_, (field_pattern, field_value)) in pattern
                        .as_ref()
                        .zip(value)
                        .expect("pattern is incorrect structure")
                        .into_iter()
                    {
                        match_impl(field_pattern, field_value, matches);
                    }
                }
            }
        }
        match_impl(self, value, &mut result);
        result
    }
}
