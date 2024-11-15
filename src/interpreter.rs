use super::*;

#[derive(Clone)]
pub struct State {
    pub spawned: bool,
    builtins: Parc<HashMap<&'static str, Builtin>>,
    scope: Parc<Scope>,
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
                let mut insert_ty = |name, ty: InferredType| {
                    map.insert(
                        name,
                        Box::new(move |expected: Type| {
                            expected.expect_inferred(InferredType::Type)?;
                            Ok(Value::Type(ty.clone().into()))
                        }),
                    );
                };
                insert_ty("bool", InferredType::Bool);
                insert_ty("int32", InferredType::Int32);
                insert_ty("int64", InferredType::Int64);
                insert_ty("float64", InferredType::Float64);
                insert_ty("string", InferredType::String);
                insert_ty("ast", InferredType::Ast);
                insert_ty("type", InferredType::Type);
                map.insert(
                    "dbg",
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: Type::new_not_inferred(),
                            result: InferredType::Unit.into(),
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "dbg".to_owned(),
                            r#impl: (std::sync::Arc::new(|fn_ty: FnType, value: Value| {
                                let ty = &fn_ty.arg;
                                assert_eq!(&value.ty(), ty);
                                println!("{value} :: {ty}");
                                Ok(Value::Unit)
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "panic",
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: InferredType::String.into(),
                            result: InferredType::Unit.into(), // TODO never type
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "panic".to_owned(),
                            r#impl: (std::sync::Arc::new(|_fn_ty, s: Value| {
                                let s = s.expect_string()?;
                                Err(eyre!("panic: {s}"))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "parse",
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: InferredType::String.into(),
                            result: Type::new_not_inferred(),
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "parse".to_owned(),
                            r#impl: (std::sync::Arc::new(|fn_ty: FnType, s: Value| {
                                let s = s.expect_string()?;
                                Ok(match fn_ty.result.inferred() {
                                    Ok(ty) => match ty {
                                        InferredType::Int32 => Value::Int32(s.parse()?),
                                        InferredType::Int64 => Value::Int64(s.parse()?),
                                        InferredType::Float64 => Value::Float64(s.parse()?),
                                        _ => eyre::bail!("{ty} is not parseable???"),
                                    },
                                    Err(_) => eyre::bail!("cant parse not inferred type"),
                                })
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "print",
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: InferredType::String.into(),
                            result: InferredType::Unit.into(),
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "print".to_owned(),
                            r#impl: (std::sync::Arc::new(|_fn_ty, s: Value| {
                                let s = s.expect_string()?;
                                println!("{s}");
                                Ok(Value::Unit)
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "<",
                    Box::new(|expected: Type| {
                        let operand_type = Type::new_not_inferred();
                        let ty = FnType {
                            arg: InferredType::Tuple({
                                let mut args = Tuple::empty();
                                args.add_named("lhs", operand_type.clone());
                                args.add_named("rhs", operand_type.clone());
                                args
                            })
                            .into(),
                            result: InferredType::Bool.into(),
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "<".to_owned(),
                            r#impl: (std::sync::Arc::new(|_fn_ty, args: Value| {
                                let [lhs, rhs] = args.expect_tuple()?.into_named(["lhs", "rhs"])?;
                                let result = match (&lhs, &rhs) {
                                    (Value::Int32(lhs), Value::Int32(rhs)) => {
                                        Value::Bool(lhs < rhs)
                                    }
                                    (Value::Int64(lhs), Value::Int64(rhs)) => {
                                        Value::Bool(lhs < rhs)
                                    }
                                    _ => eyre::bail!(
                                        "< doesnt work for {} and {}",
                                        lhs.ty(),
                                        rhs.ty()
                                    ),
                                };
                                Ok(result)
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "-",
                    Box::new(|expected: Type| {
                        let operand_type = Type::new_not_inferred();
                        let ty = FnType {
                            arg: InferredType::Tuple({
                                let mut args = Tuple::empty();
                                args.add_named("lhs", operand_type.clone());
                                args.add_named("rhs", operand_type.clone());
                                args
                            })
                            .into(),
                            result: operand_type,
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "-".to_owned(),
                            r#impl: (std::sync::Arc::new(|_fn_ty, args: Value| {
                                let [lhs, rhs] = args.expect_tuple()?.into_named(["lhs", "rhs"])?;
                                let result = match (&lhs, &rhs) {
                                    (Value::Int32(lhs), Value::Int32(rhs)) => Value::Int32(
                                        lhs.checked_sub(*rhs).ok_or_else(|| eyre!("overflow"))?,
                                    ),
                                    (Value::Int64(lhs), Value::Int64(rhs)) => Value::Int64(
                                        lhs.checked_sub(*rhs).ok_or_else(|| eyre!("overflow"))?,
                                    ),
                                    _ => eyre::bail!(
                                        "+ doesnt work for {} and {}",
                                        lhs.ty(),
                                        rhs.ty()
                                    ),
                                };
                                Ok(result)
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "+",
                    Box::new(|expected: Type| {
                        let operand_type = Type::new_not_inferred();
                        let ty = FnType {
                            arg: InferredType::Tuple({
                                let mut args = Tuple::empty();
                                args.add_named("lhs", operand_type.clone());
                                args.add_named("rhs", operand_type.clone());
                                args
                            })
                            .into(),
                            result: operand_type,
                        };
                        expected.expect_inferred(InferredType::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "+".to_owned(),
                            r#impl: (std::sync::Arc::new(|_fn_ty, args: Value| {
                                let [lhs, rhs] = args.expect_tuple()?.into_named(["lhs", "rhs"])?;
                                let result = match (&lhs, &rhs) {
                                    (Value::Int32(lhs), Value::Int32(rhs)) => Value::Int32(
                                        lhs.checked_add(*rhs).ok_or_else(|| eyre!("overflow"))?,
                                    ),
                                    (Value::Int64(lhs), Value::Int64(rhs)) => Value::Int64(
                                        lhs.checked_add(*rhs).ok_or_else(|| eyre!("overflow"))?,
                                    ),
                                    _ => eyre::bail!(
                                        "+ doesnt work for {} and {}",
                                        lhs.ty(),
                                        rhs.ty()
                                    ),
                                };
                                Ok(result)
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                Parc::new(map)
            },
            scope: Parc::new(Scope::new()),
        }
    }
    pub fn autocomplete<'a>(&'a self, s: &'a str) -> impl Iterator<Item = CompletionCandidate> {
        self.scope.inspect(|locals| {
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
        })
    }
    pub async fn get(&self, name: &str) -> Option<Value> {
        self.scope.get_impl(name, self.spawned).await
    }
    pub fn get_nowait(&self, name: &str) -> Option<Value> {
        self.scope.get_nowait(name)
    }
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.scope.insert(name.to_owned(), value);
    }
    pub fn scope_syntax_definitions(&self) -> Vec<Parc<ast::SyntaxDefinition>> {
        self.scope.syntax_definitions.lock().unwrap().clone()
    }
    pub fn insert_syntax(&mut self, definition: Parc<ast::SyntaxDefinition>) -> eyre::Result<()> {
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
        inner.interpreter.scope = Parc::new({
            let mut scope = Scope::recursive();
            scope.parent = Some(self.interpreter.scope.clone());
            scope
        });
        inner
    }
    #[must_use]
    pub fn enter_scope(&self) -> Self {
        let mut inner = self.clone();
        inner.interpreter.scope = Parc::new({
            let mut scope = Scope::new();
            scope.parent = Some(self.interpreter.scope.clone());
            scope
        });
        inner
    }
    #[must_use]
    pub fn with_scope(&self, scope: Parc<Scope>) -> Self {
        let mut kast = self.clone();
        kast.interpreter.scope = scope;
        kast
    }
    pub fn add_local(&mut self, name: &str, value: Value) {
        self.interpreter.scope.insert(name.to_owned(), value);
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
        let expected_ty = self.substitute_type_bindings(&expr.data().ty);
        let r#impl = async move {
            tracing::trace!("evaluating {}", expr.show_short());
            tracing::trace!("as {}", expr.data().ty);
            let result = match expr {
                Expr::Unit { data } => match data.ty.inferred_or_default()? {
                    Ok(InferredType::Type) => Value::Type(InferredType::Unit.into()),
                    Ok(InferredType::Unit) => Value::Unit,
                    Ok(other) => panic!("unit inferred to be not unit but {other}???"),
                    Err(_) => panic!("unit not inferred"),
                },
                Expr::FunctionType {
                    arg,
                    result,
                    data: _,
                } => {
                    let arg = self.eval(arg).await?.expect_type()?;
                    let result = self.eval(result).await?.expect_type()?;
                    Value::Type(InferredType::Function(Box::new(FnType { arg, result })).into())
                }
                Expr::Cast {
                    value,
                    target,
                    data: _,
                } => {
                    let value = self.eval(value).await?;
                    match self
                        .cache
                        .compiler
                        .casts
                        .lock()
                        .unwrap()
                        .cast(value, target)?
                    {
                        Ok(result) => result,
                        Err(value) => eyre::bail!("casting {value} into {target} not implemented"),
                    }
                }
                Expr::Match {
                    value,
                    branches,
                    data: _,
                } => 'result: {
                    let value = self.eval(value).await?;
                    for branch in branches {
                        // TODO no clone value
                        if let Some(matches) = branch.pattern.r#match(value.clone()) {
                            let mut kast = self.enter_scope();
                            kast.interpreter.scope.extend(matches);
                            let result = kast.eval(&branch.body).await?;
                            break 'result result;
                        }
                    }
                    eyre::bail!("non exhaustive pattern matching??")
                }
                Expr::Is {
                    value,
                    pattern,
                    data: _,
                } => {
                    let value = self.eval(value).await?;
                    if let Some(matches) = pattern.r#match(value) {
                        self.interpreter.scope.extend(matches);
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
                Expr::Newtype { def, data: _ } => {
                    let def = self.eval(def).await?;
                    let ty: Type = match def {
                        Value::Multiset(values) => {
                            let mut variants = Vec::new();
                            for value in values {
                                let value = value.expect_variant()?;
                                variants.push(VariantType {
                                    name: value.name,
                                    value: value
                                        .value
                                        .map(|value| value.expect_type())
                                        .transpose()?
                                        .map(Box::new),
                                });
                            }
                            InferredType::Variant(variants).into()
                        }
                        _ => eyre::bail!("{def} can not be used in newtype"),
                    };
                    Value::Type(ty)
                }
                Expr::MakeMultiset { values, data: _ } => {
                    let mut result = Vec::new();
                    for value in values {
                        result.push(self.eval(value).await?);
                    }
                    Value::Multiset(result)
                }
                Expr::Variant { name, value, data } => {
                    let value = match value {
                        Some(value) => Some(self.eval(value).await?),
                        None => None,
                    };
                    Value::Variant(VariantValue {
                        name: name.clone(),
                        value: value.map(Box::new),
                        ty: data.ty.clone(),
                    })
                }
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
                Expr::Tuple { tuple, data } => {
                    let mut result = Tuple::empty();
                    for (name, field) in tuple.as_ref() {
                        result.add(name, self.eval(field).await?);
                    }
                    let result = Value::Tuple(result);
                    match data.ty.inferred_or_default()? {
                        Ok(InferredType::Type) => self
                            .cache
                            .compiler
                            .casts
                            .lock()
                            .unwrap()
                            .cast(result, &Value::Type(InferredType::Type.into()))?
                            .map_err(|tuple| eyre!("{tuple} can not be cast into type"))?,
                        Ok(InferredType::Tuple(..)) => result,
                        Ok(ty) => eyre::bail!("tuple expr type inferred as {ty}???"),
                        Err(_) => eyre::bail!("tuple type could not be inferred???"),
                    }
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
                        Value::SyntaxModule(definitions) => Value::SyntaxDefinition(
                            definitions
                                .iter() // TODO store a map? iteration is slow?
                                .find(|def| def.name == *field)
                                .ok_or_else(|| {
                                    eyre!("syntax def {field:?} not found in syntax module")
                                })?
                                .clone(),
                        ),
                        _ => eyre::bail!("{obj} is not smth that has fields"),
                    }
                }
                Expr::Recursive { body, data: _ } => {
                    let mut inner = self.enter_recursive_scope();
                    inner.eval(body).await?.expect_unit()?;
                    let mut fields = Tuple::empty();
                    inner.interpreter.scope.inspect(|locals| {
                        for (name, value) in locals {
                            fields.add_named(name.clone(), value.clone());
                        }
                    });
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
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data: _,
                } => {
                    let mut kast = self.enter_scope();
                    let condition = kast.eval(condition).await?.expect_bool()?;
                    if condition {
                        kast.eval(then_case).await?
                    } else if let Some(else_case) = else_case {
                        kast.eval(else_case).await?
                    } else {
                        Value::Unit
                    }
                }
                Expr::Then { list, data: _ } => {
                    let mut value = Value::Unit;
                    for expr in list {
                        value = self.eval(expr).await?;
                    }
                    value
                }
                Expr::Constant { value, data: _ } => match value {
                    Value::Type(ty) => Value::Type(self.substitute_type_bindings(ty)),
                    _ => value.clone(),
                },
                Expr::Number { raw, data } => match data.ty.inferred_or_default()? {
                    Ok(InferredType::Int32) => Value::Int32(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int32"))?,
                    ),
                    Ok(InferredType::Int64) => Value::Int64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int64"))?,
                    ),
                    Ok(InferredType::Float64) => Value::Float64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as float64"))?,
                    ),
                    Ok(other) => {
                        eyre::bail!("number literals can not be treated as {other}")
                    }
                    Err(_) => eyre::bail!("number literal type could not be inferred"),
                },
                Expr::Native { name, data } => {
                    let actual_type = self.substitute_type_bindings(&data.ty);
                    let name = self.eval(name).await?.expect_string()?;
                    tracing::trace!("native {name} :: {actual_type}");
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
                    let matches = pattern
                        .r#match(value)
                        .expect("pattern match was not exhaustive???");
                    self.interpreter.scope.extend(matches);
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
                Expr::Instantiate {
                    template,
                    arg,
                    data: _,
                } => {
                    let template = self.eval(template).await?.expect_template()?;
                    let arg = self.eval(arg).await?;
                    self.call_fn(template, arg).await?
                }
            };
            let should_check_result_ty = match expr {
                Expr::Unit { .. }
                | Expr::FunctionType { .. }
                | Expr::Cast { .. }
                | Expr::Is { .. }
                | Expr::Match { .. }
                | Expr::Newtype { .. }
                | Expr::Variant { .. }
                | Expr::MakeMultiset { .. }
                | Expr::Use { .. }
                | Expr::FieldAccess { .. }
                | Expr::Binding { .. }
                | Expr::If { .. }
                | Expr::Then { .. }
                | Expr::Constant { .. }
                | Expr::Number { .. }
                | Expr::Native { .. }
                | Expr::Let { .. }
                | Expr::Call { .. }
                | Expr::Scope { .. }
                | Expr::Function { .. }
                | Expr::Template { .. }
                | Expr::Instantiate { .. }
                | Expr::Tuple { .. }
                | Expr::Ast { .. } => true,
                // TODO
                Expr::Recursive { .. } => false,
            };
            if should_check_result_ty && result.ty() != expected_ty {
                eyre::bail!(
                    "expr expected to be of type {}, but we got {} :: {}",
                    expected_ty,
                    result,
                    result.ty(),
                );
            }
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

    pub async fn await_compiled(&mut self, f: &Function) -> eyre::Result<Parc<CompiledFn>> {
        self.advance_executor();
        match &*f.compiled.lock().unwrap() {
            Some(compiled) => Ok(compiled.clone()),
            None => panic!("function is not compiled yet"),
        }
    }

    pub async fn call_fn(&mut self, f: Function, arg: Value) -> eyre::Result<Value> {
        let mut new_scope = Scope::new();
        new_scope.parent = Some(f.captured.clone());
        let compiled: Parc<CompiledFn> = self.await_compiled(&f).await?;
        arg.ty().make_same(compiled.arg.data().ty.clone())?;
        let mut kast = self.with_scope(Parc::new(new_scope));
        kast.bind_pattern_match(&compiled.arg, arg);
        let value = kast.eval(&compiled.body).await?;
        Ok(value)
    }

    pub fn bind_pattern_match(&mut self, pattern: &Pattern, value: Value) {
        self.interpreter
            .scope
            .extend(pattern.r#match(value).expect("???"));
    }
}

impl Pattern {
    pub fn r#match(&self, value: Value) -> Option<Vec<(Parc<Binding>, Value)>> {
        let mut result = Vec::new();
        fn match_impl(
            pattern: &Pattern,
            value: Value,
            matches: &mut Vec<(Parc<Binding>, Value)>,
        ) -> Option<()> {
            match pattern {
                Pattern::Placeholder { data: _ } => {}
                Pattern::Unit { data: _ } => {
                    assert_eq!(value, Value::Unit);
                }
                Pattern::Binding { binding, data: _ } => {
                    matches.push((binding.clone(), value));
                }
                Pattern::Variant {
                    name,
                    value: value_pattern,
                    data: _,
                } => {
                    let value = value
                        .expect_variant()
                        .expect("matching non variant with variant???");
                    if value.name == *name {
                        match (value_pattern, value.value) {
                            (None, None) => {}
                            (Some(_), None) => panic!("pattern expected a value"),
                            (None, Some(_)) => panic!("pattern did not expect a value"),
                            (Some(value_pattern), Some(value)) => {
                                match_impl(value_pattern, *value, matches)?
                            }
                        }
                    } else {
                        return None;
                    }
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
                        match_impl(field_pattern, field_value, matches)?;
                    }
                }
            }
            Some(())
        }
        match_impl(self, value, &mut result)?;
        Some(result)
    }
}
