use super::*;

mod autocomplete;
mod natives;

#[derive(Clone)]
pub struct State {
    pub contexts: Parc<Mutex<contexts::State>>,
    pub check_types: bool,
}

pub struct Cache {
    pub natives: natives::Natives,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            natives: natives::Natives::new(),
        }
    }
}

impl State {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            contexts: Parc::new(Mutex::new(contexts::State::default())),
            check_types: cfg!(debug_assertions), // TODO add option to configure in cli
        }
    }
}

impl Kast {
    pub fn scope_syntax_definitions(&self) -> Vec<Parc<ast::SyntaxDefinition>> {
        self.scopes
            .interpreter
            .syntax_definitions()
            .lock()
            .unwrap()
            .clone()
    }
    pub fn insert_syntax(&mut self, definition: Parc<ast::SyntaxDefinition>) -> eyre::Result<()> {
        self.scopes
            .interpreter
            .syntax_definitions()
            .lock()
            .unwrap()
            .push(definition);
        Ok(())
    }
}

impl Drop for Kast {
    fn drop(&mut self) {
        let _ = self.cache.executor.advance();
    }
}

pub trait SomeExprResult: Sized {
    fn unit() -> Self;
    fn from_ref(r#ref: PlaceRef, kast: &Kast) -> eyre::Result<Self>;
}

impl SomeExprResult for Value {
    fn unit() -> Self {
        ValueShape::Unit.into()
    }
    fn from_ref(r#ref: PlaceRef, kast: &Kast) -> eyre::Result<Self> {
        r#ref.claim_value(kast)
    }
}

impl SomeExprResult for PlaceRef {
    fn unit() -> Self {
        PlaceRef::new_temp(ValueShape::Unit.into())
    }
    fn from_ref(r#ref: PlaceRef, _kast: &Kast) -> eyre::Result<Self> {
        Ok(r#ref)
    }
}

impl Kast {
    pub fn register_fn<A: Rusty, R: Rusty>(
        &mut self,
        name: &str,
        f: impl Fn(A) -> R + Sync + Send + 'static,
    ) {
        let f = std::sync::Arc::new(f);
        self.add_local(
            self.new_symbol(
                name,
                // TODO maybe have span as arg
                Span {
                    start: Position::ZERO,
                    end: Position::ZERO,
                    filename: "<rust code>".into(),
                },
            ),
            ValueShape::NativeFunction(NativeFunction::new(
                Name::new(NamePart::Str("<rusty>".into())),
                name,
                FnType {
                    arg: A::ty()
                        .unwrap_or_else(|| Type::new_not_inferred(&format!("{name:?} arg"))),
                    contexts: Contexts::new_not_inferred(),
                    result: R::ty()
                        .unwrap_or_else(|| Type::new_not_inferred(&format!("{name:?} result"))),
                },
                move |_kast: Kast, _fn_type: FnType, arg: Value| {
                    let f = f.clone();
                    async move { Ok(f(A::from_value(arg)?).into_value()) }.boxed()
                },
            ))
            .into(),
        );
    }
}

impl Kast {
    pub fn assign<'a>(
        &'a mut self,
        assignee: &'a AssigneeExpr,
        value: PlaceRef,
    ) -> BoxFuture<'a, eyre::Result<()>> {
        let r#impl = async move {
            match assignee {
                AssigneeExpr::Placeholder { data: _ } => {}
                AssigneeExpr::Unit { data: _ } => {}
                AssigneeExpr::Tuple { tuple, data: _ } => {
                    let value = {
                        let value = value.read_value()?;
                        let value = value.as_inferred()?;
                        let value = value.as_tuple()?;
                        value.inner.as_ref().map(|place| place.get_ref())
                    };
                    for (_field_name, (field_assignee, field_value)) in tuple.as_ref().zip(value)? {
                        self.assign(field_assignee, field_value).await?;
                    }
                }
                AssigneeExpr::Place { place, data: _ } => {
                    let place = self.eval_place(place).await?;
                    place.assign(value.claim_value(self)?)?;
                }
                AssigneeExpr::Let { pattern, data: _ } => {
                    self.pattern_match(pattern, value)?;
                }
            }
            Ok(())
        };
        r#impl.boxed()
    }
    pub fn pattern_match(&mut self, pattern: &Pattern, place: PlaceRef) -> eyre::Result<()> {
        let matches = pattern
            .r#match(place, self)?
            .ok_or_else(|| eyre!("pattern match was not exhaustive???"))?;
        for (binding, value) in matches {
            self.scopes
                .interpreter
                .insert(&binding.symbol, value, binding.mutability);
        }
        Ok(())
    }
    pub fn capture(&self) -> Parc<Scopes> {
        Parc::new(self.scopes.weak_ref())
    }

    #[must_use]
    pub fn enter_recursive_scope(&self) -> Self {
        self.enter_scope_impl(ScopeType::Recursive)
    }
    #[must_use]
    pub fn enter_scope(&self) -> Self {
        self.enter_scope_impl(ScopeType::NonRecursive)
    }

    // This language sucks more than writing Assembly
    #[must_use]
    pub fn enter_scope_impl(&self, ty: ScopeType) -> Self {
        let mut inner = self.clone();
        inner.scopes = Scopes::new(self.spawn_id, ty, Some(self.scopes.clone()));
        inner.interpreter.contexts = Parc::new(Mutex::new(
            self.interpreter.contexts.lock().unwrap().clone(),
        ));
        inner
    }
    #[must_use]
    pub fn with_scopes(&self, scope: Scopes) -> Self {
        let mut kast = self.clone();
        kast.scopes = scope;
        kast
    }
    pub fn add_local(&mut self, symbol: Symbol, value: Value) {
        // self.scopes.interpreter.insert(&symbol, value);
        self.scopes
            .compiler
            .insert(symbol.name(), &symbol.span, value);
    }
    pub fn eval_source<R: SomeExprResult>(
        &mut self,
        source: SourceFile,
        expected_ty: Option<Type>,
    ) -> eyre::Result<R> {
        let ast = ast::parse(&self.syntax, source)?;
        match ast {
            Some(ast) => {
                // sounds like a hack
                let ast = compiler::init_ast(ast);
                // TODO but idr what this todo is about
                futures_lite::future::block_on(self.eval_ast(&ast, expected_ty))
            }
            None => Ok(R::unit()),
        }
    }
    pub async fn eval_ast_opt<R: SomeExprResult>(
        &mut self,
        ast: &Option<Ast>,
        expected_ty: Option<Type>,
    ) -> eyre::Result<R> {
        match ast {
            Some(ast) => self.eval_ast(ast, expected_ty).await,
            None => Ok(R::unit()),
        }
    }
    pub async fn eval_ast<R: SomeExprResult>(
        &mut self,
        ast: &Ast,
        expected_ty: Option<Type>,
    ) -> eyre::Result<R> {
        let mut expr: PlaceExpr = self.compile(ast).await?;
        if let Some(ty) = expected_ty {
            expr.data_mut().ty.make_same(ty)?;
        }
        let result = self.eval_place(&expr).await?;
        Ok(R::from_ref(result, self)?)
    }
    pub fn eval_place<'a>(
        &'a mut self,
        expr: &'a PlaceExpr,
    ) -> BoxFuture<'a, eyre::Result<PlaceRef>> {
        tracing::trace!("evaluating {}", expr.show_short());
        let r#impl = async move {
            let result: PlaceRef = match expr {
                PlaceExpr::Temporary { value, data: _ } => {
                    let value = self.eval(value).await?;
                    OwnedPlace::new_temp(value).get_ref()
                }
                PlaceExpr::Binding { binding, data: _ } => self
                    .scopes
                    .interpreter
                    .get(&binding.symbol)
                    .ok_or_else(|| eyre!("{:?} not found", binding.symbol))?,
                PlaceExpr::FieldAccess {
                    obj,
                    field,
                    data: _,
                } => {
                    let obj = self.eval_place(obj).await?;
                    let obj = obj.read_value()?;
                    let obj = obj.as_inferred()?;
                    let obj = &*obj;
                    match obj {
                        ValueShape::Tuple(TupleValue {
                            inner: fields,
                            name: _,
                            ty: _,
                        }) => match fields.get(field.as_ref()) {
                            Some(place) => place.get_ref(),
                            None => eyre::bail!("{obj} does not have field {field:?}"),
                        },
                        // TODO should this be making a temporary? probably not
                        ValueShape::SyntaxModule(definitions) => OwnedPlace::new_temp(
                            ValueShape::SyntaxDefinition(
                                definitions
                                    .iter() // TODO store a map? iteration is slow?
                                    .find(|def| {
                                        tuple::Member::Named(std::borrow::Cow::Borrowed(&def.name))
                                            == *field
                                    })
                                    .ok_or_else(|| {
                                        eyre!("syntax def {field:?} not found in syntax module")
                                    })?
                                    .clone(),
                            )
                            .into(),
                        )
                        .get_ref(),
                        ValueShape::Target(target) => PlaceRef::new_temp(target.get_field(field)?),
                        _ => eyre::bail!("{obj} is not smth that has fields"),
                    }
                }
                PlaceExpr::Deref { r#ref, data: _ } => {
                    self.eval(r#ref).await?.into_inferred()?.into_ref()?
                }
            };
            Ok::<_, eyre::Report>(result)
        };
        async {
            r#impl
                .await
                .wrap_err_with(|| format!("while evaluating {}", expr.show_short()))
        }
        .boxed()
    }
    pub fn eval_type<'a>(&'a mut self, expr: &'a TypeExpr) -> BoxFuture<'a, eyre::Result<Type>> {
        tracing::trace!("evaluating {}", expr.show_short());
        let r#impl = async move {
            let result: Type = match expr {
                TypeExpr::Ref { inner, data: _ } => {
                    TypeShape::Ref(self.eval_type(inner).await?).into()
                }
                TypeExpr::List { inner, data: _ } => {
                    TypeShape::List(self.eval_type(inner).await?).into()
                }
                TypeExpr::Unit { data: _ } => TypeShape::Unit.into(),
                TypeExpr::Tuple { fields, data: _ } => TypeShape::Tuple(TupleType {
                    name: None.into(),
                    fields: {
                        let mut field_types = Tuple::empty();
                        for (member, field) in fields.as_ref() {
                            field_types.add_member(member, self.eval_type(field).await?);
                        }
                        field_types
                    },
                })
                .into(),
                TypeExpr::Function {
                    arg,
                    result,
                    contexts,
                    data: _,
                } => todo!(),
                TypeExpr::Expr { expr, data: _ } => self.eval(expr).await?.into_type()?,
            };
            Ok::<_, eyre::Report>(result)
        };
        async {
            r#impl
                .await
                .wrap_err_with(|| format!("while evaluating {}", expr.show_short()))
        }
        .boxed()
    }
    pub fn eval<'a>(&'a mut self, expr: &'a Expr) -> BoxFuture<'a, eyre::Result<Value>> {
        tracing::trace!("evaluating {}", expr.show_short());
        let expected_ty = self.interpreter.check_types.then(|| {
            let ty = expr
                .data()
                .ty
                .clone()
                .substitute_bindings(self, &mut RecurseCache::new());
            tracing::trace!("as {ty}");
            ty
        });
        let r#impl = async move {
            let result = match expr {
                Expr::Type { expr, data: _ } => {
                    ValueShape::Type(self.eval_type(expr).await?).into()
                }
                Expr::TargetDependent { branches, data: _ } => {
                    let body = self
                        .select_target_dependent_branch(branches, Target::Interpreter)
                        .await?;
                    self.enter_scope().eval(body).await?
                }
                Expr::Ref { place, data } => {
                    let ref_ty: Type = data
                        .ty
                        .inferred_or_default()?
                        .map_err(|_| eyre!("ref expr type not inferred"))?
                        .into();
                    match ref_ty.inferred().ok().unwrap() {
                        TypeShape::Type => {
                            let inner_ty = self
                                .eval_place(place)
                                .await?
                                .claim_value(self)?
                                .into_type()?;
                            ValueShape::Type(TypeShape::Ref(inner_ty).into()).into()
                        }
                        TypeShape::Ref { .. } => {
                            ValueShape::Ref(self.eval_place(place).await?).into()
                        }
                        inferred => eyre::bail!("ref inferred to be {inferred}"),
                    }
                }
                Expr::ReadPlace { place, data: _ } => {
                    let place = self.eval_place(place).await?;
                    place.claim_value(self)?
                }
                Expr::And { lhs, rhs, data: _ } => {
                    let lhs = self.eval(lhs).await?.into_inferred()?.into_bool()?;
                    if lhs {
                        self.eval(rhs).await?
                    } else {
                        ValueShape::Bool(false).into()
                    }
                }
                Expr::Or { lhs, rhs, data: _ } => {
                    let lhs = self.eval(lhs).await?.into_inferred()?.into_bool()?;
                    if !lhs {
                        self.eval(rhs).await?
                    } else {
                        ValueShape::Bool(true).into()
                    }
                }
                // I understand what it is
                Expr::List {
                    values: values_exprs,
                    data,
                } => {
                    let list_ty: Type = data
                        .ty
                        .inferred_or_default()?
                        .map_err(|_| eyre!("list expr type not inferred"))?
                        .into();
                    let list_ty = list_ty.substitute_bindings(self, &mut RecurseCache::new());
                    match list_ty.inferred().ok().unwrap() {
                        TypeShape::Type => {
                            #[allow(clippy::needless_borrowed_reference)]
                            let &[ref element_ty] =
                                values_exprs.as_slice().try_into().map_err(|_e| {
                                    eyre!("list type must be a list of single element")
                                })?;
                            let element_ty: Type = self.eval(element_ty).await?.into_type()?;
                            ValueShape::Type(TypeShape::List(element_ty).into()).into()
                        }
                        TypeShape::List(element_ty) => {
                            let mut values = Vec::new();
                            for value_expr in values_exprs {
                                values.push(OwnedPlace::new(
                                    self.eval(value_expr).await?,
                                    Mutability::Nested,
                                ));
                            }
                            ValueShape::List(ListValue { values, element_ty }).into()
                        }
                        _ => eyre::bail!("list expr enferred to be {}", data.ty),
                    }
                }
                Expr::Unwind {
                    name,
                    value,
                    data: _,
                } => {
                    let name = self
                        .eval(name)
                        .await?
                        .into_inferred()?
                        .into_unwind_handle()?;
                    let value = self.eval(value).await?;
                    name.sender
                        .lock()
                        .unwrap()
                        .send(value)
                        .map_err(|async_oneshot::Closed()| eyre!("unwindable channel closed"))?;
                    future::pending().await
                }
                Expr::Unwindable {
                    name,
                    body,
                    data: _,
                } => {
                    let (unwind_sender, unwind_receiver) = async_oneshot::oneshot();
                    let executor = async_executor::Executor::new();
                    let mut kast = self.enter_scope();
                    #[allow(clippy::let_and_return)] // lifetime issue otherwise
                    let result = match executor
                        .run({
                            kast.pattern_match(
                                name,
                                OwnedPlace::new_temp(
                                    ValueShape::UnwindHandle(UnwindHandle {
                                        sender: Parc::new(Mutex::new(unwind_sender)),
                                        ty: body.data().ty.clone(),
                                    })
                                    .into(),
                                )
                                .get_ref(),
                            )?;
                            future::select(kast.eval(body), unwind_receiver)
                        })
                        .await
                    {
                        future::Either::Left((result, _unwind_reciever)) => result?,
                        future::Either::Right((result, _body)) => result
                            .map_err(|async_oneshot::Closed()| eyre!("unwind channel closed???"))?,
                    };
                    result
                }
                Expr::CallMacro {
                    r#macro,
                    arg,
                    data: _,
                } => {
                    let r#macro = self.eval(r#macro).await?.into_inferred()?.into_macro()?;
                    let arg = self.eval(arg).await?;
                    self.call_fn(r#macro.f, arg).await?
                }
                Expr::InjectContext { context, data: _ } => {
                    let context = self.eval(context).await?;
                    self.interpreter
                        .contexts
                        .lock()
                        .unwrap()
                        .insert_runtime(context)?;
                    ValueShape::Unit.into()
                }
                Expr::CurrentContext { data } => {
                    let ty = data
                        .ty
                        .clone()
                        .substitute_bindings(self, &mut RecurseCache::new());
                    self.interpreter
                        .contexts
                        .lock()
                        .unwrap()
                        .get_runtime(ty.clone())?
                        .ok_or_else(|| eyre!("{ty} context not available"))?
                }
                Expr::Unit { data } => match data.ty.inferred_or_default()? {
                    Ok(TypeShape::Type) => ValueShape::Type(TypeShape::Unit.into()).into(),
                    Ok(TypeShape::Unit) => ValueShape::Unit.into(),
                    Ok(other) => panic!("unit inferred to be not unit but {other}???"),
                    Err(_) => panic!("unit not inferred"),
                },
                Expr::FunctionType {
                    arg,
                    contexts,
                    result,
                    data: _,
                } => {
                    let arg = self.eval(arg).await?.into_type()?;
                    let contexts = match contexts {
                        Some(contexts) => self
                            .eval(contexts)
                            .await?
                            .into_inferred()?
                            .into_contexts()?,
                        None => Contexts::new_not_inferred(),
                    };
                    let result = self.eval(result).await?.into_type()?;
                    ValueShape::Type(
                        TypeShape::Function(Box::new(FnType {
                            arg,
                            contexts,
                            result,
                        }))
                        .into(),
                    )
                    .into()
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
                    let value: PlaceRef = self.eval_place(value).await?;
                    for branch in branches {
                        if let Some(matches) = branch.pattern.r#match(value.clone(), self)? {
                            let mut kast = self.enter_scope();
                            for (binding, value) in matches {
                                kast.scopes.interpreter.insert(
                                    &binding.symbol,
                                    value,
                                    binding.mutability,
                                );
                            }
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
                    let value = self.eval_place(value).await?;
                    if let Some(matches) = pattern.r#match(value, self)? {
                        for (binding, value) in matches {
                            self.scopes.interpreter.insert(
                                &binding.symbol,
                                value,
                                binding.mutability,
                            );
                        }
                        ValueShape::Bool(true).into()
                    } else {
                        ValueShape::Bool(false).into()
                    }
                }
                Expr::Newtype { def, data: _ } => {
                    let def = self.eval(def).await?;
                    let ty: Type = match def.clone().into_inferred()? {
                        ValueShape::Multiset(values) => {
                            let mut variants = Vec::new();
                            for value in values {
                                let variant = value.into_inferred()?.as_variant()?.clone();
                                variants.push(VariantTypeVariant {
                                    name: variant.name,
                                    value: variant
                                        .value
                                        .map(|value| value.into_value()?.into_type())
                                        .transpose()?
                                        .map(Box::new),
                                });
                            }
                            TypeShape::Variant(VariantType {
                                name: self.current_name.clone(),
                                variants,
                            })
                            .into()
                        }
                        ValueShape::Variant(variant) => TypeShape::Variant(VariantType {
                            name: self.current_name.clone(),
                            variants: vec![VariantTypeVariant {
                                name: variant.name,
                                value: variant
                                    .value
                                    .map(|value| value.into_value()?.into_type())
                                    .transpose()?
                                    .map(Box::new),
                            }],
                        })
                        .into(),
                        ValueShape::Tuple(tuple) => TypeShape::Tuple(TupleType {
                            name: Some(self.current_name.clone()).into(),
                            fields: {
                                let mut fields = Tuple::empty();
                                for (member, value) in tuple.into_values() {
                                    fields.add_member(
                                        member,
                                        self.cache
                                            .compiler
                                            .casts
                                            .lock()
                                            .unwrap()
                                            .cast_to_ty(value)?
                                            .unwrap(),
                                    );
                                }
                                fields
                            },
                        })
                        .into(),
                        _ => eyre::bail!("{def} can not be used in newtype"),
                    };
                    ValueShape::Type(ty).into()
                }
                Expr::MakeMultiset { values, data: _ } => {
                    let mut result = Vec::new();
                    for value in values {
                        result.push(self.eval(value).await?);
                    }
                    ValueShape::Multiset(result).into()
                }
                Expr::Variant { name, value, data } => {
                    let value = match value {
                        Some(value) => Some(self.eval(value).await?),
                        None => None,
                    };
                    ValueShape::Variant(VariantValue {
                        name: name.clone(),
                        value: value.map(|value| OwnedPlace::new(value, Mutability::Nested)),
                        ty: data
                            .ty
                            .clone()
                            .substitute_bindings(self, &mut RecurseCache::new()),
                    })
                    .into()
                }
                Expr::Use { namespace, data } => {
                    let namespace = self.eval(namespace).await?;
                    match namespace.clone().into_inferred()? {
                        ValueShape::Tuple(namespace) => {
                            for (member, value) in namespace.into_values().into_iter() {
                                let name = member
                                    .into_name()
                                    .ok_or_else(|| eyre!("cant use unnamed fields"))?;
                                self.add_local(
                                    self.new_symbol(
                                        name,
                                        // TODO actual original span
                                        data.span.clone(),
                                    ),
                                    value,
                                );
                            }
                        }
                        _ => eyre::bail!("{namespace} is not a namespace"),
                    }
                    ValueShape::Unit.into()
                }
                Expr::Tuple { tuple, data } => {
                    let mut result = Tuple::empty();
                    for (member, field) in tuple.as_ref() {
                        let mut kast = self.clone();
                        kast.current_name =
                            kast.current_name.append(NamePart::Str(format!("{member}")));
                        result.add_member(member, kast.eval(field).await?);
                    }
                    let result_ty = data
                        .ty
                        .clone()
                        .substitute_bindings(self, &mut RecurseCache::new());
                    match result_ty.inferred_or_default()? {
                        Ok(TypeShape::Type) => {
                            let result = ValueShape::Tuple(TupleValue::new_unnamed(
                                result,
                                Type::new_not_inferred("tuple type"),
                            ))
                            .into();
                            self.cache
                                .compiler
                                .casts
                                .lock()
                                .unwrap()
                                .cast(result, &ValueShape::Type(TypeShape::Type.into()).into())?
                                .map_err(|tuple| eyre!("{tuple} can not be cast into type"))?
                        }
                        Ok(TypeShape::Tuple(..)) => {
                            let result = ValueShape::Tuple(TupleValue::new_unnamed(
                                result,
                                result_ty.clone(),
                            ))
                            .into();
                            // println!("{result} :: {result_ty}");
                            result
                        }
                        Ok(ty) => eyre::bail!("tuple expr type inferred as {ty}???"),
                        Err(_) => eyre::bail!("tuple type could not be inferred???"),
                    }
                }
                Expr::Recursive {
                    body,
                    compiler_scope,
                    data,
                } => {
                    let mut inner = self.enter_recursive_scope();
                    inner.eval(body).await?.into_inferred()?.into_unit()?;
                    let mut fields = Tuple::empty();
                    inner.scopes.interpreter.inspect(|locals| {
                        for (name, place) in locals.iter() {
                            fields.add_named(name.name(), place.clone());
                        }
                    });
                    ValueShape::Tuple(TupleValue {
                        name: Some(self.current_name.clone()),
                        inner: fields,
                        ty: data
                            .ty
                            .clone()
                            .substitute_bindings(self, &mut RecurseCache::new()),
                    })
                    .into()
                }
                Expr::Ast {
                    expr_root,
                    definition,
                    values,
                    data,
                    hygiene,
                    def_site,
                } => {
                    let mut ast_values = Tuple::empty();
                    for (member, value) in values.as_ref().into_iter() {
                        let value = self.eval_place(value).await?;
                        let value = value.read_value()?.clone();
                        let value = value.into_inferred()?.into_ast()?;
                        ast_values.add_member(member, value);
                    }
                    let mut ast = Ast::Complex {
                        definition: definition.clone(),
                        values: ast_values,
                        data: AstData {
                            span: data.span.clone(),
                            hygiene: *hygiene,
                            def_site: def_site.clone(),
                        },
                    };
                    if *expr_root {
                        ast = self.set_def_site(&ast);
                    }
                    ValueShape::Ast(ast).into()
                }
                Expr::Function { ty, compiled, data } => ValueShape::Function(TypedFunction {
                    ty: {
                        let ty = ty
                            .clone()
                            .substitute_bindings(self, &mut RecurseCache::new());
                        tracing::trace!("at {} = {ty} ({})", expr.data().span, expr.data().ty);
                        ty
                    },
                    f: Function {
                        id: Id::new(),
                        span: data.span.clone(),
                        name: self.current_name.clone(),
                        captured: self.capture(),
                        compiled: compiled.clone(),
                    },
                })
                .into(),
                Expr::Template { compiled, data } => ValueShape::Template(Function {
                    id: Id::new(),
                    span: data.span.clone(),
                    name: self.current_name.clone(),
                    captured: self.capture(),
                    compiled: compiled.clone(),
                })
                .into(),
                Expr::Scope { expr, data: _ } => {
                    let mut inner = self.enter_scope();
                    inner.eval(expr).await?
                }
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data: _,
                } => {
                    let mut kast = self.enter_scope();
                    let condition = kast.eval(condition).await?.into_inferred()?.into_bool()?;
                    if condition {
                        kast.eval(then_case).await?
                    } else if let Some(else_case) = else_case {
                        kast.eval(else_case).await?
                    } else {
                        ValueShape::Unit.into()
                    }
                }
                Expr::Then { list, data: _ } => {
                    let mut value = ValueShape::Unit.into();
                    for expr in list {
                        value = self.eval(expr).await?;
                    }
                    value
                }
                Expr::Constant { value, data: _ } => match value.clone().inferred() {
                    // TODO SubstituteBindings for value?
                    Some(ValueShape::Type(ty)) => ValueShape::Type(
                        ty.clone()
                            .substitute_bindings(self, &mut RecurseCache::new()),
                    )
                    .into(),
                    _ => value.clone(),
                },
                Expr::Number { raw, data } => match data.ty.inferred_or_default()? {
                    Ok(TypeShape::Int32) => ValueShape::Int32(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int32"))?,
                    )
                    .into(),
                    Ok(TypeShape::Int64) => ValueShape::Int64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int64"))?,
                    )
                    .into(),
                    Ok(TypeShape::Float64) => ValueShape::Float64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as float64"))?,
                    )
                    .into(),
                    Ok(other) => {
                        eyre::bail!("number literals can not be treated as {other}")
                    }
                    Err(_) => eyre::bail!("number literal type could not be inferred"),
                },
                Expr::String { token, data } => {
                    fn make_string(
                        token: &kast_ast::StringToken,
                        ty: &Type,
                    ) -> eyre::Result<Value> {
                        let string_type = token.typ;
                        Ok(match ty.inferred_or_default()? {
                            Ok(ty) => match ty {
                                TypeShape::Char if string_type == ast::StringType::SingleQuoted => {
                                    ValueShape::Char({
                                        let mut chars = token.contents.chars();
                                        let c = chars
                                            .next()
                                            .ok_or_else(|| eyre!("char literal has no content"))?;
                                        if chars.next().is_some() {
                                            eyre::bail!("char literal has more than one char");
                                        }
                                        c
                                    })
                                }
                                TypeShape::String
                                    if string_type == ast::StringType::DoubleQuoted =>
                                {
                                    ValueShape::String(token.contents.clone())
                                }
                                TypeShape::Ref(inner) => {
                                    let value = make_string(token, &inner)?;
                                    ValueShape::Ref(PlaceRef::new_temp(value))
                                }
                                other => eyre::bail!(
                                    "{string_type:?} string literals can not be treated as {other}"
                                ),
                            },
                            Err(_) => {
                                eyre::bail!(
                                    "{string_type:?} string literal type could not be inferred"
                                )
                            }
                        }
                        .into())
                    }
                    make_string(token, &data.ty)?
                }
                Expr::Native {
                    name,
                    compiler_scope: _,
                    data,
                } => {
                    let actual_type = data
                        .ty
                        .clone()
                        .substitute_bindings(self, &mut RecurseCache::new());
                    let native_name = name; // self.eval(name).await?.into_inferred()?.as_str()?.to_owned();
                    tracing::trace!("native {native_name} :: {actual_type}");
                    match self.cache.interpreter.natives.get_named(
                        self.current_name.clone(),
                        native_name.as_str(),
                        actual_type,
                    )? {
                        Some(value) => value,
                        None => eyre::bail!("native {native_name:?} not found"),
                    }
                }
                Expr::Assign {
                    assignee,
                    value,
                    data: _,
                } => {
                    let value = self.eval_place(value).await?;
                    self.assign(assignee, value).await?;
                    ValueShape::Unit.into()
                }
                Expr::Let {
                    is_const_let: _,
                    pattern,
                    value,
                    data: _,
                } => {
                    match value {
                        Some(value) => {
                            let value = self.eval_place(value).await?;
                            self.pattern_match(pattern, value)?;
                        }
                        None => {
                            pattern.collect_bindings(&mut |binding| {
                                tracing::trace!(
                                    "{:?} = <uninitialized> :: {}",
                                    binding.symbol,
                                    binding.ty,
                                );
                                self.scopes.interpreter.insert_uninitialized(
                                    &binding.symbol,
                                    binding.ty.clone(),
                                    binding.mutability,
                                );
                            });
                        }
                    }
                    ValueShape::Unit.into()
                }
                Expr::Call { f, arg, data: _ } => {
                    let f = self.eval(f).await?;
                    let arg = self.eval(arg).await?;
                    self.call(f, arg).await?
                }
                Expr::Instantiate {
                    template,
                    arg,
                    data: _,
                } => {
                    let template = self.eval(template).await?;
                    let arg = self.eval(arg).await?;
                    self.instantiate(template, arg).await?
                }
            };
            if let Some(expected_ty) = expected_ty {
                let should_check_result_ty = match expr {
                    Expr::Unit { .. }
                    | Expr::Ref { .. }
                    | Expr::And { .. }
                    | Expr::Or { .. }
                    | Expr::List { .. }
                    | Expr::Unwind { .. }
                    | Expr::Unwindable { .. }
                    | Expr::CallMacro { .. }
                    | Expr::InjectContext { .. }
                    | Expr::CurrentContext { .. }
                    | Expr::FunctionType { .. }
                    | Expr::Cast { .. }
                    | Expr::Is { .. }
                    | Expr::Match { .. }
                    | Expr::Newtype { .. }
                    | Expr::Variant { .. }
                    | Expr::MakeMultiset { .. }
                    | Expr::Use { .. }
                    | Expr::If { .. }
                    | Expr::Then { .. }
                    | Expr::Constant { .. }
                    | Expr::Number { .. }
                    | Expr::String { .. }
                    | Expr::Native { .. }
                    | Expr::Let { .. }
                    | Expr::Assign { .. }
                    | Expr::Call { .. }
                    | Expr::Scope { .. }
                    | Expr::Function { .. }
                    | Expr::Template { .. }
                    | Expr::Instantiate { .. }
                    | Expr::Tuple { .. }
                    | Expr::ReadPlace { .. }
                    | Expr::TargetDependent { .. }
                    | Expr::Type { .. }
                    | Expr::Ast { .. } => true,
                    // TODO
                    Expr::Recursive { .. } => false,
                };
                let result_ty = result.ty(); // .substitute_bindings(self); // TODO not needed?
                if should_check_result_ty {
                    if let Err(e) = result_ty.clone().make_same(expected_ty.clone()) {
                        tracing::error!("expected {expected_ty}");
                        tracing::error!("result is {result_ty}");
                        eyre::bail!("expr evaluated to incorrent type: {e}");
                    }
                }
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

    pub async fn await_compiled(&self, f: &MaybeCompiledFn) -> eyre::Result<Parc<CompiledFn>> {
        self.cache.executor.advance()?;
        Ok(f.get().await?.clone())
    }

    pub async fn instantiate(&self, template: Value, arg: Value) -> eyre::Result<Value> {
        let template = template.into_inferred()?.into_template()?;
        // TODO memoization
        let mut inner = self.clone();
        inner.current_name = template.name.append(NamePart::Instantiate(arg.clone()));
        let result = inner.call_fn(template, arg).await?;
        Ok(result)
    }

    pub async fn call(&self, f: Value, arg: Value) -> eyre::Result<Value> {
        match f.clone().inferred() {
            Some(ValueShape::Function(f)) => self.call_fn(f.f, arg).await,
            Some(ValueShape::NativeFunction(native)) => {
                native.r#impl.call(self.clone(), native.ty, arg).await
            }
            _ => eyre::bail!("{f} is not a function"),
        }
    }

    pub async fn call_fn(&self, f: Function, arg: Value) -> eyre::Result<Value> {
        let mut kast = self.with_scopes(Scopes::new(
            self.spawn_id,
            ScopeType::NonRecursive,
            Some((*f.captured).clone()),
        ));
        let compiled: Parc<CompiledFn> = self.await_compiled(&f.compiled).await?;
        arg.ty().make_same(
            compiled
                .arg
                .data()
                .ty
                .clone()
                .substitute_bindings(&kast, &mut RecurseCache::new()),
        )?;
        kast.pattern_match(&compiled.arg, OwnedPlace::new_temp(arg).get_ref())?;
        tracing::trace!("calling fn at {}", compiled.body.data().span);
        let value = kast.eval(&compiled.body).await?;
        Ok(value)
    }
}

impl Pattern {
    pub fn r#match(
        &self,
        place: PlaceRef,
        kast: &Kast,
    ) -> eyre::Result<Option<Vec<(Parc<Binding>, Value)>>> {
        let mut result = Vec::new();
        fn match_impl(
            kast: &Kast,
            pattern: &Pattern,
            place: PlaceRef,
            matches: &mut Vec<(Parc<Binding>, Value)>,
        ) -> eyre::Result<Option<()>> {
            match pattern {
                Pattern::Placeholder { data: _ } => {}
                Pattern::Unit { data: _ } => {
                    let value = place.read_value()?;
                    assert_eq!(*value, ValueShape::Unit.into());
                }
                Pattern::Binding {
                    binding,
                    bind_mode,
                    data: _,
                } => {
                    matches.push((
                        binding.clone(),
                        match bind_mode {
                            PatternBindMode::Claim => place.claim_value(kast)?,
                            PatternBindMode::Ref => ValueShape::Ref(place).into(),
                        },
                    ));
                }
                Pattern::Variant {
                    name: name_pattern,
                    value: value_pattern,
                    data: _,
                } => {
                    let (names_match, actual_value) = {
                        let value = place.read_value()?;
                        let value = value.as_inferred()?;
                        let value = value
                            .as_variant()
                            .expect("matching non variant with variant???");
                        let actual_value = value.value.as_ref().map(|place| place.get_ref());
                        (value.name == *name_pattern, actual_value)
                    };
                    if names_match {
                        match (value_pattern, actual_value) {
                            (None, None) => {}
                            (Some(_), None) => panic!("pattern expected a value"),
                            (None, Some(_)) => panic!("pattern did not expect a value"),
                            (Some(value_pattern), Some(value)) => {
                                if match_impl(kast, value_pattern, value, matches)?.is_none() {
                                    return Ok(None);
                                }
                            }
                        }
                    } else {
                        return Ok(None);
                    }
                }
                Pattern::Tuple {
                    tuple: pattern,
                    data: _,
                } => {
                    let value: Tuple<PlaceRef> = {
                        let place = place.read_value()?;
                        let place = place.as_inferred()?;
                        let place = place.as_tuple()?;
                        place.inner.as_ref().map(|place| place.get_ref())
                    };
                    for (_, (field_pattern, field_value)) in pattern
                        .as_ref()
                        .zip(value)
                        .expect("pattern is incorrect structure")
                        .into_iter()
                    {
                        if match_impl(kast, field_pattern, field_value, matches)?.is_none() {
                            return Ok(None);
                        }
                    }
                }
            }
            Ok(Some(()))
        }
        Ok(match_impl(kast, self, place, &mut result)?.map(|()| result))
    }
}
