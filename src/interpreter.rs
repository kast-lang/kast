use super::*;

#[derive(Clone)]
pub struct State {
    pub builtins: Parc<HashMap<String, Builtin>>,
    pub contexts: contexts::State,
}

type Builtin = Box<dyn Fn(Type) -> eyre::Result<Value> + Send + Sync>;

struct NamedBuiltin {
    name: String,
    value: Builtin,
}

pub struct CompletionCandidate {
    pub name: String,
    pub ty: Type,
}

impl State {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let default_number_type = {
            let mut context = Tuple::empty();
            context.add_named(
                "default_number_type",
                Value::NativeFunction(NativeFunction {
                    name: "default_number_type".to_owned(),
                    r#impl: (std::sync::Arc::new(|_kast, _fn_ty, s: Value| {
                        let _s = s.expect_string()?;
                        Ok(Value::Type(Type::new_not_inferred()))
                    }) as std::sync::Arc<NativeFunctionImpl>)
                        .into(),
                    ty: FnType {
                        arg: TypeShape::String.into(),
                        contexts: Contexts::empty(),
                        result: TypeShape::Type.into(),
                    },
                }),
            );
            Value::Tuple(context)
        };
        let (output_context, output_context_type) = {
            let write_type = FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Unit.into(),
            };
            let mut context_type = Tuple::empty();
            context_type.add_named(
                "write",
                TypeShape::Function(Box::new(write_type.clone())).into(),
            );
            let context_type = TypeShape::Tuple(context_type).into();
            let mut context = Tuple::empty();
            context.add_named(
                "write",
                Value::NativeFunction(NativeFunction {
                    name: "print".to_owned(),
                    r#impl: (std::sync::Arc::new(|kast: Kast, _fn_ty, s: Value| {
                        let s = s.expect_string()?;
                        kast.output.write(s);
                        Ok(Value::Unit)
                    }) as std::sync::Arc<NativeFunctionImpl>)
                        .into(),
                    ty: write_type,
                }),
            );
            let context = Value::Tuple(context);
            assert_eq!(context.ty(), context_type);
            (context, context_type)
        };

        Self {
            contexts: {
                let mut contexts = contexts::State::new();
                contexts.insert_runtime(output_context).unwrap();
                contexts
                    .insert_runtime(default_number_type.clone())
                    .unwrap();
                contexts
            },
            builtins: {
                let mut map = HashMap::<String, Builtin>::new();

                let mut insert_value = |name: &str, value: Value| {
                    let expected_ty = value.ty().inferred().unwrap();
                    map.insert(
                        name.to_owned(),
                        Box::new(move |expected: Type| {
                            expected.expect_inferred(expected_ty.clone())?;
                            Ok(value.clone())
                        }),
                    );
                };
                insert_value("true", Value::Bool(true));
                insert_value("false", Value::Bool(false));

                let mut insert_ty = |name: &str, ty: TypeShape| {
                    map.insert(
                        name.to_owned(),
                        Box::new(move |expected: Type| {
                            expected.expect_inferred(TypeShape::Type)?;
                            Ok(Value::Type(ty.clone().into()))
                        }),
                    );
                };
                insert_ty("bool", TypeShape::Bool);
                insert_ty("int32", TypeShape::Int32);
                insert_ty("int64", TypeShape::Int64);
                insert_ty("float64", TypeShape::Float64);
                insert_ty("string", TypeShape::String);
                insert_ty("ast", TypeShape::Ast);
                insert_ty("type", TypeShape::Type);
                insert_ty("symbol", TypeShape::Symbol);
                insert_ty("output", output_context_type.inferred().unwrap());
                // does anyone understand what happened here?
                insert_ty(
                    "default_number_type",
                    default_number_type.ty().inferred().unwrap(),
                );

                map.insert(
                    "dbg_type".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: TypeShape::Type.into(),
                            contexts: Contexts::empty(),
                            result: TypeShape::String.into(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "dbg_type".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, _fn_ty: FnType, value: Value| {
                                let value: Type = value.expect_type()?;
                                Ok(Value::String(value.to_string()))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "dbg_type_of_value".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: Type::new_not_inferred(),
                            contexts: Contexts::empty(),
                            result: TypeShape::String.into(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "dbg_type_of_value".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, _fn_ty: FnType, value: Value| {
                                Ok(Value::String(value.ty().to_string()))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "dbg".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: Type::new_not_inferred(),
                            contexts: Contexts::empty(),
                            result: TypeShape::String.into(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "dbg".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, fn_ty: FnType, value: Value| {
                                let ty = &fn_ty.arg;
                                assert_eq!(&value.ty(), ty);
                                Ok(Value::String(value.to_string()))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "contains".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: TypeShape::Tuple({
                                let mut args = Tuple::empty();
                                args.add_named("s", TypeShape::String.into());
                                args.add_named("substring", TypeShape::String.into());
                                args
                            })
                            .into(),
                            contexts: Contexts::empty(),
                            result: TypeShape::Bool.into(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "contains".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
                                let mut args = args.expect_tuple()?;
                                let s = args.take_named("s").unwrap().expect_string()?;
                                let substring =
                                    args.take_named("substring").unwrap().expect_string()?;
                                Ok(Value::Bool(s.contains(&substring)))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );
                map.insert(
                    "panic".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: TypeShape::String.into(),
                            contexts: Contexts::empty(),    // TODO panic
                            result: TypeShape::Unit.into(), // TODO never type
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "panic".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, _fn_ty, s: Value| {
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
                    "parse".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: TypeShape::String.into(),
                            contexts: Contexts::empty(), // TODO error
                            result: Type::new_not_inferred(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "parse".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, fn_ty: FnType, s: Value| {
                                let s = s.expect_string()?;
                                Ok(match fn_ty.result.inferred() {
                                    Ok(ty) => match ty {
                                        TypeShape::Int32 => Value::Int32(s.parse()?),
                                        TypeShape::Int64 => Value::Int64(s.parse()?),
                                        TypeShape::Float64 => Value::Float64(s.parse()?),
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
                    "gensym".to_owned(),
                    Box::new(|expected: Type| {
                        let ty = FnType {
                            arg: TypeShape::String.into(),
                            contexts: Contexts::empty(),
                            result: TypeShape::Symbol.into(),
                        };
                        expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                        Ok(Value::NativeFunction(NativeFunction {
                            name: "gensym".to_owned(),
                            r#impl: (std::sync::Arc::new(|_kast, _fn_ty, name: Value| {
                                let name = name.expect_string()?;
                                Ok(Value::Symbol(Symbol::new(name)))
                            })
                                as std::sync::Arc<NativeFunctionImpl>)
                                .into(),
                            ty,
                        }))
                    }),
                );

                let mut insert_named = |named: NamedBuiltin| {
                    map.insert(named.name.clone(), named.value);
                };

                fn binary_op_impl(
                    name: &str,
                    result_ty: Option<TypeShape>,
                    f: impl Fn(Value, Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
                ) -> NamedBuiltin {
                    let name = name.to_owned();
                    NamedBuiltin {
                        name: name.clone(),
                        value: Box::new(move |expected: Type| {
                            let operand_type = Type::new_not_inferred();
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_named("lhs", operand_type.clone());
                                    args.add_named("rhs", operand_type.clone());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(), // TODO
                                result: result_ty.clone().map_or(operand_type, Into::into),
                            };
                            expected.expect_inferred(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(Value::NativeFunction(NativeFunction {
                                name: name.clone(),
                                r#impl: (std::sync::Arc::new(move |_kast, _fn_ty, args: Value| {
                                    let [lhs, rhs] =
                                        args.expect_tuple()?.into_named(["lhs", "rhs"])?;
                                    f(lhs, rhs)
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            }))
                        }),
                    }
                }
                fn binary_op(
                    name: &str,
                    f: impl Fn(Value, Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
                ) -> NamedBuiltin {
                    binary_op_impl(name, None, f)
                }

                fn binary_cmp_op(
                    name: &str,
                    f: impl Fn(Value, Value) -> eyre::Result<bool> + Copy + Send + Sync + 'static,
                ) -> NamedBuiltin {
                    binary_op_impl(name, Some(TypeShape::Bool), move |lhs, rhs| {
                        Ok(Value::Bool(f(lhs, rhs)?))
                    })
                }
                macro_rules! binary_cmp_op {
                    ($op:tt) => {
                        insert_named(binary_cmp_op(stringify!($op), |lhs, rhs| {
                            Ok(match (lhs, rhs) {
                                (Value::Int32(lhs), Value::Int32(rhs)) => lhs $op rhs,
                                (Value::Int64(lhs), Value::Int64(rhs)) => lhs $op rhs,
                                (Value::Float64(lhs), Value::Float64(rhs)) => lhs $op rhs,
                                (Value::String(lhs), Value::String(rhs)) => lhs $op rhs,
                                (lhs, rhs) => {
                                    eyre::bail!(
                                        "{:?} doesnt work for {} and {}",
                                        stringify!($op),
                                        lhs.ty(),
                                        rhs.ty(),
                                    )
                                }
                            })
                        }));
                    };
                }
                binary_cmp_op!(<);
                binary_cmp_op!(<=);
                binary_cmp_op!(==);
                binary_cmp_op!(!=);
                binary_cmp_op!(>=);
                binary_cmp_op!(>);

                macro_rules! binary_op {
                    ($op:tt, $method: ident) => {
                        insert_named(binary_op(stringify!($op), |lhs, rhs| {
                            Ok(match (lhs, rhs) {
                                (Value::Int32(lhs), Value::Int32(rhs)) => {
                                    Value::Int32(lhs.$method(rhs).ok_or_else(|| eyre!("overflow"))?)
                                }
                                (Value::Int64(lhs), Value::Int64(rhs)) => {
                                    Value::Int64(lhs.$method(rhs).ok_or_else(|| eyre!("overflow"))?)
                                }
                                (Value::Float64(lhs), Value::Float64(rhs)) => Value::Float64(
                                    lhs $op rhs,
                                ),
                                (lhs, rhs) => {
                                    eyre::bail!(
                                        "{:?} doesnt work for {} and {}",
                                        stringify!($op),
                                        lhs.ty(),
                                        rhs.ty(),
                                    );
                                }
                            })
                        }));
                    };
                }
                binary_op!(-, checked_sub);
                binary_op!(+, checked_add);
                binary_op!(*, checked_mul);
                binary_op!(/, checked_div);
                binary_op!(%, checked_rem);
                Parc::new(map)
            },
        }
    }
}

impl Kast {
    pub fn autocomplete<'a>(&'a self, s: &'a str) -> impl Iterator<Item = CompletionCandidate> {
        self.scopes.interpreter.inspect(|locals| {
            locals
                .iter()
                .filter_map(move |(name, value)| {
                    if name.name().contains(s) {
                        Some(CompletionCandidate {
                            name: name.name().to_owned(),
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
        self.advance_executor();
    }
}

impl Kast {
    pub fn pattern_match(&mut self, pattern: &Pattern, value: Value) -> eyre::Result<()> {
        let matches = pattern
            .r#match(value)
            .ok_or_else(|| eyre!("pattern match was not exhaustive???"))?;
        for (binding, value) in matches {
            self.scopes.interpreter.insert(&binding.symbol, value);
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

    #[must_use]
    pub fn enter_scope_impl(&self, ty: ScopeType) -> Self {
        let mut inner = self.clone();
        inner.scopes = Scopes::new(ty, Some(self.scopes.clone()));
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
        self.scopes.compiler.insert(symbol.name(), value);
    }
    pub fn eval_source(
        &mut self,
        source: SourceFile,
        expected_ty: Option<Type>,
    ) -> eyre::Result<Value> {
        let ast = ast::parse(&self.syntax, source)?;
        match ast {
            Some(ast) => {
                let ast = ast.map_data(|span| AstData {
                    span,
                    hygiene: Hygiene::DefSite,
                    def_site: None,
                });
                // TODO but idr what this todo is about
                futures_lite::future::block_on(self.eval_ast(&ast, expected_ty))
            }
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
        tracing::trace!("evaluating {}", expr.show_short());
        let expected_ty = expr
            .data()
            .ty
            .clone()
            .substitute_bindings(self, &mut RecurseCache::new());
        tracing::trace!("as {expected_ty}");
        let r#impl = async move {
            let result = match expr {
                Expr::Unwind {
                    name,
                    value,
                    data: _,
                } => {
                    let name = self.eval(name).await?.expect_unwind_handle()?;
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
                                Value::UnwindHandle(UnwindHandle {
                                    sender: Parc::new(Mutex::new(unwind_sender)),
                                    ty: body.data().ty.clone(),
                                }),
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
                    let r#macro = self.eval(r#macro).await?.expect_macro()?;
                    let arg = self.eval(arg).await?;
                    self.call_fn(r#macro.f, arg).await?
                }
                Expr::InjectContext { context, data: _ } => {
                    let context = self.eval(context).await?;
                    self.interpreter.contexts.insert_runtime(context)?;
                    Value::Unit
                }
                Expr::CurrentContext { data } => {
                    let ty = data.ty.clone();
                    self.interpreter
                        .contexts
                        .get_runtime(ty.clone())?
                        .ok_or_else(|| eyre!("{ty} context not available"))?
                }
                Expr::Unit { data } => match data.ty.inferred_or_default()? {
                    Ok(TypeShape::Type) => Value::Type(TypeShape::Unit.into()),
                    Ok(TypeShape::Unit) => Value::Unit,
                    Ok(other) => panic!("unit inferred to be not unit but {other}???"),
                    Err(_) => panic!("unit not inferred"),
                },
                Expr::FunctionType {
                    arg,
                    contexts,
                    result,
                    data: _,
                } => {
                    let arg = self.eval(arg).await?.expect_type()?;
                    let contexts = match contexts {
                        Some(contexts) => self.eval(contexts).await?.into_contexts()?,
                        None => Contexts::new_not_inferred(),
                    };
                    let result = self.eval(result).await?.expect_type()?;
                    Value::Type(
                        TypeShape::Function(Box::new(FnType {
                            arg,
                            contexts,
                            result,
                        }))
                        .into(),
                    )
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
                            for (binding, value) in matches {
                                kast.scopes.interpreter.insert(&binding.symbol, value);
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
                    let value = self.eval(value).await?;
                    if let Some(matches) = pattern.r#match(value) {
                        for (binding, value) in matches {
                            self.scopes.interpreter.insert(&binding.symbol, value);
                        }
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
                                let variant = value.expect_variant()?;
                                variants.push(VariantType {
                                    name: variant.name,
                                    value: variant
                                        .value
                                        .map(|value| value.expect_type())
                                        .transpose()?
                                        .map(Box::new),
                                });
                            }
                            TypeShape::Variant(variants).into()
                        }
                        Value::Variant(variant) => TypeShape::Variant(vec![VariantType {
                            name: variant.name,
                            value: variant
                                .value
                                .map(|value| value.expect_type())
                                .transpose()?
                                .map(Box::new),
                        }])
                        .into(),
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
                                self.add_local(Symbol::new(name), value);
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
                        Ok(TypeShape::Type) => self
                            .cache
                            .compiler
                            .casts
                            .lock()
                            .unwrap()
                            .cast(result, &Value::Type(TypeShape::Type.into()))?
                            .map_err(|tuple| eyre!("{tuple} can not be cast into type"))?,
                        Ok(TypeShape::Tuple(..)) => result,
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
                    inner.scopes.interpreter.inspect(|locals| {
                        for (name, value) in locals.iter() {
                            fields.add_named(name.name(), value.clone());
                        }
                    });
                    Value::Tuple(fields)
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
                    for (name, value) in values.as_ref().into_iter() {
                        let value = self.eval(value).await?;
                        let value = value.expect_ast()?;
                        ast_values.add(name, value);
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
                    Value::Ast(ast)
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: _,
                } => Value::Function(TypedFunction {
                    ty: {
                        let ty = ty
                            .clone()
                            .substitute_bindings(self, &mut RecurseCache::new());
                        tracing::trace!("at {} = {ty} ({})", expr.data().span, expr.data().ty);
                        ty
                    },
                    f: Function {
                        id: Id::new(),
                        captured: self.capture(),
                        compiled: compiled.clone(),
                    },
                }),
                Expr::Template { compiled, data: _ } => Value::Template(Function {
                    id: Id::new(),
                    captured: self.capture(),
                    compiled: compiled.clone(),
                }),
                Expr::Scope { expr, data: _ } => {
                    let mut inner = self.enter_scope();
                    inner.eval(expr).await?
                }
                Expr::Binding { binding, data: _ } => self
                    .scopes
                    .interpreter
                    .get(&binding.symbol)
                    .ok_or_else(|| eyre!("{:?} not found", binding.symbol))?
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
                    Value::Type(ty) => Value::Type(
                        ty.clone()
                            .substitute_bindings(self, &mut RecurseCache::new()),
                    ),
                    _ => value.clone(),
                },
                Expr::Number { raw, data } => match data.ty.inferred_or_default()? {
                    Ok(TypeShape::Int32) => Value::Int32(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int32"))?,
                    ),
                    Ok(TypeShape::Int64) => Value::Int64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as int64"))?,
                    ),
                    Ok(TypeShape::Float64) => Value::Float64(
                        raw.parse()
                            .wrap_err_with(|| format!("Failed to parse {raw:?} as float64"))?,
                    ),
                    Ok(other) => {
                        eyre::bail!("number literals can not be treated as {other}")
                    }
                    Err(_) => eyre::bail!("number literal type could not be inferred"),
                },
                Expr::Native { name, data } => {
                    let actual_type = data
                        .ty
                        .clone()
                        .substitute_bindings(self, &mut RecurseCache::new());
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
                    self.pattern_match(pattern, value)?;
                    Value::Unit
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
                    let template = self.eval(template).await?.expect_template()?;
                    let arg = self.eval(arg).await?;
                    self.call_fn(template, arg).await?
                }
            };
            let should_check_result_ty = match expr {
                Expr::Unit { .. }
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
            let result_ty = result.ty(); // .substitute_bindings(self); // TODO not needed?
            if should_check_result_ty {
                result_ty
                    .clone()
                    .make_same(expected_ty)
                    .wrap_err("expr evaluated to incorrect type")?;
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

    pub async fn await_compiled(&self, f: &Function) -> eyre::Result<Parc<CompiledFn>> {
        self.advance_executor();
        match &*f.compiled.lock().unwrap() {
            Some(compiled) => Ok(compiled.clone()),
            None => panic!("function is not compiled yet"),
        }
    }

    pub async fn call(&self, f: Value, arg: Value) -> eyre::Result<Value> {
        match f {
            Value::Function(f) => self.call_fn(f.f, arg).await,
            Value::NativeFunction(native) => (native.r#impl)(self.clone(), native.ty, arg),
            _ => eyre::bail!("{f} is not a function"),
        }
    }

    pub async fn call_fn(&self, f: Function, arg: Value) -> eyre::Result<Value> {
        let mut kast = self.with_scopes(Scopes::new(
            ScopeType::NonRecursive,
            Some((*f.captured).clone()),
        ));
        let compiled: Parc<CompiledFn> = self.await_compiled(&f).await?;
        arg.ty().make_same(
            compiled
                .arg
                .data()
                .ty
                .clone()
                .substitute_bindings(&kast, &mut RecurseCache::new()),
        )?;
        kast.pattern_match(&compiled.arg, arg)?;
        let value = kast.eval(&compiled.body).await?;
        Ok(value)
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
