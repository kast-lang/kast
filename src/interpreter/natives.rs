use super::*;

type Native = Box<dyn Fn(Type) -> eyre::Result<Value> + Send + Sync>;

pub struct Natives {
    natives: Mutex<HashMap<String, Native>>,
    #[allow(dead_code)]
    set_natives: Parc<Mutex<HashMap<String, Value>>>,
}

impl Natives {
    pub fn get(&self, name: &str, ty: Type) -> eyre::Result<Option<Value>> {
        if let Some(value) = self.set_natives.lock().unwrap().get(name) {
            return Ok(Some(value.clone()));
        }
        let natives = self.natives.lock().unwrap();
        let Some(native) = natives.get(name) else {
            return Ok(None);
        };
        Ok(Some(native(ty)?))
    }
}

struct PreparingNatives(HashMap<String, Native>);

trait IntoNative {
    fn into_native(self) -> Native;
}

impl IntoNative for ValueShape {
    fn into_native(self) -> Native {
        let expected_ty = self.ty().inferred().unwrap();
        let value: Value = self.into();
        Box::new(move |expected: Type| {
            expected.infer_as(expected_ty.clone())?;
            Ok(value.clone())
        })
    }
}

impl IntoNative for TypeShape {
    fn into_native(self) -> Native {
        let ty: Type = self.into();
        ty.into_native()
    }
}

impl IntoNative for Type {
    fn into_native(self) -> Native {
        let value: Value = ValueShape::Type(self).into();
        Box::new(move |expected: Type| {
            expected.infer_as(TypeShape::Type)?;
            Ok(value.clone())
        })
    }
}

impl PreparingNatives {
    fn insert(&mut self, name: impl Into<String>, native: impl IntoNative) {
        self.0.insert(name.into(), native.into_native());
    }
    fn insert_fn(
        &mut self,
        name: impl Into<String>,
        ty: impl Fn() -> FnType + Send + Sync + 'static,
        r#impl: impl NativeFunctionClosure,
    ) {
        let r#impl: Parc<dyn NativeFunctionClosure> =
            (std::sync::Arc::new(r#impl) as std::sync::Arc<dyn NativeFunctionClosure>).into();
        let name = name.into();
        self.0.insert(
            name,
            Box::new(move |expected: Type| {
                let ty = ty();
                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                Ok(ValueShape::NativeFunction(NativeFunction {
                    name: "dbg_type".to_owned(),
                    r#impl: r#impl.clone(),
                    ty,
                })
                .into())
            }),
        );
    }
    fn insert_unary_op(
        &mut self,
        name: impl Into<String>,
        f: impl Fn(Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
    ) {
        let name = name.into();
        self.insert_fn(
            name.clone(),
            move || {
                let ty = Type::new_not_inferred(&format!("unary op {name:?} ty"));
                FnType {
                    arg: ty.clone(),
                    contexts: Contexts::empty(), // TODO
                    result: ty.clone(),
                }
            },
            move |_kast, _fn_ty, arg: Value| async move { f(arg) }.boxed(),
        );
    }
    fn insert_binary_op(
        &mut self,
        name: impl Into<String>,
        f: impl Fn(Value, Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
    ) {
        self.insert_binary_op_impl(name, None, f);
    }

    fn insert_binary_op_impl(
        &mut self,
        name: impl Into<String>,
        result_ty: Option<TypeShape>,
        f: impl Fn(Value, Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
    ) {
        let name = name.into();
        self.insert_fn(
            name.clone(),
            move || {
                let operand_type = Type::new_not_inferred("binary op {name:?} operand_type");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_named("lhs", operand_type.clone());
                        args.add_named("rhs", operand_type.clone());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(), // TODO
                    result: result_ty.clone().map_or(operand_type, Into::into),
                }
            },
            move |_kast, _fn_ty, args: Value| {
                async move {
                    let [lhs, rhs] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_named(["lhs", "rhs"])?;
                    f(lhs, rhs)
                }
                .boxed()
            },
        );
    }
    fn insert_binary_cmp_op(
        &mut self,
        name: impl Into<String>,
        f: impl Fn(&Value, &Value) -> eyre::Result<bool> + Copy + Send + Sync + 'static,
    ) {
        self.insert_binary_op_impl(name, Some(TypeShape::Bool), move |lhs, rhs| {
            Ok(ValueShape::Bool(f(&lhs, &rhs)?).into())
        });
    }
}

impl Natives {
    pub fn new() -> Self {
        let set_natives: Parc<Mutex<HashMap<String, Value>>> =
            Parc::new(Mutex::new(HashMap::new()));
        let mut natives = PreparingNatives(HashMap::new());

        natives.insert("true", ValueShape::Bool(true));
        natives.insert("false", ValueShape::Bool(false));

        natives.insert("bool", TypeShape::Bool);
        natives.insert("int32", TypeShape::Int32);
        natives.insert("int64", TypeShape::Int64);
        natives.insert("float64", TypeShape::Float64);
        natives.insert("char", TypeShape::Char);
        natives.insert("string", TypeShape::String);
        natives.insert("ast", TypeShape::Ast);
        natives.insert("type", TypeShape::Type);
        natives.insert("symbol", TypeShape::Symbol);
        natives.insert("output", contexts::default_output().ty());
        natives.insert("filesystem", contexts::default_file_system().ty());
        // does anyone understand what happened here?
        natives.insert("default_number_type", contexts::default_number_type().ty());

        natives.insert_fn(
            "eval_ast",
            || FnType {
                arg: TypeShape::Ast.into(),
                contexts: Contexts::empty(),
                result: Type::new_not_inferred("eval_ast result"),
            },
            |kast: Kast, fn_ty: FnType, ast: Value| {
                async move {
                    let ast = ast.into_inferred()?.into_ast()?;
                    let result_ty = fn_ty.result;
                    let mut kast = kast;
                    kast.eval_ast(&ast, Some(result_ty)).await
                }
                .boxed()
            },
        );

        natives.insert_fn(
            "clone",
            || {
                let item_ty = Type::new_not_inferred("cloned item type");
                FnType {
                    arg: TypeShape::Ref(item_ty.clone()).into(),
                    contexts: Contexts::empty(),
                    result: item_ty.clone(),
                }
            },
            |_kast, _fn_ty: FnType, value: Value| {
                async move {
                    let value_ref = value.into_inferred()?.into_ref()?;
                    Ok(value_ref.clone_value()?)
                }
                .boxed()
            },
        );

        natives.insert_fn(
            "dbg_type",
            || FnType {
                arg: TypeShape::Type.into(),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |_kast, _fn_ty: FnType, value: Value| {
                async move {
                    let value: Type = value.into_type()?;
                    Ok(ValueShape::String(value.to_string()).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "dbg_type_of_value",
            || FnType {
                arg: Type::new_not_inferred("arg of dbg_type_of_value"),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |_kast, _fn_ty: FnType, value: Value| {
                async move { Ok(ValueShape::String(value.ty().to_string()).into()) }.boxed()
            },
        );
        natives.insert_fn(
            "dbg",
            || FnType {
                arg: Type::new_not_inferred("arg of dbg"),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |_kast, fn_ty: FnType, value: Value| {
                async move {
                    let ty = &fn_ty.arg;
                    assert_eq!(&value.ty(), ty);
                    Ok(ValueShape::String(value.to_string()).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "time.now",
            || FnType {
                arg: TypeShape::Unit.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Float64.into(),
            },
            |kast: Kast, _fn_ty: FnType, _value: Value| {
                async move {
                    let now: f64 = kast.cache.start.elapsed().as_secs_f64();
                    Ok(ValueShape::Float64(now.into()).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "HashMap",
            || FnType {
                arg: TypeShape::Tuple({
                    let mut args = Tuple::empty();
                    args.add_unnamed(TypeShape::Type.into());
                    args.add_unnamed(TypeShape::Type.into());
                    args
                })
                .into(),
                contexts: Contexts::empty(),
                result: TypeShape::Type.into(),
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let [key, value] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    Ok(ValueShape::Type(
                        TypeShape::HashMap(HashMapType {
                            key: key.into_type()?,
                            value: value.into_type()?,
                        })
                        .into(),
                    )
                    .into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "HashMap.new",
            || FnType {
                arg: TypeShape::Unit.into(),
                contexts: Contexts::empty(),
                result: TypeShape::HashMap(HashMapType {
                    key: Type::new_not_inferred("HashMap.new key"),
                    value: Type::new_not_inferred("HashMap.new value"),
                })
                .into(),
            },
            move |_kast, fn_ty: FnType, args: Value| {
                async move {
                    args.into_inferred()?.into_unit()?;
                    Ok(ValueShape::HashMap(HashMapValue {
                        values: HashMap::new(),
                        ty: fn_ty.result.expect_inferred()?.expect_hash_map()?,
                    })
                    .into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "HashMap.insert",
            || {
                let key_ty = Type::new_not_inferred("HashMap.insert key");
                let value_ty = Type::new_not_inferred("HashMap.insert value");
                let map_type: Type = TypeShape::HashMap(HashMapType {
                    key: key_ty.clone(),
                    value: value_ty.clone(),
                })
                .into();
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_unnamed(TypeShape::Ref(map_type.clone()).into());
                        args.add_unnamed(key_ty.clone());
                        args.add_unnamed(value_ty.clone());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(),
                    result: TypeShape::Unit.into(),
                }
            },
            move |_kast, _fn_ty, args: Value| {
                async move {
                    let [map, key, value] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let map = map.into_inferred()?.into_ref()?;
                    let mut map = map.write_value()?;
                    let map = map.as_hash_map_mut()?;
                    map.values.insert(
                        HashableValue(key),
                        OwnedPlace::new(value, Mutability::Nested),
                    );
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "HashMap.size",
            || {
                let key_ty = Type::new_not_inferred("HashMap.size key");
                let value_ty = Type::new_not_inferred("HashMap.size value");
                let map_type: Type = TypeShape::HashMap(HashMapType {
                    key: key_ty.clone(),
                    value: value_ty.clone(),
                })
                .into();
                FnType {
                    arg: TypeShape::Ref(map_type.clone()).into(),
                    contexts: Contexts::empty(),
                    result: TypeShape::Int32.into(),
                }
            },
            move |_kast, _fn_ty, args: Value| {
                async move {
                    let map = args.into_inferred()?.into_ref()?;
                    let map = map.read_value()?;
                    let map = map.as_inferred()?;
                    let map = map.as_hash_map()?;
                    Ok(ValueShape::Int32(map.values.len().try_into()?).into())
                }
                .boxed()
            },
        );
        // What's the deal with all these hashmaps?
        natives.insert_fn(
            "HashMap.get",
            || {
                let key_ty = Type::new_not_inferred("HashMap.get key");
                let value_ty = Type::new_not_inferred("HashMap.get value");
                let map_type: Type = TypeShape::HashMap(HashMapType {
                    key: key_ty.clone(),
                    value: value_ty.clone(),
                })
                .into();
                let result_ty = Type::new_not_inferred("HashMap.get result");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_unnamed(TypeShape::Ref(map_type.clone()).into());
                        args.add_unnamed(TypeShape::Ref(key_ty.clone()).into());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(),
                    // TODO use Option
                    result: result_ty.clone(),
                }
            },
            move |_kast, fn_ty: FnType, args: Value| {
                let result_ty = fn_ty.result;
                async move {
                    let [map, key] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let map = map.into_inferred()?.into_ref()?;
                    let map = map.read_value()?;
                    let map = map.as_inferred()?;
                    let map = map.as_hash_map()?;
                    let key = key.into_inferred()?.into_ref()?;
                    let key = key.clone_value()?; // TODO not clone
                    let value_ref = map
                        .values
                        .get(&HashableValue(key))
                        .map(|place| place.get_ref());
                    Ok(ValueShape::Variant(VariantValue {
                        name: match value_ref {
                            Some(_) => "Some",
                            None => "None",
                        }
                        .to_owned(),
                        value: value_ref.map(|value_ref| {
                            OwnedPlace::new(ValueShape::Ref(value_ref).into(), Mutability::Nested)
                        }),
                        ty: result_ty,
                    })
                    .into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "HashMap.into_iter",
            || FnType {
                arg: Type::new_not_inferred("HashMap.into_iter arg"),
                contexts: Contexts::empty(), // TODO generator_handler
                result: TypeShape::Unit.into(),
            },
            {
                let set_natives = set_natives.clone();
                move |kast: Kast, _fn_ty: FnType, value: Value| {
                    let set_natives = set_natives.clone();
                    async move {
                        let map = value.into_inferred()?.into_hash_map()?;
                        let generator_handler = set_natives
                            .lock()
                            .unwrap()
                            .get("generator_handler")
                            .ok_or_else(|| eyre!("generator_handler not set"))?
                            .clone();
                        let generator_handler = kast
                            .instantiate(
                                generator_handler,
                                ValueShape::Type(
                                    TypeShape::Tuple({
                                        let mut tuple = Tuple::empty();
                                        tuple.add_unnamed(map.ty.key.clone());
                                        tuple.add_unnamed(map.ty.value.clone());
                                        tuple
                                    })
                                    .into(),
                                )
                                .into(),
                            )
                            .await?
                            .into_type()?;
                        let handler = kast
                            .interpreter
                            .contexts
                            .lock()
                            .unwrap()
                            .get_runtime(generator_handler)?
                            .ok_or_else(|| eyre!("no handler"))?
                            .into_inferred()?
                            .as_tuple()?
                            .clone()
                            .into_values();
                        // oh shit
                        let handler = handler.get_named("handle").ok_or_else(|| eyre!("wut"))?;
                        for (key, value) in map.values.into_iter() {
                            let HashableValue(key) = key;
                            let mut tuple = Tuple::empty();
                            tuple.add_unnamed(key);
                            tuple.add_unnamed(value.into_value()?);
                            kast.call(handler.clone(), ValueShape::Tuple(tuple.into()).into())
                                .await?;
                        }
                        Ok(ValueShape::Unit.into())
                    }
                    .boxed()
                }
            },
        );
        natives.insert_fn(
            "contains",
            || FnType {
                arg: TypeShape::Tuple({
                    let mut args = Tuple::empty();
                    args.add_named("s", TypeShape::String.into());
                    args.add_named("substring", TypeShape::String.into());
                    args
                })
                .into(),
                contexts: Contexts::empty(),
                result: TypeShape::Bool.into(),
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let mut args = args.into_inferred()?.as_tuple()?.clone().into_values();
                    // TODO do not to_owned
                    let s = args
                        .take_named("s")
                        .unwrap()
                        .into_inferred()?
                        .as_str()?
                        .to_owned();
                    let substring = args
                        .take_named("substring")
                        .unwrap()
                        .into_inferred()?
                        .as_str()?
                        .to_owned();
                    Ok(ValueShape::Bool(s.contains(&substring)).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "loop",
            || FnType {
                arg: TypeShape::Function(Box::new(FnType {
                    arg: TypeShape::Unit.into(),
                    contexts: Contexts::new_not_inferred(),
                    result: TypeShape::Unit.into(),
                }))
                .into(),
                contexts: Contexts::new_not_inferred(),
                result: Type::new_not_inferred("loop result"), // TODO never
            },
            |kast: Kast, _fn_ty, body: Value| {
                async move {
                    loop {
                        kast.call(body.clone(), ValueShape::Unit.into()).await?;
                    }
                    // rust is stupid
                    #[allow(unreachable_code)]
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "list.iter",
            || {
                FnType {
                    arg: TypeShape::Ref(
                        TypeShape::List(Type::new_not_inferred("list.iter elem_ty")).into(),
                    )
                    .into(),
                    contexts: Contexts::new_not_inferred(), // TODO generator_handler
                    result: TypeShape::Unit.into(),
                }
            },
            {
                let set_natives = set_natives.clone();
                move |kast: Kast, _fn_ty: FnType, value: Value| {
                    let set_natives = set_natives.clone();
                    async move {
                        let list = value.into_inferred()?.into_ref()?;
                        let list = list.read_value()?;
                        let list = list.as_inferred()?;
                        let list = list.as_list()?;
                        let generator_handler = set_natives
                            .lock()
                            .unwrap()
                            .get("generator_handler")
                            .ok_or_else(|| eyre!("generator_handler not set"))?
                            .clone();
                        let generator_handler = kast
                            .instantiate(
                                generator_handler,
                                ValueShape::Type(TypeShape::Ref(list.element_ty.clone()).into())
                                    .into(),
                            )
                            .await?
                            .into_type()?;
                        let handler = kast
                            .interpreter
                            .contexts
                            .lock()
                            .unwrap()
                            .get_runtime(generator_handler)?
                            .ok_or_else(|| eyre!("no handler"))?
                            .into_inferred()?
                            .as_tuple()?
                            .clone()
                            .into_values();
                        let handler = handler.get_named("handle").ok_or_else(|| eyre!("wut"))?;
                        for elem in list.values.iter() {
                            kast.call(handler.clone(), ValueShape::Ref(elem.get_ref()).into())
                                .await?;
                        }
                        Ok(ValueShape::Unit.into())
                    }
                    .boxed()
                }
            },
        );
        natives.insert_fn(
            "chars",
            || FnType {
                arg: TypeShape::Ref(TypeShape::String.into()).into(),
                contexts: Contexts::new_not_inferred(), // TODO generator_handler
                result: TypeShape::Unit.into(),
            },
            {
                let set_natives = set_natives.clone();
                move |kast: Kast, _fn_ty: FnType, value: Value| {
                    let set_natives = set_natives.clone();
                    async move {
                        let value = value.into_inferred()?.into_ref()?;
                        let generator_handler = set_natives
                            .lock()
                            .unwrap()
                            .get("generator_handler")
                            .ok_or_else(|| eyre!("generator_handler not set"))?
                            .clone();
                        let generator_handler = kast
                            .instantiate(
                                generator_handler,
                                ValueShape::Type(TypeShape::Char.into()).into(),
                            )
                            .await?
                            .into_type()?;
                        let handler = kast
                            .interpreter
                            .contexts
                            .lock()
                            .unwrap()
                            .get_runtime(generator_handler)?
                            .ok_or_else(|| eyre!("no handler"))?
                            .into_inferred()?
                            .as_tuple()?
                            .clone()
                            .into_values();
                        let handler = handler.get_named("handle").ok_or_else(|| eyre!("wut"))?;
                        let value = value.read_value()?;
                        let value = value.as_inferred()?;
                        let value = value.as_str()?;
                        for c in value.chars() {
                            kast.call(handler.clone(), ValueShape::Char(c).into())
                                .await?;
                        }
                        Ok(ValueShape::Unit.into())
                    }
                    .boxed()
                }
            },
        );
        natives.insert_fn(
            "set_native",
            || FnType {
                arg: TypeShape::Tuple({
                    let mut args = Tuple::empty();
                    args.add_named("name", TypeShape::String.into());
                    args.add_named("value", Type::new_not_inferred("set_natives arg.value"));
                    args
                })
                .into(),
                contexts: Contexts::empty(),
                result: TypeShape::Unit.into(),
            },
            {
                let set_natives = set_natives.clone();
                move |_kast, _fn_ty, args: Value| {
                    let set_natives = set_natives.clone();
                    async move {
                        let [name, value] = args
                            .into_inferred()?
                            .as_tuple()?
                            .clone()
                            .into_values()
                            .into_named(["name", "value"])?;
                        let name = name.into_inferred()?.as_str()?.to_owned();
                        set_natives.lock().unwrap().insert(name, value);
                        Ok(ValueShape::Unit.into())
                    }
                    .boxed()
                }
            },
        );
        natives.insert_fn(
            "list.get",
            || {
                let elem_ty = Type::new_not_inferred("list.get elem_ty");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut tuple = Tuple::empty();
                        tuple.add_unnamed(
                            TypeShape::Ref(TypeShape::List(elem_ty.clone()).into()).into(),
                        );
                        tuple.add_unnamed(TypeShape::Int32.into()); // TODO usize?
                        tuple
                    })
                    .into(),
                    contexts: Contexts::empty(),
                    result: TypeShape::Ref(elem_ty).into(),
                }
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let [list, index] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let list = list.into_inferred()?.into_ref()?;
                    let list = list.read_value()?;
                    let list = list.as_inferred()?;
                    let list = list.as_list()?;
                    let index = index.into_inferred()?.into_int32()?;
                    Ok(ValueShape::Ref(
                        list.values
                            .get(
                                usize::try_from(index)
                                    .map_err(|e| eyre!("incorrect index: {e}"))?,
                            )
                            .ok_or_else(|| eyre!("list index out of bounds"))?
                            .get_ref(),
                    )
                    .into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "list.length",
            || FnType {
                arg: TypeShape::Ref(
                    TypeShape::List(Type::new_not_inferred("list.length elem_ty")).into(),
                )
                .into(),
                contexts: Contexts::empty(),
                result: TypeShape::Int32.into(), // TODO usize?
            },
            |_kast, _fn_ty, list: Value| {
                async move {
                    let list = list.into_inferred()?.into_ref()?;
                    let list = list.read_value()?;
                    let list = list.as_inferred()?;
                    let list = list.as_list()?;
                    Ok(ValueShape::Int32(
                        list.values
                            .len()
                            .try_into()
                            .map_err(|e| eyre!("list length doesnt fit in int32: {e}"))?,
                    )
                    .into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "list.set",
            || {
                let elem_ty = Type::new_not_inferred("list.set elem_ty");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_unnamed(
                            TypeShape::Ref(TypeShape::List(elem_ty.clone()).into()).into(),
                        );
                        args.add_unnamed(TypeShape::Int32.into());
                        args.add_unnamed(elem_ty.clone());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(),
                    result: TypeShape::Unit.into(),
                }
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let [list, index, new_value] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let list_ref = list.into_inferred()?.into_ref()?;
                    let index = index.into_inferred()?.into_int32()?;
                    let index: usize = index.try_into()?;
                    let mut list = list_ref.write()?;
                    let list = list.get_mut()?.as_list_mut()?;
                    list.values
                        .get_mut(index)
                        .ok_or_else(|| eyre!("out of bounds"))?
                        .assign(new_value)?;
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "list.push",
            || {
                let elem_ty = Type::new_not_inferred("list.push elem_ty");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_unnamed(
                            TypeShape::Ref(TypeShape::List(elem_ty.clone()).into()).into(),
                        );
                        args.add_unnamed(elem_ty.clone());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(),
                    result: TypeShape::Unit.into(),
                }
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let [list_ref, new_elem] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let list_ref = list_ref.into_inferred()?.into_ref()?;
                    let mut list = list_ref.write()?;
                    let list = list.get_mut()?.as_list_mut()?;
                    list.values
                        .push(OwnedPlace::new(new_elem, Mutability::Nested));
                    Ok(ValueShape::Unit.into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "push_char",
            || FnType {
                arg: TypeShape::Tuple({
                    let mut args = Tuple::empty();
                    args.add_unnamed(TypeShape::String.into());
                    args.add_unnamed(TypeShape::Char.into());
                    args
                })
                .into(),
                contexts: Contexts::empty(),
                result: TypeShape::String.into(),
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let [s, c] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_unnamed()?;
                    let s = s.into_inferred()?.as_str()?.to_owned();
                    let c = c.into_inferred()?.into_char()?;
                    let mut result = s;
                    result.push(c);
                    Ok(ValueShape::String(result).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "char_ord",
            || FnType {
                arg: TypeShape::Char.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Int32.into(), // TODO UInt32?
            },
            |_kast, _fn_ty, args: Value| {
                async move {
                    let c = args.into_inferred()?.into_char()?;
                    Ok(ValueShape::Int32(u32::from(c).try_into()?).into())
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "panic",
            || FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),    // TODO panic
                result: TypeShape::Unit.into(), // TODO never type
            },
            |_kast, _fn_ty, s: Value| {
                async move {
                    let s = s.into_inferred()?.as_str()?.to_owned();
                    Err(eyre!("panic: {s}"))
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "random",
            || {
                let value_ty = Type::new_not_inferred("random value_ty");
                FnType {
                    arg: TypeShape::Tuple({
                        let mut args = Tuple::empty();
                        args.add_named("min", value_ty.clone());
                        args.add_named("max", value_ty.clone());
                        args
                    })
                    .into(),
                    contexts: Contexts::empty(), // TODO rng
                    result: value_ty,
                }
            },
            |_kast, fn_ty: FnType, args: Value| {
                async move {
                    use rand::prelude::*;
                    let [min, max] = args
                        .into_inferred()?
                        .as_tuple()?
                        .clone()
                        .into_values()
                        .into_named(["min", "max"])?;
                    let min = min.into_inferred()?;
                    let max = max.into_inferred()?;
                    Ok(match fn_ty.result.inferred() {
                        Ok(ty) => Value::from(match ty {
                            TypeShape::Int32 => {
                                let min = min.into_int32()?;
                                let max = max.into_int32()?;
                                ValueShape::Int32(thread_rng().gen_range(min..=max))
                            }
                            TypeShape::Int64 => {
                                let min = min.into_int64()?;
                                let max = max.into_int64()?;
                                ValueShape::Int64(thread_rng().gen_range(min..=max))
                            }
                            TypeShape::Float64 => {
                                let min = min.into_float64()?;
                                let max = max.into_float64()?;
                                ValueShape::Float64(thread_rng().gen_range(min..=max))
                            }
                            _ => eyre::bail!("{ty} is not rngable???"),
                        }),
                        Err(_) => eyre::bail!("cant parse not inferred type"),
                    })
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "parse",
            || {
                FnType {
                    arg: TypeShape::Ref(TypeShape::String.into()).into(),
                    contexts: Contexts::empty(), // TODO error
                    result: Type::new_not_inferred("parse result"),
                }
            },
            |_kast, fn_ty: FnType, s: Value| {
                async move {
                    let s = s.into_inferred()?.into_ref()?;
                    let s = s.read_value()?;
                    let s = s.as_inferred()?;
                    let s = s.as_str()?;
                    Ok(match fn_ty.result.inferred() {
                        Ok(ty) => Value::from(match ty {
                            TypeShape::Int32 => ValueShape::Int32(s.parse()?),
                            TypeShape::Int64 => ValueShape::Int64(s.parse()?),
                            TypeShape::Float64 => ValueShape::Float64(s.parse()?),
                            _ => eyre::bail!("{ty} is not parseable???"),
                        }),
                        Err(_) => {
                            eyre::bail!("cant parse not inferred type")
                        }
                    })
                }
                .boxed()
            },
        );
        natives.insert_fn(
            "gensym",
            || FnType {
                arg: TypeShape::String.into(),
                contexts: Contexts::empty(),
                result: TypeShape::Symbol.into(),
            },
            |_kast, _fn_ty, name: Value| {
                async move {
                    let name = name.into_inferred()?.as_str()?.to_owned();
                    Ok(ValueShape::Symbol(Symbol::new(name)).into())
                }
                .boxed()
            },
        );

        macro_rules! binary_cmp_op {
            ($op:tt, $name:ident) => {
                fn $name(lhs: &Value, rhs: &Value) -> eyre::Result<bool> {
                    let lhs = lhs.as_inferred()?;
                    let lhs = lhs.as_ref()?.read_value()?;
                    let lhs = lhs.as_inferred()?;
                    let rhs = rhs.as_inferred()?;
                    let rhs = rhs.as_ref()?.read_value()?;
                    let rhs = rhs.as_inferred()?;
                    Ok(match (&*lhs, &*rhs) {
                        // TODO types should only implement ==
                        (ValueShape::Type(lhs), ValueShape::Type(rhs)) => {
                            let lhs = lhs.expect_inferred()?;
                            let rhs = rhs.expect_inferred()?;
                            lhs $op rhs
                        }
                        (ValueShape::Bool(lhs), ValueShape::Bool(rhs)) => lhs $op rhs,
                        (ValueShape::Int32(lhs), ValueShape::Int32(rhs)) => lhs $op rhs,
                        (ValueShape::Int64(lhs), ValueShape::Int64(rhs)) => lhs $op rhs,
                        (ValueShape::Float64(lhs), ValueShape::Float64(rhs)) => lhs $op rhs,
                        (ValueShape::Char(lhs), ValueShape::Char(rhs)) => lhs $op rhs,
                        (ValueShape::String(lhs), ValueShape::String(rhs)) => lhs $op rhs,
                        (ValueShape::Tuple(lhs), ValueShape::Tuple(rhs)) => 'result: {
                            for (_name, (lhs, rhs)) in lhs.values()?.zip(rhs.values()?).unwrap() {
                                if *lhs != *rhs {
                                    break 'result $name(&*lhs, &*rhs)?;
                                }
                            }
                            stringify!($op).contains('=')
                        },
                        (lhs, rhs) => {
                            eyre::bail!(
                                "{:?} doesnt work for {} and {}",
                                stringify!($op),
                                lhs.ty(),
                                rhs.ty(),
                            )
                        }
                    })
                }
                natives.insert_binary_cmp_op(stringify!($op), $name);
            };
        }
        binary_cmp_op!(<, lt);
        binary_cmp_op!(<=, le);
        binary_cmp_op!(==, eq);
        binary_cmp_op!(!=, ne);
        binary_cmp_op!(>=, gt);
        binary_cmp_op!(>, ge);

        natives.insert_unary_op("unary -", |value| {
            Ok(match value.clone().into_inferred()? {
                ValueShape::Int32(value) => ValueShape::Int32(-value).into(),
                ValueShape::Int64(value) => ValueShape::Int64(-value).into(),
                ValueShape::Float64(value) => ValueShape::Float64(-value).into(),
                _ => eyre::bail!("unary - doesnt work for {}", value.ty()),
            })
        });
        natives.insert_unary_op("not", |value| {
            Ok(match value.clone().into_inferred()? {
                ValueShape::Bool(x) => ValueShape::Bool(!x).into(),
                _ => eyre::bail!("not doesnt work for {}", value.ty()),
            })
        });

        macro_rules! binary_op {
            ($op:tt, $method: ident) => {
                natives.insert_binary_op(stringify!($op), |lhs, rhs| {
                    Ok(match (lhs.into_inferred()?, rhs.into_inferred()?) {
                        (ValueShape::Int32(lhs), ValueShape::Int32(rhs)) => {
                            ValueShape::Int32(lhs.$method(rhs).ok_or_else(|| eyre!("overflow"))?)
                        }
                        (ValueShape::Int64(lhs), ValueShape::Int64(rhs)) => {
                            ValueShape::Int64(lhs.$method(rhs).ok_or_else(|| eyre!("overflow"))?)
                        }
                        (ValueShape::Float64(lhs), ValueShape::Float64(rhs)) => ValueShape::Float64(
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
                    }.into())
                });
            };
        }
        // I am even worse in other things
        binary_op!(-, checked_sub);
        binary_op!(+, checked_add);
        binary_op!(*, checked_mul);
        binary_op!(/, checked_div);
        binary_op!(%, checked_rem);

        {
            let exec_mode_ty = TypeShape::Variant(vec![
                VariantType {
                    name: "Run".to_owned(),
                    value: None,
                },
                VariantType {
                    name: "Import".to_owned(),
                    value: None,
                },
            ]);
            natives.insert_fn(
                "exec_mode",
                {
                    let exec_mode_ty = exec_mode_ty.clone();
                    move || FnType {
                        arg: TypeShape::Unit.into(),
                        contexts: Contexts::empty(),
                        result: exec_mode_ty.clone().into(),
                    }
                },
                move |kast: Kast, _fn_ty, _: Value| {
                    let exec_mode_ty = exec_mode_ty.clone();
                    async move {
                        Ok(ValueShape::Variant(VariantValue {
                            name: match kast.exec_mode {
                                ExecMode::Run => "Run".into(),
                                ExecMode::Import => "Import".into(),
                            },
                            value: None,
                            ty: exec_mode_ty.clone().into(),
                        })
                        .into())
                    }
                    .boxed()
                },
            );
        };
        Natives {
            natives: Mutex::new(natives.0),
            set_natives,
        }
    }
}
