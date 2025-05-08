use super::*;

#[derive(Clone)]
pub struct State {
    pub contexts: Parc<Mutex<contexts::State>>,
    pub check_types: bool,
}

type Native = Box<dyn Fn(Type) -> eyre::Result<Value> + Send + Sync>;

struct NamedBuiltin {
    name: String,
    value: Native,
}

pub struct CompletionCandidate {
    pub name: String,
    pub ty: Type,
}

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

pub struct Cache {
    pub natives: Natives,
}

impl Cache {
    pub fn new() -> Self {
        let set_natives: Parc<Mutex<HashMap<String, Value>>> =
            Parc::new(Mutex::new(HashMap::new()));
        Self {
            natives: Natives {
                natives: {
                    let mut map = HashMap::<String, Native>::new();

                    let mut insert_value = |name: &str, value: Value| {
                        let expected_ty = value.ty().inferred().unwrap();
                        map.insert(
                            name.to_owned(),
                            Box::new(move |expected: Type| {
                                expected.infer_as(expected_ty.clone())?;
                                Ok(value.clone())
                            }),
                        );
                    };
                    insert_value("true", ValueShape::Bool(true).into());
                    insert_value("false", ValueShape::Bool(false).into());

                    let mut insert_ty = |name: &str, ty: TypeShape| {
                        map.insert(
                            name.to_owned(),
                            Box::new(move |expected: Type| {
                                expected.infer_as(TypeShape::Type)?;
                                Ok(ValueShape::Type(ty.clone().into()).into())
                            }),
                        );
                    };
                    insert_ty("bool", TypeShape::Bool);
                    insert_ty("int32", TypeShape::Int32);
                    insert_ty("int64", TypeShape::Int64);
                    insert_ty("float64", TypeShape::Float64);
                    insert_ty("char", TypeShape::Char);
                    insert_ty("string", TypeShape::String);
                    insert_ty("ast", TypeShape::Ast);
                    insert_ty("type", TypeShape::Type);
                    insert_ty("symbol", TypeShape::Symbol);
                    insert_ty(
                        "output",
                        contexts::default_output().ty().inferred().unwrap(),
                    );
                    insert_ty(
                        "filesystem",
                        contexts::default_file_system().ty().inferred().unwrap(),
                    );
                    // does anyone understand what happened here?
                    insert_ty(
                        "default_number_type",
                        contexts::default_number_type().ty().inferred().unwrap(),
                    );

                    map.insert(
                        "dbg_type".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Type.into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::String.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "dbg_type".to_owned(),
                                r#impl: (std::sync::Arc::new(
                                    |_kast, _fn_ty: FnType, value: Value| {
                                        async move {
                                            let value: Type = value.into_type()?;
                                            Ok(ValueShape::String(value.to_string()).into())
                                        }
                                        .boxed()
                                    },
                                )
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "dbg_type_of_value".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: Type::new_not_inferred("arg of dbg_type_of_value"),
                                contexts: Contexts::empty(),
                                result: TypeShape::String.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "dbg_type_of_value".to_owned(),
                                r#impl: (std::sync::Arc::new(
                                    |_kast, _fn_ty: FnType, value: Value| {
                                        async move {
                                            Ok(ValueShape::String(value.ty().to_string()).into())
                                        }
                                        .boxed()
                                    },
                                )
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "dbg".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: Type::new_not_inferred("arg of dbg"),
                                contexts: Contexts::empty(),
                                result: TypeShape::String.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "dbg".to_owned(),
                                r#impl: (std::sync::Arc::new(
                                    |_kast, fn_ty: FnType, value: Value| {
                                        async move {
                                            let ty = &fn_ty.arg;
                                            assert_eq!(&value.ty(), ty);
                                            Ok(ValueShape::String(value.to_string()).into())
                                        }
                                        .boxed()
                                    },
                                )
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "time.now".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Unit.into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Float64.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "time.now".to_owned(),
                                r#impl: (std::sync::Arc::new(
                                    |kast: Kast, _fn_ty: FnType, _value: Value| {
                                        async move {
                                            let now: f64 = kast.cache.start.elapsed().as_secs_f64();
                                            Ok(ValueShape::Float64(now.into()).into())
                                        }
                                        .boxed()
                                    },
                                )
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "HashMap".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_unnamed(TypeShape::Type.into());
                                    args.add_unnamed(TypeShape::Type.into());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Type.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "HashMap".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
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
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "HashMap.new".to_owned(),
                        Box::new(|expected: Type| {
                            let key_ty = Type::new_not_inferred("HashMap.new key");
                            let value_ty = Type::new_not_inferred("HashMap.new value");
                            let ty = FnType {
                                arg: TypeShape::Unit.into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::HashMap(HashMapType {
                                    key: key_ty.clone(),
                                    value: value_ty.clone(),
                                })
                                .into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "HashMap.new".to_owned(),
                                r#impl: (std::sync::Arc::new(move |_kast, _fn_ty, args: Value| {
                                    let key_ty = key_ty.clone();
                                    let value_ty = value_ty.clone();
                                    async move {
                                        args.into_inferred()?.into_unit()?;
                                        Ok(ValueShape::HashMap(HashMapValue {
                                            values: HashMap::new(),
                                            ty: HashMapType {
                                                key: key_ty.clone(),
                                                value: value_ty.clone(),
                                            },
                                        })
                                        .into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "HashMap.insert".to_owned(),
                        Box::new(|expected: Type| {
                            let key_ty = Type::new_not_inferred("HashMap.insert key");
                            let value_ty = Type::new_not_inferred("HashMap.insert value");
                            let map_type: Type = TypeShape::HashMap(HashMapType {
                                key: key_ty.clone(),
                                value: value_ty.clone(),
                            })
                            .into();
                            let ty = FnType {
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
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "HashMap.insert".to_owned(),
                                r#impl: (std::sync::Arc::new(move |_kast, _fn_ty, args: Value| {
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
                                        map.values
                                            .insert(HashableValue(key), OwnedPlace::new(value));
                                        Ok(ValueShape::Unit.into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "HashMap.size".to_owned(),
                        Box::new(|expected: Type| {
                            let key_ty = Type::new_not_inferred("HashMap.size key");
                            let value_ty = Type::new_not_inferred("HashMap.size value");
                            let map_type: Type = TypeShape::HashMap(HashMapType {
                                key: key_ty.clone(),
                                value: value_ty.clone(),
                            })
                            .into();
                            let ty = FnType {
                                arg: TypeShape::Ref(map_type.clone()).into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Int32.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "HashMap.size".to_owned(),
                                r#impl: (std::sync::Arc::new(move |_kast, _fn_ty, args: Value| {
                                    async move {
                                        let map = args.into_inferred()?.into_ref()?;
                                        let map = map.read_value()?;
                                        let map = map.as_inferred()?;
                                        let map = map.as_hash_map()?;
                                        Ok(ValueShape::Int32(map.values.len().try_into()?).into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    // What's the deal with all these hashmaps?
                    map.insert(
                        "HashMap.get".to_owned(),
                        Box::new(|expected: Type| {
                            let key_ty = Type::new_not_inferred("HashMap.get key");
                            let value_ty = Type::new_not_inferred("HashMap.get value");
                            let map_type: Type = TypeShape::HashMap(HashMapType {
                                key: key_ty.clone(),
                                value: value_ty.clone(),
                            })
                            .into();
                            let result_ty = Type::new_not_inferred("HashMap.get result");
                            let ty = FnType {
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
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "HashMap.get".to_owned(),
                                r#impl: (std::sync::Arc::new(move |_kast, _fn_ty, args: Value| {
                                    let result_ty = result_ty.clone();
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
                                                OwnedPlace::new(ValueShape::Ref(value_ref).into())
                                            }),
                                            ty: result_ty,
                                        })
                                        .into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "HashMap.into_iter".to_owned(),
                        Box::new({
                            let set_natives = set_natives.clone();
                            move |expected: Type| {
                                let ty = FnType {
                                    arg: Type::new_not_inferred("HashMap.into_iter arg"),
                                    contexts: Contexts::empty(), // TODO generator_handler
                                    result: TypeShape::Unit.into(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                let set_natives = set_natives.clone();
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: "HashMap.into_iter".to_owned(),
                                    r#impl: (std::sync::Arc::new(
                                        move |kast: Kast, _fn_ty: FnType, value: Value| {
                                            let set_natives = set_natives.clone();
                                            async move {
                                                let map = value.into_inferred()?.into_hash_map()?;
                                                let generator_handler = set_natives
                                                    .lock()
                                                    .unwrap()
                                                    .get("generator_handler")
                                                    .ok_or_else(|| {
                                                        eyre!("generator_handler not set")
                                                    })?
                                                    .clone();
                                                let generator_handler = kast
                                                    .instantiate(
                                                        generator_handler,
                                                        ValueShape::Type(
                                                            TypeShape::Tuple({
                                                                let mut tuple = Tuple::empty();
                                                                tuple.add_unnamed(
                                                                    map.ty.key.clone(),
                                                                );
                                                                tuple.add_unnamed(
                                                                    map.ty.value.clone(),
                                                                );
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
                                                let handler = handler
                                                    .get_named("handle")
                                                    .ok_or_else(|| eyre!("wut"))?;
                                                for (key, value) in map.values.into_iter() {
                                                    let HashableValue(key) = key;
                                                    let mut tuple = Tuple::empty();
                                                    tuple.add_unnamed(key);
                                                    tuple.add_unnamed(value.into_value()?);
                                                    kast.call(
                                                        handler.clone(),
                                                        ValueShape::Tuple(tuple.into()).into(),
                                                    )
                                                    .await?;
                                                }
                                                Ok(ValueShape::Unit.into())
                                            }
                                            .boxed()
                                        },
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }
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
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "contains".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
                                    async move {
                                        let mut args =
                                            args.into_inferred()?.as_tuple()?.clone().into_values();
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
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "loop".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Function(Box::new(FnType {
                                    arg: TypeShape::Unit.into(),
                                    contexts: Contexts::new_not_inferred(),
                                    result: TypeShape::Unit.into(),
                                }))
                                .into(),
                                contexts: Contexts::new_not_inferred(),
                                result: Type::new_not_inferred("loop result"), // TODO never
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "loop".to_owned(),
                                r#impl: (std::sync::Arc::new(|kast: Kast, _fn_ty, body: Value| {
                                    async move {
                                        loop {
                                            kast.call(body.clone(), ValueShape::Unit.into())
                                                .await?;
                                        }
                                        // rust is stupid
                                        #[allow(unreachable_code)]
                                        Ok(ValueShape::Unit.into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "list_iter".to_owned(),
                        Box::new({
                            let set_natives = set_natives.clone();
                            move |expected: Type| {
                                let set_natives = set_natives.clone();
                                let elem_ty = Type::new_not_inferred("list_iter elem_ty");
                                let ty = FnType {
                                    arg: TypeShape::List(elem_ty).into(),
                                    contexts: Contexts::new_not_inferred(), // TODO generator_handler
                                    result: TypeShape::Unit.into(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: "list_iter".to_owned(),
                                    r#impl: (std::sync::Arc::new(
                                        move |kast: Kast, _fn_ty: FnType, value: Value| {
                                            let set_natives = set_natives.clone();
                                            async move {
                                                let value = value.into_inferred()?.into_list()?;
                                                let generator_handler = set_natives
                                                    .lock()
                                                    .unwrap()
                                                    .get("generator_handler")
                                                    .ok_or_else(|| {
                                                        eyre!("generator_handler not set")
                                                    })?
                                                    .clone();
                                                let generator_handler = kast
                                                    .instantiate(
                                                        generator_handler,
                                                        ValueShape::Type(value.element_ty).into(),
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
                                                let handler = handler
                                                    .get_named("handle")
                                                    .ok_or_else(|| eyre!("wut"))?;
                                                for elem in value.values.iter() {
                                                    kast.call(handler.clone(), elem.clone())
                                                        .await?;
                                                }
                                                Ok(ValueShape::Unit.into())
                                            }
                                            .boxed()
                                        },
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }
                        }),
                    );
                    map.insert(
                        "chars".to_owned(),
                        Box::new({
                            let set_natives = set_natives.clone();
                            move |expected: Type| {
                                let set_natives = set_natives.clone();
                                let ty = FnType {
                                    arg: TypeShape::Ref(TypeShape::String.into()).into(),
                                    contexts: Contexts::new_not_inferred(), // TODO generator_handler
                                    result: TypeShape::Unit.into(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: "chars".to_owned(),
                                    r#impl: (std::sync::Arc::new(
                                        move |kast: Kast, _fn_ty: FnType, value: Value| {
                                            let set_natives = set_natives.clone();
                                            async move {
                                                let value = value.into_inferred()?.into_ref()?;
                                                let generator_handler = set_natives
                                                    .lock()
                                                    .unwrap()
                                                    .get("generator_handler")
                                                    .ok_or_else(|| {
                                                        eyre!("generator_handler not set")
                                                    })?
                                                    .clone();
                                                let generator_handler = kast
                                                    .instantiate(
                                                        generator_handler,
                                                        ValueShape::Type(TypeShape::Char.into())
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
                                                let handler = handler
                                                    .get_named("handle")
                                                    .ok_or_else(|| eyre!("wut"))?;
                                                let value = value.read_value()?;
                                                let value = value.as_inferred()?;
                                                let value = value.as_str()?;
                                                for c in value.chars() {
                                                    kast.call(
                                                        handler.clone(),
                                                        ValueShape::Char(c).into(),
                                                    )
                                                    .await?;
                                                }
                                                Ok(ValueShape::Unit.into())
                                            }
                                            .boxed()
                                        },
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }
                        }),
                    );
                    map.insert(
                        "set_native".to_owned(),
                        Box::new({
                            let set_natives = set_natives.clone();
                            move |expected: Type| {
                                let set_natives = set_natives.clone();
                                let ty = FnType {
                                    arg: TypeShape::Tuple({
                                        let mut args = Tuple::empty();
                                        args.add_named("name", TypeShape::String.into());
                                        args.add_named(
                                            "value",
                                            Type::new_not_inferred("set_natives arg.value"),
                                        );
                                        args
                                    })
                                    .into(),
                                    contexts: Contexts::empty(),
                                    result: TypeShape::Unit.into(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: "set_native".to_owned(),
                                    r#impl: (std::sync::Arc::new(
                                        move |_kast, _fn_ty, args: Value| {
                                            let set_natives = set_natives.clone();
                                            async move {
                                                let [name, value] = args
                                                    .into_inferred()?
                                                    .as_tuple()?
                                                    .clone()
                                                    .into_values()
                                                    .into_named(["name", "value"])?;
                                                let name =
                                                    name.into_inferred()?.as_str()?.to_owned();
                                                set_natives.lock().unwrap().insert(name, value);
                                                Ok(ValueShape::Unit.into())
                                            }
                                            .boxed()
                                        },
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }
                        }),
                    );
                    map.insert(
                        "list_get".to_owned(),
                        Box::new(|expected: Type| {
                            let elem_ty = Type::new_not_inferred("list_get elem_ty");
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut tuple = Tuple::empty();
                                    tuple.add_unnamed(TypeShape::List(elem_ty.clone()).into());
                                    tuple.add_unnamed(TypeShape::Int32.into()); // TODO usize?
                                    tuple
                                })
                                .into(),
                                contexts: Contexts::empty(),
                                result: elem_ty,
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "list_get".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
                                    async move {
                                        let [list, index] = args
                                            .into_inferred()?
                                            .as_tuple()?
                                            .clone()
                                            .into_values()
                                            .into_unnamed()?;
                                        let list = list.into_inferred()?.into_list()?;
                                        let index = index.into_inferred()?.into_int32()?;
                                        Ok(list
                                            .values
                                            .get(
                                                usize::try_from(index)
                                                    .map_err(|e| eyre!("incorrect index: {e}"))?,
                                            )
                                            .ok_or_else(|| eyre!("list index out of bounds"))?
                                            .clone())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "list_length".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::List(Type::new_not_inferred("list_length elem_ty"))
                                    .into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Int32.into(), // TODO usize?
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "list_length".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, list: Value| {
                                    async move {
                                        let list = list.into_inferred()?.into_list()?;
                                        Ok(ValueShape::Int32(
                                            list.values.len().try_into().map_err(|e| {
                                                eyre!("list length doesnt fit in int32: {e}")
                                            })?,
                                        )
                                        .into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "list_set".to_owned(),
                        Box::new(|expected: Type| {
                            let elem_ty = Type::new_not_inferred("list_set elem_ty");
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_unnamed(
                                        TypeShape::Ref(TypeShape::List(elem_ty.clone()).into())
                                            .into(),
                                    );
                                    args.add_unnamed(TypeShape::Int32.into());
                                    args.add_unnamed(elem_ty.clone());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Unit.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "list_set".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
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
                                        *list
                                            .values
                                            .get_mut(index)
                                            .ok_or_else(|| eyre!("out of bounds"))? = new_value;
                                        Ok(ValueShape::Unit.into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "list_push".to_owned(),
                        Box::new(|expected: Type| {
                            let elem_ty = Type::new_not_inferred("list_push elem_ty");
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_unnamed(
                                        TypeShape::Ref(TypeShape::List(elem_ty.clone()).into())
                                            .into(),
                                    );
                                    args.add_unnamed(elem_ty.clone());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Unit.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "list_push".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
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
                                        list.values.push(new_elem);
                                        Ok(ValueShape::Unit.into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "push_char".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_unnamed(TypeShape::String.into());
                                    args.add_unnamed(TypeShape::Char.into());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::String.into(),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "push_char".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
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
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "char_ord".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Char.into(),
                                contexts: Contexts::empty(),
                                result: TypeShape::Int32.into(), // TODO UInt32?
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "char_ord".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, args: Value| {
                                    async move {
                                        let c = args.into_inferred()?.into_char()?;
                                        Ok(ValueShape::Int32(u32::from(c).try_into()?).into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "panic".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::String.into(),
                                contexts: Contexts::empty(), // TODO panic
                                result: TypeShape::Unit.into(), // TODO never type
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "panic".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, s: Value| {
                                    async move {
                                        let s = s.into_inferred()?.as_str()?.to_owned();
                                        Err(eyre!("panic: {s}"))
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "random".to_owned(),
                        Box::new(|expected: Type| {
                            let value_ty = Type::new_not_inferred("random value_ty");
                            let ty = FnType {
                                arg: TypeShape::Tuple({
                                    let mut args = Tuple::empty();
                                    args.add_named("min", value_ty.clone());
                                    args.add_named("max", value_ty.clone());
                                    args
                                })
                                .into(),
                                contexts: Contexts::empty(), // TODO rng
                                result: value_ty,
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "random".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, fn_ty: FnType, args: Value| {
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
                                                    ValueShape::Int32(
                                                        thread_rng().gen_range(min..=max),
                                                    )
                                                }
                                                TypeShape::Int64 => {
                                                    let min = min.into_int64()?;
                                                    let max = max.into_int64()?;
                                                    ValueShape::Int64(
                                                        thread_rng().gen_range(min..=max),
                                                    )
                                                }
                                                TypeShape::Float64 => {
                                                    let min = min.into_float64()?;
                                                    let max = max.into_float64()?;
                                                    ValueShape::Float64(
                                                        thread_rng().gen_range(min..=max),
                                                    )
                                                }
                                                _ => eyre::bail!("{ty} is not rngable???"),
                                            }),
                                            Err(_) => eyre::bail!("cant parse not inferred type"),
                                        })
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );
                    map.insert(
                        "parse".to_owned(),
                        Box::new(|expected: Type| {
                            let ty = FnType {
                                arg: TypeShape::Ref(TypeShape::String.into()).into(),
                                contexts: Contexts::empty(), // TODO error
                                result: Type::new_not_inferred("parse result"),
                            };
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "parse".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, fn_ty: FnType, s: Value| {
                                    async move {
                                        let s = s.into_inferred()?.into_ref()?;
                                        let s = s.read_value()?;
                                        let s = s.as_inferred()?;
                                        let s = s.as_str()?;
                                        Ok(match fn_ty.result.inferred() {
                                            Ok(ty) => Value::from(match ty {
                                                TypeShape::Int32 => ValueShape::Int32(s.parse()?),
                                                TypeShape::Int64 => ValueShape::Int64(s.parse()?),
                                                TypeShape::Float64 => {
                                                    ValueShape::Float64(s.parse()?)
                                                }
                                                _ => eyre::bail!("{ty} is not parseable???"),
                                            }),
                                            Err(_) => {
                                                eyre::bail!("cant parse not inferred type")
                                            }
                                        })
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
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
                            expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                            Ok(ValueShape::NativeFunction(NativeFunction {
                                name: "gensym".to_owned(),
                                r#impl: (std::sync::Arc::new(|_kast, _fn_ty, name: Value| {
                                    async move {
                                        let name = name.into_inferred()?.as_str()?.to_owned();
                                        Ok(ValueShape::Symbol(Symbol::new(name)).into())
                                    }
                                    .boxed()
                                })
                                    as std::sync::Arc<NativeFunctionImpl>)
                                    .into(),
                                ty,
                            })
                            .into())
                        }),
                    );

                    let mut insert_named = |named: NamedBuiltin| {
                        map.insert(named.name.clone(), named.value);
                    };

                    fn unary_op(
                        name: &str,
                        f: impl Fn(Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
                    ) -> NamedBuiltin {
                        let name = name.to_owned();
                        NamedBuiltin {
                            name: name.clone(),
                            value: Box::new(move |expected: Type| {
                                let ty = Type::new_not_inferred(&format!("unary op {name:?} ty"));
                                let ty = FnType {
                                    arg: ty.clone(),
                                    contexts: Contexts::empty(), // TODO
                                    result: ty.clone(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: name.clone(),
                                    r#impl: (std::sync::Arc::new(
                                        move |_kast, _fn_ty, arg: Value| {
                                            async move { f(arg) }.boxed()
                                        },
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }),
                        }
                    }
                    fn binary_op_impl(
                        name: &str,
                        result_ty: Option<TypeShape>,
                        f: impl Fn(Value, Value) -> eyre::Result<Value> + Copy + Send + Sync + 'static,
                    ) -> NamedBuiltin {
                        let name = name.to_owned();
                        NamedBuiltin {
                            name: name.clone(),
                            value: Box::new(move |expected: Type| {
                                let operand_type =
                                    Type::new_not_inferred("binary op {name:?} operand_type");
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
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: name.clone(),
                                    r#impl: (std::sync::Arc::new(
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
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
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
                        f: impl Fn(&Value, &Value) -> eyre::Result<bool> + Copy + Send + Sync + 'static,
                    ) -> NamedBuiltin {
                        binary_op_impl(name, Some(TypeShape::Bool), move |lhs, rhs| {
                            Ok(ValueShape::Bool(f(&lhs, &rhs)?).into())
                        })
                    }
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
                        insert_named(binary_cmp_op(stringify!($op), $name));
                    };
                }
                    binary_cmp_op!(<, lt);
                    binary_cmp_op!(<=, le);
                    binary_cmp_op!(==, eq);
                    binary_cmp_op!(!=, ne);
                    binary_cmp_op!(>=, gt);
                    binary_cmp_op!(>, ge);
                    insert_named(unary_op("unary -", |value| {
                        Ok(match value.clone().into_inferred()? {
                            ValueShape::Int32(value) => ValueShape::Int32(-value).into(),
                            ValueShape::Int64(value) => ValueShape::Int64(-value).into(),
                            ValueShape::Float64(value) => ValueShape::Float64(-value).into(),
                            _ => eyre::bail!("unary - doesnt work for {}", value.ty()),
                        })
                    }));
                    insert_named(unary_op("not", |value| {
                        Ok(match value.clone().into_inferred()? {
                            ValueShape::Bool(x) => ValueShape::Bool(!x).into(),
                            _ => eyre::bail!("not doesnt work for {}", value.ty()),
                        })
                    }));

                    macro_rules! binary_op {
                    ($op:tt, $method: ident) => {
                        insert_named(binary_op(stringify!($op), |lhs, rhs| {
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
                        }));
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
                        map.insert(
                            "exec_mode".to_owned(),
                            Box::new(move |expected: Type| {
                                let exec_mode_ty = exec_mode_ty.clone();
                                let ty = FnType {
                                    arg: TypeShape::Unit.into(),
                                    contexts: Contexts::empty(),
                                    result: exec_mode_ty.clone().into(),
                                };
                                expected.infer_as(TypeShape::Function(Box::new(ty.clone())))?;
                                Ok(ValueShape::NativeFunction(NativeFunction {
                                    name: "exec_mode".to_owned(),
                                    r#impl: (std::sync::Arc::new(
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
                                    )
                                        as std::sync::Arc<NativeFunctionImpl>)
                                        .into(),
                                    ty,
                                })
                                .into())
                            }),
                        );
                    };
                    Mutex::new(map)
                },
                set_natives,
            },
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
    pub fn autocomplete<'a>(&'a self, s: &'a str) -> impl Iterator<Item = CompletionCandidate> {
        self.scopes.interpreter.inspect(|locals| {
            locals
                .iter()
                .filter_map(move |(name, place)| {
                    if name.name().contains(s) {
                        Some(CompletionCandidate {
                            name: name.name().to_owned(),
                            ty: place.ty.clone(),
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
            Symbol::new(name),
            ValueShape::NativeFunction(NativeFunction {
                name: name.into(),
                ty: FnType {
                    arg: A::ty()
                        .unwrap_or_else(|| Type::new_not_inferred(&format!("{name:?} arg"))),
                    contexts: Contexts::new_not_inferred(),
                    result: R::ty()
                        .unwrap_or_else(|| Type::new_not_inferred(&format!("{name:?} result"))),
                },
                r#impl: (std::sync::Arc::new(move |_kast: Kast, _fn_type: FnType, arg: Value| {
                    let f = f.clone();
                    async move { Ok(f(A::from_value(arg)?).into_value()) }.boxed()
                }) as std::sync::Arc<NativeFunctionImpl>)
                    .into(),
            })
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
                        value.0.as_ref().map(|place| place.get_ref())
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
        self.scopes.compiler.insert(symbol.name(), value);
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
                    OwnedPlace::new(value).get_ref()
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
                        ValueShape::Tuple(TupleValue(fields)) => match fields.get_named(field) {
                            Some(place) => place.get_ref(),
                            None => eyre::bail!("{obj} does not have field {field:?}"),
                        },
                        // TODO should this be making a temporary?
                        ValueShape::SyntaxModule(definitions) => OwnedPlace::new(
                            ValueShape::SyntaxDefinition(
                                definitions
                                    .iter() // TODO store a map? iteration is slow?
                                    .find(|def| def.name == *field)
                                    .ok_or_else(|| {
                                        eyre!("syntax def {field:?} not found in syntax module")
                                    })?
                                    .clone(),
                            )
                            .into(),
                        )
                        .get_ref(),
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
                                values.push(self.eval(value_expr).await?);
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
                                OwnedPlace::new(
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
                    let value = self.eval_place(value).await?;
                    if let Some(matches) = pattern.r#match(value, self)? {
                        for (binding, value) in matches {
                            self.scopes.interpreter.insert(&binding.symbol, value);
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
                                variants.push(VariantType {
                                    name: variant.name,
                                    value: variant
                                        .value
                                        .map(|value| value.into_value()?.into_type())
                                        .transpose()?
                                        .map(Box::new),
                                });
                            }
                            TypeShape::Variant(variants).into()
                        }
                        ValueShape::Variant(variant) => TypeShape::Variant(vec![VariantType {
                            name: variant.name,
                            value: variant
                                .value
                                .map(|value| value.into_value()?.into_type())
                                .transpose()?
                                .map(Box::new),
                        }])
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
                        value: value.map(OwnedPlace::new),
                        ty: data.ty.clone(),
                    })
                    .into()
                }
                Expr::Use { namespace, data: _ } => {
                    let namespace = self.eval(namespace).await?;
                    match namespace.clone().into_inferred()? {
                        ValueShape::Tuple(namespace) => {
                            for (name, value) in namespace.into_values().into_iter() {
                                let name = name.ok_or_else(|| eyre!("cant use unnamed fields"))?;
                                self.add_local(Symbol::new(name), value);
                            }
                        }
                        _ => eyre::bail!("{namespace} is not a namespace"),
                    }
                    ValueShape::Unit.into()
                }
                Expr::Tuple { tuple, data } => {
                    let mut result = Tuple::empty();
                    for (name, field) in tuple.as_ref() {
                        result.add(name, self.eval(field).await?);
                    }
                    let result = ValueShape::Tuple(result.into()).into();
                    match data.ty.inferred_or_default()? {
                        Ok(TypeShape::Type) => self
                            .cache
                            .compiler
                            .casts
                            .lock()
                            .unwrap()
                            .cast(result, &ValueShape::Type(TypeShape::Type.into()).into())?
                            .map_err(|tuple| eyre!("{tuple} can not be cast into type"))?,
                        Ok(TypeShape::Tuple(..)) => result,
                        Ok(ty) => eyre::bail!("tuple expr type inferred as {ty}???"),
                        Err(_) => eyre::bail!("tuple type could not be inferred???"),
                    }
                }
                Expr::Recursive { body, data: _ } => {
                    let mut inner = self.enter_recursive_scope();
                    inner.eval(body).await?.into_inferred()?.into_unit()?;
                    let mut fields = Tuple::empty();
                    inner.scopes.interpreter.inspect(|locals| {
                        for (name, place) in locals.iter() {
                            fields.add_named(name.name(), place.clone());
                        }
                    });
                    ValueShape::Tuple(TupleValue(fields)).into()
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
                        let value = self.eval_place(value).await?;
                        let value = value.read_value()?.clone();
                        let value = value.into_inferred()?.into_ast()?;
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
                    ValueShape::Ast(ast).into()
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: _,
                } => ValueShape::Function(TypedFunction {
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
                })
                .into(),
                Expr::Template { compiled, data: _ } => ValueShape::Template(Function {
                    id: Id::new(),
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
                Expr::Native { name, data } => {
                    let actual_type = data
                        .ty
                        .clone()
                        .substitute_bindings(self, &mut RecurseCache::new());
                    let name = self.eval(name).await?.into_inferred()?.as_str()?.to_owned();
                    tracing::trace!("native {name} :: {actual_type}");
                    match self
                        .cache
                        .interpreter
                        .natives
                        .get(name.as_str(), actual_type)?
                    {
                        Some(value) => value,
                        None => eyre::bail!("native {name:?} not found"),
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
                                self.scopes
                                    .interpreter
                                    .insert_uninitialized(&binding.symbol, binding.ty.clone());
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
        match &*f.lock().unwrap() {
            Some(compiled) => Ok(compiled.clone()),
            None => {
                eyre::bail!("function is not compiled yet")
            }
        }
    }

    pub async fn instantiate(&self, template: Value, arg: Value) -> eyre::Result<Value> {
        let template = template.into_inferred()?.into_template()?;
        // TODO memoization
        self.call_fn(template, arg).await
    }

    pub async fn call(&self, f: Value, arg: Value) -> eyre::Result<Value> {
        match f.clone().inferred() {
            Some(ValueShape::Function(f)) => self.call_fn(f.f, arg).await,
            Some(ValueShape::NativeFunction(native)) => {
                (native.r#impl)(self.clone(), native.ty, arg).await
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
        kast.pattern_match(&compiled.arg, OwnedPlace::new(arg).get_ref())?;
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
                        place.0.as_ref().map(|place| place.get_ref())
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
