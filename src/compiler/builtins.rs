use eyre::OptionExt;

use super::*;

pub type BuiltinMacro = for<'a> fn(
    kast: &'a mut Kast,
    cty: CompiledType,
    ast: &'a Ast,
) -> BoxFuture<'a, eyre::Result<Compiled>>;

pub struct Builtins {
    builtin_macros: HashMap<&'static str, BuiltinMacro>,
}

macro_rules! assert_expr {
    ($this:expr, $cty:expr, $ast:expr) => {
        let cty = $cty;
        let ast = $ast;
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                $this.compile(ast).await?,
            )));
        }
        if cty != CompiledType::Expr {
            eyre::bail!("Expected expr");
        }
    };
}

impl Builtins {
    pub fn get(&self, macro_name: &str) -> Option<&BuiltinMacro> {
        self.builtin_macros.get(macro_name)
    }
    pub fn new() -> Self {
        let mut builtin_macros = HashMap::new();
        macro_rules! populate {
            ($($name:ident),*$(,)?) => {
                $(
                    let name = stringify!($name).strip_prefix("macro_").unwrap();
                    let closure: BuiltinMacro = |kast: &mut Kast, cty, ast| Builtins::$name(kast, cty, ast).boxed();
                    builtin_macros.insert(name, closure);
                )*
            }
        }
        populate!(
            macro_type,
            macro_native,
            macro_type_ascribe,
            macro_context_ascribe,
            macro_const_let,
            macro_let,
            macro_call,
            macro_then,
            macro_if,
            macro_match,
            macro_target_dependent,
            macro_variant,
            macro_newtype,
            macro_merge,
            macro_scope,
            macro_macro,
            macro_function_def,
            macro_struct_def,
            macro_tuple,
            macro_field,
            macro_quote,
            macro_field_access,
            macro_function_type,
            macro_make_unit,
            macro_use,
            macro_syntax_module,
            macro_impl_syntax,
            macro_import,
            macro_include,
            macro_template_def,
            macro_instantiate_template,
            macro_placeholder,
            macro_is,
            macro_cast,
            macro_impl_cast,
            macro_with_context,
            macro_current_context,
            macro_comptime,
            macro_include_ast,
            macro_call_macro,
            macro_unwindable,
            macro_unwind,
            macro_list,
            macro_mutable_pattern,
            macro_assign,
            macro_and,
            macro_or,
            macro_ref,
            macro_deref,
            macro_ref_pattern,
            macro_typeof,
        );
        Self { builtin_macros }
    }

    async fn macro_type_ascribe(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let [value, ty] = values
            .as_ref()
            .into_named(["value", "type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ty = kast
            .eval_ast::<Value>(ty, Some(TypeShape::Type.into()))
            .await
            .wrap_err_with(|| "Failed to evaluate the type")?
            .into_type()?;
        let mut value = kast.compile_into(cty, value).await?;
        value.ty_mut().make_same(ty)?;
        Ok(value)
    }
    async fn macro_context_ascribe(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let [expr, contexts] = values
            .as_ref()
            .into_named(["expr", "contexts"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let contexts = kast
            .eval_ast::<Value>(contexts, None) // TODO contexts
            .await
            .wrap_err_with(|| "Failed to evaluate the contexts")?
            .into_inferred()?
            .into_contexts()?;
        let mut expr = kast.compile_into(cty, expr).await?;
        let data: &mut ExprData = match &mut expr {
            Compiled::Expr(e) => e.data_mut(),
            Compiled::PlaceExpr(e) => e.data_mut(),
            Compiled::TypeExpr(_) => todo!(),
            Compiled::AssigneeExpr(_) => todo!(),
            Compiled::Pattern(_) => todo!(),
        };
        data.contexts.0.make_same(contexts.0)?;
        Ok(expr)
    }
    async fn macro_const_let(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [pattern, value_ast] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = kast.compile(pattern).await?;
        let value = {
            let mut kast = kast.clone();
            if let Pattern::Binding { binding, .. } = &pattern {
                kast.current_name = kast
                    .current_name
                    .append(NamePart::Symbol(binding.symbol.clone()));
            }

            kast.eval_ast::<Value>(value_ast, Some(pattern.data().ty.clone()))
                .await?
        };

        // TODO don't clone value
        let matches = pattern
            .r#match(OwnedPlace::new_temp(value.clone()).get_ref(), kast)?
            .ok_or_else(|| eyre!("pattern match was not exhaustive???"))?;
        for (binding, value) in matches {
            kast.scopes
                .compiler
                .insert(binding.symbol.name(), &binding.symbol.span, value);
        }

        let value = Box::new(PlaceExpr::new_temp(
            Expr::Constant {
                value,
                data: value_ast.data().span.clone(),
            }
            .init(kast)
            .await?,
        ));
        Ok(Compiled::Expr(
            Expr::Let {
                is_const_let: true,
                pattern,
                value: Some(value),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_let(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                kast.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let pattern = values
            .as_ref()
            .into_single_named("pattern")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = kast.compile(pattern).await?;
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(
                AssigneeExpr::Let {
                    pattern,
                    data: span,
                }
                .init(kast)
                .await?,
            ),
            CompiledType::Expr => {
                kast.inject_bindings(&pattern);
                Compiled::Expr(
                    Expr::Let {
                        is_const_let: false,
                        pattern,
                        value: None,
                        data: span,
                    }
                    .init(kast)
                    .await?,
                )
            }
            _ => eyre::bail!("must be assignee"),
        })
    }
    async fn macro_type(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([], [expr]) = values
            .as_ref()
            .into_named_opt([], ["expr"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            match expr {
                Some(expr) => Expr::Type {
                    expr: kast.compile(expr).await?,
                    data: span,
                },
                None => Expr::Constant {
                    value: ValueShape::Type(TypeShape::Type.into()).into(),
                    data: span,
                },
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_native(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let name = values
            .as_ref()
            .into_single_named("name")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        // let name = kast.compile(name).await?;
        let name = kast
            .eval_ast::<Value>(name, Some(TypeShape::String.into()))
            .await?
            .into_inferred()?
            .into_string()?;
        Ok(Compiled::Expr(
            Expr::Native {
                // name: Box::new(name),
                name,
                compiler_scope: kast.scopes.compiler.clone(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_newtype(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let def = values
            .as_ref()
            .into_single_named("def")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Newtype {
                def: Box::new(kast.compile(def).await?),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_merge(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (macro_name, span) = match ast {
            Ast::Complex {
                definition,
                data: AstData { span, .. },
                ..
            } => (definition.name.as_str(), span.clone()),
            _ => unreachable!(),
        };
        let ListCollected {
            list: value_asts,
            all_binary: _,
        } = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut values = Vec::new();
        for value_ast in value_asts {
            values.push(kast.compile(value_ast).await?);
        }
        Ok(match cty {
            CompiledType::Expr => {
                Compiled::Expr(Expr::MakeMultiset { values, data: span }.init(kast).await?)
            }
            CompiledType::TypeExpr => todo!(),
            CompiledType::Pattern => todo!(),
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::AssigneeExpr => eyre::bail!("not assignee"),
        })
    }
    async fn macro_variant(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                kast.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let ([name], [ty, value]) = values
            .as_ref()
            .into_named_opt(["name"], ["type", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let (inline_ty, name): (Option<Type>, &str) = match name {
            kast_ast::Ast::Simple {
                token: kast_ast::Token::Ident { name, .. },
                ..
            } => (None, name),
            kast_ast::Ast::Complex {
                definition,
                values,
                data: _,
            } if definition.name == "builtin macro field_access" => {
                let [ty, name] = values.as_ref().into_named(["obj", "field"])?;
                let ty = kast
                    .compile::<Expr>(ty)
                    .await?
                    .auto_instantiate(kast)
                    .await?;
                ty.data().ty.infer_as(TypeShape::Type)?;
                let ty = kast.eval(&ty).await?.into_type()?;
                (Some(ty), name.as_ident().ok_or_eyre("expected ident")?)
            }
            _ => eyre::bail!("{} is not expected here", name.show_short()),
        };
        if ty.is_some() && inline_ty.is_some() {
            eyre::bail!("both ty and inline ty cant be");
        }
        let ty = match ty {
            None => inline_ty,
            Some(ty) => Some({
                let ty = kast
                    .compile::<Expr>(ty)
                    .await?
                    .auto_instantiate(kast)
                    .await?;
                ty.data().ty.infer_as(TypeShape::Type)?;
                kast.eval(&ty).await?.into_type()?
            }),
        };
        Ok(match cty {
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::AssigneeExpr => eyre::bail!("not assignee"),
            CompiledType::TypeExpr => todo!(),
            CompiledType::Expr => Compiled::Expr({
                let mut expr = Expr::Variant {
                    name: name.to_owned(),
                    value: match value {
                        Some(value) => Some(Box::new(kast.compile(value).await?)),
                        None => None,
                    },
                    data: span,
                }
                .init(kast)
                .await?;
                if let Some(ty) = ty {
                    expr.data_mut().ty.make_same(ty)?;
                }
                expr
            }),
            CompiledType::Pattern => Compiled::Pattern({
                let mut pattern = Pattern::Variant {
                    name: name.to_owned(),
                    value: match value {
                        Some(value) => Some(Box::new(kast.compile(value).await?)),
                        None => None,
                    },
                    data: span,
                }
                .init()?;
                if let Some(ty) = ty {
                    pattern.data_mut().ty.make_same(ty)?;
                }
                pattern
            }),
        })
    }
    /// Sarah is kinda cool
    /// Kappa
    /// NoKappa
    async fn macro_match(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, branches] = values
            .as_ref()
            .into_named(["value", "branches"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ListCollected {
            list: branch_asts,
            all_binary: _,
        } = ListCollector {
            macro_name: "builtin macro merge",
            a: "a",
            b: "b",
        }
        .collect(branches)?;
        let mut branches = Vec::new();
        for branch in branch_asts {
            match branch {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == "builtin macro function_def" => {
                    let [arg, body] = values.as_ref().into_named(["arg", "body"])?;
                    let mut kast = kast.enter_scope();
                    let pattern = kast.compile(arg).await?;
                    branches.push(MatchBranch {
                        body: {
                            let mut kast = kast.enter_scope();
                            kast.inject_bindings(&pattern);
                            kast.compile(body).await?
                        },
                        pattern,
                    });
                }
                _ => eyre::bail!("match branches wrong syntax"),
            }
        }
        Ok(Compiled::Expr(
            Expr::Match {
                value: Box::new(kast.compile(value).await?),
                branches,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_target_dependent(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let branches = values
            .as_ref()
            .into_single_named("branches")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ListCollected {
            list: branch_asts,
            all_binary: _,
        } = ListCollector {
            macro_name: "builtin macro merge",
            a: "a",
            b: "b",
        }
        .collect(branches)?;
        let mut branches = Vec::<TargetDependentBranch<Expr>>::new();
        for branch in branch_asts {
            match branch {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == "builtin macro function_def" => {
                    let [arg, body] = values.as_ref().into_named(["arg", "body"])?;
                    branches.push(TargetDependentBranch {
                        condition: kast
                            .with_scopes(kast.cache.target_dependent_scopes.clone())
                            .enter_scope()
                            .compile(arg)
                            .await?,
                        body: kast.enter_scope().compile(body).await?,
                    });
                }
                _ => eyre::bail!("match branches wrong syntax"),
            }
        }
        Ok(Compiled::Expr(
            Expr::TargetDependent {
                branches,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_if(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([cond, then_case], [else_case]) = values
            .as_ref()
            .into_named_opt(["cond", "then"], ["else"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut then_scope = kast.enter_scope();
        let cond: Expr = then_scope.compile(cond).await?;
        Ok(Compiled::Expr(
            Expr::If {
                then_case: {
                    then_scope.inject_conditional_bindings(&cond, true);
                    Box::new(then_scope.compile(then_case).await?)
                },
                else_case: match else_case {
                    Some(else_case) => Some({
                        let mut else_scope = kast.enter_scope();
                        else_scope.inject_conditional_bindings(&cond, false);
                        Box::new(else_scope.compile(else_case).await?)
                    }),
                    None => None,
                },
                condition: Box::new(cond),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_then(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let macro_name = match ast {
            Ast::Complex { definition, .. } => definition.name.as_str(),
            _ => unreachable!(),
        };
        assert_expr!(kast, cty, ast);
        let ListCollected {
            list: ast_list,
            all_binary,
        } = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut expr_list = Vec::with_capacity(ast_list.len());
        for ast in ast_list {
            expr_list.push(kast.compile(ast).await?);
        }
        let expr = Expr::Then {
            list: expr_list,
            data: ast.data().span.clone(),
        }
        .init(kast)
        .await?;
        if !all_binary {
            expr.data().ty.infer_as(TypeShape::Unit)?;
        }
        Ok(Compiled::Expr(expr))
    }
    async fn macro_impl_syntax(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let [def, r#impl] = values
            .as_ref()
            .into_named(["def", "impl"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let def = kast
            .eval_ast::<Value>(def, Some(TypeShape::SyntaxDefinition.into()))
            .await?
            .into_inferred()?
            .into_syntax_definition()?;
        tracing::trace!("defined syntax {:?}", def.name);
        let r#impl = kast.eval_ast(r#impl, None).await?; // TODO should be a macro?
        kast.cache
            .compiler
            .syntax_definitions
            .lock()
            .unwrap()
            .insert(def, std::task::Poll::Ready(r#impl)); // TODO check previous value?
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Unit.into(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_syntax_module(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = kast.enter_recursive_scope();
        inner
            .eval_ast::<Value>(body, Some(TypeShape::Unit.into()))
            .await?
            .into_inferred()?
            .into_unit()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::SyntaxModule(Parc::new(inner.scope_syntax_definitions())).into(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_struct_def(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = kast.enter_recursive_scope();
        let body = inner.compile(body).await?;
        Ok(Compiled::Expr(
            Expr::Recursive {
                body: Box::new(body),
                compiler_scope: inner.scopes.compiler.clone(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_macro(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let def = values
            .as_ref()
            .into_single_named("def")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        // TODO expect some type here?
        let def = kast.eval_ast::<Value>(def, None).await?;
        let def = def.into_inferred()?.into_function()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Macro(def).into(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_template_def(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let ([arg, body], [r#where]) = values
            .as_ref()
            .into_named_opt(["arg", "body"], ["where"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let _ = r#where; // TODO

        let mut inner = kast.enter_scope();
        let arg: Pattern = inner.compile(arg).await?;
        inner.inject_bindings(&arg);
        let compiled = inner.compile_fn_body(
            arg,
            body,
            Contexts::empty(),
            Type::new_not_inferred("template result"),
        );
        Ok(Compiled::Expr(
            Expr::Template {
                compiled,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_instantiate_template(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let [template, arg] = values
            .as_ref()
            .into_named(["template", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let template = kast.compile(template).await?;
        let arg = kast.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Instantiate {
                template: Box::new(template),
                arg: Box::new(arg),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_function_def(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let ([body], [arg, contexts, result_type]) = values
            .as_ref()
            .into_named_opt(["body"], ["arg", "contexts", "result_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = kast.enter_scope();
        let arg: Pattern = match arg {
            Some(arg) => inner.compile(arg).await?,
            None => Pattern::Unit { data: span.clone() }.init()?,
        };
        inner.inject_bindings(&arg);
        let arg_ty = arg.data().ty.clone();
        let result_type = match result_type {
            Some(ast) => inner
                .eval_ast::<Value>(ast, Some(TypeShape::Type.into()))
                .await?
                .into_type()?,
            None => Type::new_not_inferred(&format!("result type of fn at {span}")),
        };
        let contexts = match contexts {
            Some(contexts) => kast
                .eval_ast::<Value>(
                    contexts, None, // TODO Contexts??
                )
                .await?
                .into_inferred()?
                .into_contexts()?,
            None => Contexts::new_not_inferred(),
        };
        // todon't: Contexts!!
        let compiled = inner.compile_fn_body(arg, body, contexts.clone(), result_type.clone());
        Ok(Compiled::Expr(
            Expr::Function {
                ty: FnType {
                    arg: arg_ty,
                    contexts,
                    result: result_type,
                },
                compiled,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_scope(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [expr] = values
            .as_ref()
            .into_unnamed()
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        tracing::trace!("compiling scoped: {expr}");
        // TODO probably need to enter scope in all cases?
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(kast.compile(expr).await?),
            CompiledType::PlaceExpr => Compiled::PlaceExpr(kast.compile(expr).await?),
            CompiledType::TypeExpr => Compiled::TypeExpr(kast.compile(expr).await?),
            CompiledType::Expr => {
                let expr = kast.enter_scope().compile(expr).await?;
                Compiled::Expr(
                    Expr::Scope {
                        expr: Box::new(expr),
                        data: span,
                    }
                    .init(kast)
                    .await?,
                )
            }
            CompiledType::Pattern => Compiled::Pattern(kast.compile(expr).await?),
        })
    }
    async fn macro_import(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let path = kast
            .eval_ast::<Value>(
                values.as_ref().into_single_named("path")?,
                Some(TypeShape::String.into()),
            )
            .await?
            .into_inferred()?
            .as_str()?
            .to_owned();
        let path = if path.starts_with('.') {
            ast.data()
                .span
                .filename
                .parent()
                .expect("no parent dir??")
                .join(path)
        } else {
            todo!("absolute import")
        };
        let value: Value = kast.import(path)?;
        Ok(Compiled::Expr(
            Expr::Constant { value, data: span }.init(kast).await?,
        ))
    }
    async fn macro_include(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let path = kast
            .eval_ast::<Value>(
                values.as_ref().into_single_named("path")?,
                Some(TypeShape::String.into()),
            )
            .await?
            .into_inferred()?
            .as_str()?
            .to_owned();
        let path = if path.starts_with('.') {
            ast.data()
                .span
                .filename
                .parent()
                .expect("no parent dir??")
                .join(path)
        } else {
            todo!("absolute include")
        };
        Ok(match kast.include(path)? {
            Some(ast) => kast.compile_into(cty, &ast).await?,
            None => Compiled::Expr(Expr::Unit { data: span }.init(kast).await?),
        })
    }
    async fn macro_use(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let namespace = values.as_ref().into_single_named("namespace")?;
        let namespace: Value = kast.eval_ast(namespace, None).await?;
        match namespace.clone().inferred() {
            Some(ValueShape::Tuple(namespace)) => {
                for (member, value) in namespace.into_values().into_iter() {
                    let name = member
                        .into_name()
                        .ok_or_else(|| eyre!("cant use unnamed fields"))?;
                    kast.add_local(
                        kast.new_symbol(
                            name,
                            span.clone(), // TODO actual original span
                        ),
                        value,
                    );
                }
            }
            _ => eyre::bail!("{namespace} is not a namespace"),
        }
        Ok(Compiled::Expr(
            Expr::Use {
                namespace: Box::new(
                    Expr::Constant {
                        value: namespace,
                        data: span.clone(),
                    }
                    .init(kast)
                    .await?,
                ),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_make_unit(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [] = values.as_ref().into_named([])?;
        Ok(match cty {
            CompiledType::AssigneeExpr => {
                Compiled::AssigneeExpr(AssigneeExpr::Unit { data: span }.init(kast).await?)
            }
            CompiledType::PlaceExpr => Compiled::PlaceExpr(PlaceExpr::new_temp(
                Expr::Unit { data: span }.init(kast).await?,
            )),
            CompiledType::Pattern => Compiled::Pattern(Pattern::Unit { data: span }.init()?),
            CompiledType::Expr => Compiled::Expr(Expr::Unit { data: span }.init(kast).await?),
            CompiledType::TypeExpr => {
                Compiled::TypeExpr(TypeExpr::Unit { data: span }.init(kast).await?)
            }
        })
    }
    async fn macro_call(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let [f, arg] = values
            .as_ref()
            .into_named(["f", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let f = kast.compile(f).await?;
        let arg = kast.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Call {
                f: Box::new(f),
                arg: Box::new(arg),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_call_macro(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let [r#macro, arg] = values
            .as_ref()
            .into_named(["macro", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let r#macro = kast.compile(r#macro).await?;
        let arg = kast.compile(arg).await?;
        // println!("evaled = {:?}", kast.eval(&arg).await.unwrap());
        Ok(Compiled::Expr(
            Expr::CallMacro {
                r#macro: Box::new(r#macro),
                arg: Box::new(arg),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_unwindable(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [name, body] = values.as_ref().into_named(["name", "body"])?;
        let (name, body) = {
            let mut kast = kast.enter_scope();
            let name: Pattern = kast.compile(name).await?;
            kast.inject_bindings(&name);
            let body = kast.compile(body).await?;
            (name, body)
        };
        Ok(Compiled::Expr(
            Expr::Unwindable {
                name,
                body: Box::new(body),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_unwind(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [name, value] = values.as_ref().into_named(["name", "value"])?;
        let name: Expr = kast.compile(name).await?;
        let value: Expr = kast.compile(value).await?;
        Ok(Compiled::Expr(
            Expr::Unwind {
                name: Box::new(name),
                value: Box::new(value),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    /// this function might succeed (no promises)
    async fn macro_function_type(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([arg, result], [contexts]) = values
            .as_ref()
            .into_named_opt(["arg", "result"], ["contexts"])?;
        Ok(Compiled::Expr(
            Expr::FunctionType {
                arg: Box::new(kast.compile(arg).await?),
                contexts: match contexts {
                    Some(contexts) => Some(Box::new(kast.compile(contexts).await?)),
                    None => None,
                },
                result: Box::new(kast.compile(result).await?),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_field_access(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        if cty == CompiledType::Expr {
            return Ok(Compiled::Expr(kast.compile::<PlaceExpr>(ast).await?.into()));
        }
        assert_eq!(cty, CompiledType::PlaceExpr);
        let (values, span) = get_complex(ast);
        let [obj, field] = values.as_ref().into_named(["obj", "field"])?;
        let field = ast_as_member(field)
            .ok_or_else(|| eyre!("expected a member (ident or index), got {field}"))?;
        // My hair is very crusty today
        Ok(Compiled::PlaceExpr(
            PlaceExpr::FieldAccess {
                obj: Box::new(kast.compile(obj).await?),
                field: field.into_owned(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_quote(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let expr = values.as_ref().into_single_named("expr")?;
        struct State {
            root: bool,
            level: usize,
        }
        fn unquote_level(ast: &Ast) -> eyre::Result<(usize, &Ast)> {
            if let Ast::Complex {
                definition,
                values,
                data: _,
            } = ast
            {
                if definition.name == "builtin macro unquote" {
                    let expr = values
                        .as_ref()
                        .into_single_named("expr")
                        .wrap_err_with(|| "wrong args to unquote")?;
                    let (level, expr) = unquote_level(expr)?;
                    return Ok((level + 1, expr));
                }
            }
            Ok((0, ast))
        }
        fn quote<'a>(
            kast: &'a mut Kast,
            state: State,
            ast: &'a Ast,
        ) -> BoxFuture<'a, eyre::Result<PlaceExpr>> {
            async move {
                Ok(match ast {
                    Ast::Complex {
                        definition,
                        values,
                        data:
                            AstData {
                                span,
                                hygiene,
                                def_site,
                            },
                    } => {
                        if definition.name == "builtin macro unquote" {
                            let (unquote_level, expr) = unquote_level(ast)?;
                            if unquote_level > state.level {
                                eyre::bail!("too much unquote");
                            }
                            if unquote_level == state.level {
                                return Ok(kast.compile(expr).await?);
                            }
                        }
                        PlaceExpr::new_temp(
                            Expr::Ast {
                                expr_root: state.root,
                                definition: definition.clone(),
                                values: {
                                    let mut result = Tuple::empty();
                                    for (member, value) in values.as_ref().into_iter() {
                                        let value = quote(
                                            kast,
                                            State {
                                                level: if definition.name == "builtin macro quote" {
                                                    state.level + 1
                                                } else {
                                                    state.level
                                                },
                                                root: false,
                                            },
                                            value,
                                        )
                                        .boxed()
                                        .await?;
                                        result.add_member(member, value);
                                    }
                                    result
                                },
                                hygiene: *hygiene,
                                def_site: def_site.clone(),
                                data: span.clone(),
                            }
                            .init(kast)
                            .await?,
                        )
                    }
                    _ => PlaceExpr::new_temp(
                        Expr::Constant {
                            value: ValueShape::Ast(match state.root {
                                true => kast.set_def_site(ast),
                                false => ast.clone(),
                            })
                            .into(),
                            data: ast.data().span.clone(),
                        }
                        .init(kast)
                        .await?,
                    ),
                })
            }
            .boxed()
        }
        Ok(Compiled::Expr(
            // TODO `($x) moves x
            Expr::ReadPlace {
                place: quote(
                    kast,
                    State {
                        root: true,
                        level: 1,
                    },
                    expr,
                )
                .await?,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_field(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        Self::macro_tuple(kast, cty, ast).await
    }
    async fn macro_tuple(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                kast.compile(ast).await?,
            )));
        }
        let ListCollected {
            list: fields,
            all_binary: _,
        } = ListCollector {
            macro_name: "builtin macro tuple",
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut tuple = Tuple::empty();
        for field in fields {
            match field {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == "builtin macro field" => {
                    let ([name], [value]) = values
                        .as_ref()
                        .into_named_opt(["name"], ["value"])
                        .wrap_err_with(|| "field macro wrong args")?;
                    let (name, ty): (&Ast, Option<Type>) = match name {
                        Ast::Complex {
                            definition,
                            values,
                            data: _,
                        } if definition.name == "builtin macro type_ascribe" => {
                            let [name, ty] = values
                                .as_ref()
                                .into_named(["value", "type"])
                                .wrap_err_with(|| "type ascribe macro wrong args")?;
                            (
                                name,
                                Some(
                                    kast.eval_ast::<Value>(ty, Some(TypeShape::Type.into()))
                                        .await?
                                        .into_type()?,
                                ),
                            )
                        }
                        _ => (name, None),
                    };
                    let value = value.unwrap_or(name);
                    let name_span = name.data().span.clone();
                    let name = name
                        .as_ident()
                        .ok_or_else(|| eyre!("{name} is not an ident"))?
                        .to_owned();
                    let mut compiled = kast.compile_into(cty, value).await?;
                    let ty = match ty {
                        Some(ty) => ty,
                        None => Type::new_not_inferred(&format!("Field {name} at {name_span}")),
                    };
                    compiled.ty_mut().make_same(ty)?;
                    tuple.add_named(name, compiled);
                }
                _ => tuple.add_unnamed(kast.compile_into(cty, field).await?),
            }
        }
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(
                AssigneeExpr::Tuple {
                    tuple: tuple.map(|field| field.expect_assignee_expr().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init(kast)
                .await?,
            ),
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::Expr => Compiled::Expr(
                Expr::Tuple {
                    tuple: tuple.map(|field| field.expect_expr().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init(kast)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(
                Pattern::Tuple {
                    tuple: tuple.map(|field| field.expect_pattern().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init()?,
            ),
            CompiledType::TypeExpr => unreachable!(),
        })
    }
    async fn macro_is(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(kast, cty, ast);
        let [value, pattern] = values
            .as_ref()
            .into_named(["value", "pattern"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Is {
                value: Box::new(kast.compile(value).await?),
                pattern: kast.compile(pattern).await?,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_cast(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, target] = values
            .as_ref()
            .into_named(["value", "target"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Cast {
                value: Box::new(kast.compile(value).await?),
                target: {
                    tracing::trace!("cast target: {target}");
                    let target = kast.eval_ast(target, None).await?;
                    tracing::trace!(" = {target}");
                    target
                },
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_impl_cast(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, target, r#impl] = values
            .as_ref()
            .into_named(["value", "target", "impl"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let value = kast.eval_ast::<Value>(value, None).await?;
        let target = kast.eval_ast::<Value>(target, None).await?;
        let mut r#impl: Expr = kast.compile(r#impl).await?;
        let impl_ty = kast
            .cast_result_ty(|| future::ready(Ok(value.clone())).boxed(), target.clone())
            .await?;
        // right now I have a small brain moment
        r#impl.data_mut().ty.make_same(impl_ty)?;
        let r#impl = {
            let mut kast = kast.clone();
            kast.current_name = Name::new(NamePart::ImplCast {
                value: value.clone(),
                target: target.clone(),
            });
            kast.eval(&r#impl).await?
        };
        kast.cache
            .compiler
            .casts
            .lock()
            .unwrap()
            .impl_cast(value, target, r#impl)?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Unit.into(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_placeholder(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                kast.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let [] = values
            .as_ref()
            .into_named([])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(match cty {
            CompiledType::AssigneeExpr => {
                Compiled::AssigneeExpr(AssigneeExpr::Placeholder { data: span }.init(kast).await?)
            }
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::Expr => Compiled::Expr(
                Expr::Constant {
                    value: Value::new_not_inferred(&format!("placeholder at {span}")),
                    data: span,
                }
                .init(kast)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(Pattern::Placeholder { data: span }.init()?),
            CompiledType::TypeExpr => Compiled::TypeExpr(
                TypeExpr::Expr {
                    expr: Box::new(
                        Expr::Constant {
                            value: Value::new_not_inferred(&format!("placeholder at {span}")),
                            data: span.clone(),
                        }
                        .init(kast)
                        .await?,
                    ),
                    data: span,
                }
                .init(kast)
                .await?,
            ),
        })
    }
    async fn macro_with_context(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([new_context], [expr]) = values
            .as_ref()
            .into_named_opt(["new_context"], ["expr"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        if expr.is_some() {
            todo!();
        }
        // god pls forgive me the sin of creating kast
        let context: Expr = kast.compile(new_context).await?;
        // no context
        // I wish I had done this in LUA
        Ok(Compiled::Expr(
            Expr::InjectContext {
                context: Box::new(context),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_current_context(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([], [context_type]) = values
            .as_ref()
            .into_named_opt([], ["context_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut expr = Expr::CurrentContext { data: span }.init(kast).await?;
        if let Some(context_type) = context_type {
            let ty = kast
                .eval_ast::<Value>(context_type, Some(TypeShape::Type.into()))
                .await?
                .into_type()?;
            expr.data_mut().ty.make_same(ty)?;
        }
        Ok(Compiled::Expr(expr))
    }
    async fn macro_comptime(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let value = values
            .as_ref()
            .into_single_named("value")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let value = kast.eval_ast(value, None).await?;
        Ok(Compiled::Expr(
            Expr::Constant { value, data: span }.init(kast).await?,
        ))
    }
    async fn macro_include_ast(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let value = values
            .as_ref()
            .into_single_named("ast")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ast = kast
            .eval_ast::<Value>(value, Some(TypeShape::Ast.into()))
            .await?
            .into_inferred()?
            .into_ast()?;
        kast.compile_into(cty, &ast).await
    }
    async fn macro_mutable_pattern(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let pattern = values.as_ref().into_single_named("pattern")?;
        let mut kast = kast.clone();
        kast.compiler.bindings_mutability = Mutability::Mutable;
        kast.compile_into(cty, pattern).await
    }
    async fn macro_or(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [lhs, rhs] = values.as_ref().into_named(["lhs", "rhs"])?;
        Ok(Compiled::Expr(
            Expr::Or {
                lhs: Box::new(kast.compile(lhs).await?),
                rhs: Box::new(kast.compile(rhs).await?),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_and(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [lhs, rhs] = values.as_ref().into_named(["lhs", "rhs"])?;
        Ok(Compiled::Expr(
            Expr::And {
                lhs: Box::new(kast.compile(lhs).await?),
                rhs: Box::new(kast.compile(rhs).await?),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_assign(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let [assignee, value] = values.as_ref().into_named(["assignee", "value"])?;
        let assignee: AssigneeExpr = kast.compile(assignee).await?;
        let mut kast = kast.clone();
        if let AssigneeExpr::Let {
            pattern: Pattern::Binding { binding, .. },
            ..
        } = &assignee
        {
            kast.current_name = kast
                .current_name
                .append(NamePart::Symbol(binding.symbol.clone()));
        }
        let value: PlaceExpr = kast.compile(value).await?;
        kast.inject_assignee_bindings(&assignee);
        Ok(Compiled::Expr(
            Expr::Assign {
                assignee,
                value: Box::new(value),
                data: span,
            }
            .init(&kast)
            .await?,
        ))
    }
    async fn macro_typeof(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let expr = values.as_ref().into_single_named("expr")?;
        let ty = kast
            .enter_scope()
            .compile::<Expr>(expr)
            .await?
            .data()
            .ty
            .clone();
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Type(ty).into(),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_ref_pattern(
        kast: &mut Kast,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Pattern);
        let (values, _span) = get_complex(ast);
        let ident = values.as_ref().into_single_named("ident")?;
        Ok(Compiled::Pattern(
            compile_ident_pattern(kast, ident, PatternBindMode::Ref).await?,
        ))
    }
    async fn macro_ref(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let place = values.as_ref().into_single_named("place")?;
        Ok(Compiled::Expr(
            Expr::Ref {
                place: kast.compile(place).await?,
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_deref(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::Expr {
            return Ok(Compiled::Expr(kast.compile::<PlaceExpr>(ast).await?.into()));
        }
        if cty == CompiledType::AssigneeExpr {
            return Ok(Compiled::AssigneeExpr(
                kast.compile::<PlaceExpr>(ast).await?.into(),
            ));
        }
        assert_eq!(cty, CompiledType::PlaceExpr);
        let (values, span) = get_complex(ast);
        let r#ref = values.as_ref().into_single_named("ref")?;
        Ok(Compiled::PlaceExpr(
            PlaceExpr::Deref {
                r#ref: Box::new(kast.compile(r#ref).await?),
                data: span,
            }
            .init(kast)
            .await?,
        ))
    }
    async fn macro_list(kast: &mut Kast, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(kast, cty, ast);
        let (values, span) = get_complex(ast);
        let ([], [values]) = values.as_ref().into_named_opt([], ["values"])?;
        let values_asts = match values {
            Some(values) => {
                let ListCollected {
                    list: values_asts,
                    all_binary: _,
                } = ListCollector {
                    macro_name: "builtin macro tuple",
                    a: "a",
                    b: "b",
                }
                .collect(values)?;
                values_asts
            }
            None => vec![],
        };
        let mut values = Vec::new();
        for value_ast in values_asts {
            values.push(kast.compile(value_ast).await?);
        }
        Ok(Compiled::Expr(
            Expr::List { values, data: span }.init(kast).await?,
        ))
    }
}
