use super::*;

use std::{collections::HashMap, sync::Arc};

struct ListCollector<'a> {
    macro_name: &'a str,
    a: &'a str,
    b: &'a str,
}

impl ListCollector<'_> {
    fn collect<'a>(&self, ast: &'a Ast) -> eyre::Result<Vec<&'a Ast>> {
        let mut result = Vec::new();
        let mut node = ast;
        let mut reverse = false;
        loop {
            match node {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == self.macro_name => {
                    let ([a], [b]) = values
                        .as_ref()
                        .into_named_opt([self.a], [self.b])
                        .wrap_err_with(|| "Macro received incorrect arguments")?;
                    match definition.associativity {
                        ast::Associativity::Left => {
                            reverse = true;
                            node = a;
                            if let Some(b) = b {
                                result.push(b);
                            } else {
                                break;
                            }
                        }
                        ast::Associativity::Right => {
                            if let Some(b) = b {
                                node = b;
                                result.push(a);
                            } else {
                                node = a;
                                break;
                            }
                        }
                    }
                }
                _ => break,
            }
        }
        result.push(node);
        if reverse {
            result.reverse();
        }
        Ok(result)
    }
}

#[derive(Clone)]
pub struct State {
    builtin_macros: HashMap<&'static str, BuiltinMacro>,
    syntax_definitions: Arc<Mutex<RefMap<Arc<ast::SyntaxDefinition>, std::task::Poll<Value>>>>,
}

impl State {
    pub fn new() -> Self {
        let mut builtin_macros = HashMap::new();
        macro_rules! populate {
            ($($name:ident),*$(,)?) => {
                $(
                    let name = stringify!($name).strip_prefix("macro_").unwrap();
                    let closure: BuiltinMacro = |kast: &mut Kast, cty, ast| Kast::$name(kast, cty, ast).boxed();
                    builtin_macros.insert(name, closure);
                )*
            }
        }
        populate!(
            macro_native,
            macro_type_ascribe,
            macro_const_let,
            macro_let,
            macro_call,
            macro_then,
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
            macro_import,
            macro_template_def,
            macro_instantiate_template,
            macro_placeholder,
        );
        Self {
            builtin_macros,
            syntax_definitions: Default::default(),
        }
    }

    fn find_macro(&self, definition: &Arc<ast::SyntaxDefinition>) -> eyre::Result<Macro> {
        Ok(
            if let Some(builtin_macro_name) = definition.name.strip_prefix("builtin macro ") {
                let r#impl = *self
                    .builtin_macros
                    .get(builtin_macro_name)
                    .ok_or_else(|| eyre!("builtin macro {builtin_macro_name:?} not found"))?;
                Macro::Builtin {
                    name: builtin_macro_name.to_owned(),
                    r#impl,
                }
            } else {
                let name = definition.name.as_str();
                if let Some(r#macro) = self.syntax_definitions.lock().unwrap().get(definition) {
                    let r#macro = match r#macro {
                        std::task::Poll::Pending => {
                            eyre::bail!("{name} can not be used until it is defined")
                        }
                        std::task::Poll::Ready(value) => value,
                    };
                    match r#macro {
                        Value::Macro(f) => Macro::UserDefined(f.f.clone()),
                        _ => Macro::Value(r#macro.clone()),
                    }
                } else {
                    eyre::bail!("{name:?} was not defined??? how is that even possible?");
                }
            },
        )
    }
}

enum Macro {
    Builtin {
        name: String,
        r#impl: BuiltinMacro,
    },
    UserDefined(Function),
    /// Not really a macro :)
    Value(Value),
}

#[async_trait]
pub trait Compilable: Sized {
    const CTY: CompiledType;
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self>;
    fn r#macro(
        macro_name: &str,
        f: BuiltinMacro,
    ) -> impl for<'a> Fn(&'a mut Kast, &'a Ast) -> BoxFuture<'a, eyre::Result<Self>> {
        move |kast, ast| {
            let macro_name = macro_name.to_owned();
            async move {
                Ok(Self::from_compiled(
                    f(kast, Self::CTY, ast)
                        .await
                        .wrap_err_with(|| format!("in builtin macro {macro_name:?}"))?,
                ))
            }
            .boxed()
        }
    }
    fn from_compiled(compiled: Compiled) -> Self;
}

#[async_trait]
impl Compilable for Expr {
    const CTY: CompiledType = CompiledType::Expr;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::Expr(e) => e,
            Compiled::Pattern(_) => unreachable!(),
        }
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        Ok(match ast {
            Ast::Simple { token, data: span } => match token {
                Token::Ident {
                    raw: _,
                    name,
                    is_raw: _,
                } => {
                    let value = kast
                        .interpreter
                        .get(name.as_str())
                        .await
                        .ok_or_else(|| eyre!("{name:?} not found"))?;
                    match value {
                        Value::Binding(binding) => {
                            Expr::Binding {
                                binding: binding.clone(),
                                data: span.clone(),
                            }
                            .init(kast)
                            .await?
                        }
                        _ => {
                            Expr::Constant {
                                value: value.clone(),
                                data: span.clone(),
                            }
                            .init(kast)
                            .await?
                        }
                    }
                }
                Token::String {
                    raw: _,
                    contents,
                    typ: _,
                } => {
                    Expr::Constant {
                        value: Value::String(contents.clone()),
                        data: span.clone(),
                    }
                    .init(kast)
                    .await?
                }
                Token::Number { raw } => {
                    Expr::Number {
                        raw: raw.clone(),
                        data: span.clone(),
                    }
                    .init(kast)
                    .await?
                }
                Token::Comment { .. } | Token::Punctuation { .. } | Token::Eof => unreachable!(),
            },
            Ast::Complex {
                definition,
                values,
                data: span,
            } => {
                match kast.compiler.find_macro(definition)? {
                    Macro::Builtin { name, r#impl } => {
                        Self::r#macro(&name, r#impl)(kast, ast).await?
                    }
                    Macro::UserDefined(r#macro) => {
                        let r#macro = r#macro.clone();
                        let arg = Value::Tuple(values.as_ref().map(|ast| Value::Ast(ast.clone())));
                        // hold on
                        let expanded = kast.call_fn(r#macro, arg).await?;
                        let expanded = match expanded {
                            Value::Ast(ast) => ast,
                            _ => eyre::bail!(
                                "macro {name} did not expand to an ast, but to {expanded}",
                                name = definition.name,
                            ),
                        };
                        kast.compile(&expanded).await?
                    }
                    Macro::Value(value) => {
                        Expr::Call {
                            // TODO not constant?
                            f: Box::new(
                                Expr::Constant {
                                    value: value.clone(),
                                    data: span.clone(),
                                }
                                .init(kast)
                                .await?,
                            ),
                            arg: Box::new(
                                Expr::Tuple {
                                    tuple: {
                                        let mut tuple = Tuple::empty();
                                        for (name, field) in values.as_ref() {
                                            tuple.add(name, kast.compile(field).await?);
                                        }
                                        tuple
                                    },
                                    data: span.clone(),
                                }
                                .init(kast)
                                .await?,
                            ),
                            data: span.clone(),
                        }
                        .init(kast)
                        .await?
                    } // _ => eyre::bail!("{macro} is not a macro"),
                }
            }
            Ast::SyntaxDefinition { def, data: span } => {
                kast.interpreter.insert_syntax(def.clone())?;
                if !def.name.starts_with("builtin macro ") {
                    kast.compiler
                        .syntax_definitions
                        .lock()
                        .unwrap()
                        .insert(def.clone(), std::task::Poll::Pending);
                    kast.executor
                        .spawn({
                            let kast = kast.spawn_clone();
                            let def = def.clone();
                            async move {
                                let value =
                                    kast.interpreter.get(&def.name).await.ok_or_else(|| {
                                        eyre!("syntax {:?} definition could not be found", def.name)
                                    })?;
                                kast.compiler
                                    .syntax_definitions
                                    .lock()
                                    .unwrap()
                                    .insert(def.clone(), std::task::Poll::Ready(value));
                                Ok(())
                            }
                            .map_err(|err: eyre::Report| {
                                tracing::error!("{err:?}");
                                panic!("{err:?}")
                            })
                        })
                        .detach();
                    kast.advance_executor();
                }
                Expr::Constant {
                    value: Value::Unit,
                    data: span.clone(),
                }
                .init(kast)
                .await?
            }
            Ast::FromScratch { next, data: span } => {
                // kast.compiler.syntax_definitions.lock().unwrap().clear();
                if let Some(next) = next {
                    kast.compile(next).await?
                } else {
                    Expr::Constant {
                        value: Value::Unit,
                        data: span.clone(),
                    }
                    .init(kast)
                    .await?
                }
            }
        })
    }
}

#[async_trait]
impl Compilable for Pattern {
    const CTY: CompiledType = CompiledType::Pattern;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::Expr(_) => unreachable!(),
            Compiled::Pattern(p) => p,
        }
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        tracing::debug!("compiling {}...", ast.show_short());
        let result = match ast {
            Ast::Simple { token, data: span } => match token {
                Token::Ident {
                    raw: _,
                    name,
                    is_raw: _,
                } => Pattern::Binding {
                    binding: Arc::new(Binding {
                        name: Name::new(name),
                        ty: Type::Infer(inference::Var::new()),
                    }),
                    data: span.clone(),
                }
                .init()?,
                Token::String {
                    raw: _,
                    contents: _,
                    typ: _,
                } => todo!(),
                Token::Number { raw: _ } => todo!(),
                Token::Comment { .. } | Token::Punctuation { .. } | Token::Eof => unreachable!(),
            },
            Ast::Complex {
                definition,
                values,
                data: _,
            } => {
                match kast.compiler.find_macro(definition)? {
                    Macro::Builtin { name, r#impl } => {
                        Self::r#macro(&name, r#impl)(kast, ast).await?
                    }
                    Macro::UserDefined(r#macro) => {
                        let arg = Value::Tuple(values.as_ref().map(|ast| Value::Ast(ast.clone())));
                        // hold on
                        let expanded = kast.call_fn(r#macro, arg).await?;
                        let expanded = match expanded {
                            Value::Ast(ast) => ast,
                            _ => eyre::bail!(
                                "macro {name} did not expand to an ast, but to {expanded}",
                                name = definition.name,
                            ),
                        };
                        kast.compile(&expanded).await?
                    }
                    Macro::Value(value) => {
                        eyre::bail!("{value} is not a macro")
                    }
                }
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
            Ast::FromScratch { next: _, data: _ } => todo!(),
        };
        tracing::debug!("compiled {}", ast.show_short());
        Ok(result)
    }
}

impl Kast {
    fn inject_bindings(&mut self, pattern: &Pattern) {
        pattern.collect_bindings(&mut |binding| {
            self.interpreter
                .insert_local(binding.name.raw(), Value::Binding(binding.clone()));
        });
    }
    pub async fn compile<T: Compilable>(&mut self, ast: &Ast) -> eyre::Result<T> {
        T::compile(self, ast)
            .boxed()
            .await
            .wrap_err_with(|| format!("while compiling {}", ast.show_short()))
    }
    async fn compile_into(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        Ok(match ty {
            CompiledType::Expr => Compiled::Expr(self.compile(ast).await?),
            CompiledType::Pattern => Compiled::Pattern(self.compile(ast).await?),
        })
    }
    async fn macro_type_ascribe(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let [value, ty] = values
            .as_ref()
            .into_named(["value", "type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ty = self
            .eval_ast(ty, Some(Type::Type))
            .await
            .wrap_err_with(|| "Failed to evaluate the type")?
            .expect_type()?;
        let mut value = self.compile_into(cty, value).await?;
        value.ty_mut().make_same(ty)?;
        Ok(value)
    }
    async fn macro_const_let(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(ty, CompiledType::Expr);
        let [pattern, value_ast] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = self.compile(pattern).await?;
        let value = self
            .eval_ast(value_ast, Some(pattern.data().ty.clone()))
            .await?;
        let value = Box::new(
            Expr::Constant {
                value,
                data: value_ast.data().clone(),
            }
            .init(self)
            .await?,
        );
        self.eval(
            &Expr::Let {
                is_const_let: false,
                pattern: pattern.clone(),
                value: value.clone(),
                data: span.clone(),
            }
            .init(self)
            .await?,
        )
        .await?;
        Ok(Compiled::Expr(
            Expr::Let {
                is_const_let: true,
                pattern,
                value,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_let(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(ty, CompiledType::Expr);
        let [pattern, value] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = self.compile(pattern).await?;
        let value: Expr = self.compile(value).await?;
        self.inject_bindings(&pattern);
        Ok(Compiled::Expr(
            Expr::Let {
                is_const_let: false,
                pattern,
                value: Box::new(value),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_native(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, span) = get_complex(ast);
        let name = values
            .as_ref()
            .into_single_named("name")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let name = self.compile(name).await?;
        Ok(Compiled::Expr(
            Expr::Native {
                name: Box::new(name),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_then(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let macro_name = match ast {
            Ast::Complex { definition, .. } => definition.name.as_str(),
            _ => unreachable!(),
        };
        assert_eq!(cty, CompiledType::Expr);
        let ast_list = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut expr_list = Vec::with_capacity(ast_list.len());
        for ast in ast_list {
            expr_list.push(self.compile(ast).await?);
        }
        Ok(Compiled::Expr(
            Expr::Then {
                list: expr_list,
                data: ast.data().clone(),
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_syntax_module(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = self.enter_recursive_scope();
        inner
            .eval_ast(body, Some(Type::Unit))
            .await?
            .expect_unit()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: Value::Syntax(Arc::new(inner.interpreter.scope_syntax_definitions())),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_struct_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = self.enter_recursive_scope();
        let body = inner.compile(body).await?;
        Ok(Compiled::Expr(
            Expr::Recursive {
                body: Box::new(body),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_macro(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let def = values
            .as_ref()
            .into_single_named("def")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        // TODO expect some type here?
        let def = self.eval_ast(def, None).await?;
        let def = def.expect_function()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: Value::Macro(def),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_template_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let ([arg, body], [r#where]) = values
            .as_ref()
            .into_named_opt(["arg", "body"], ["where"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let _ = r#where; // TODO
        let arg: Pattern = self.compile(arg).await?;

        let mut inner = self.enter_scope();
        self.inject_bindings(&arg);
        let compiled = inner.compile_fn_body(arg, body, None);
        Ok(Compiled::Expr(
            Expr::Template {
                compiled,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_instantiate_template(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let [template, arg] = values
            .as_ref()
            .into_named(["template", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let template = self.compile(template).await?;
        let arg = self.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Instantiate {
                template: Box::new(template),
                arg: Box::new(arg),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    fn compile_fn_body(
        &mut self,
        arg: Pattern,
        body: &Ast,
        result_type: Option<Type>,
    ) -> MaybeCompiledFn {
        let compiled = Arc::new(Mutex::new(None));
        self.executor
            .spawn({
                let mut kast = self.spawn_clone();
                let compiled = compiled.clone();
                let body: Ast = body.clone();
                let result_type = result_type.clone();
                async move {
                    let mut body: Expr = kast.compile(&body).await?;
                    if let Some(result_type) = result_type {
                        body.data_mut().ty.make_same(result_type)?;
                    }
                    let old_compiled = compiled
                        .lock()
                        .unwrap()
                        .replace(Arc::new(CompiledFn { body, arg }));
                    assert!(old_compiled.is_none(), "function compiled twice wtf?");
                    Ok(())
                }
                .map_err(|err: eyre::Report| {
                    tracing::error!("{err:?}");
                    panic!("{err:?}")
                })
            })
            .detach();
        compiled
    }
    async fn macro_function_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let ([body], [arg, contexts, result_type]) = values
            .as_ref()
            .into_named_opt(["body"], ["arg", "contexts", "result_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let arg: Pattern = match arg {
            Some(arg) => self.compile(arg).await?,
            None => Pattern::Unit { data: span.clone() }.init()?,
        };
        let mut inner = self.enter_scope();
        self.inject_bindings(&arg);
        let arg_ty = arg.data().ty.clone();
        let result_type = match result_type {
            Some(ast) => inner.eval_ast(ast, Some(Type::Type)).await?.expect_type()?,
            None => Type::Infer(inference::Var::new()),
        };
        let _ = contexts; // TODO
        let compiled = inner.compile_fn_body(arg, body, Some(result_type.clone()));
        Ok(Compiled::Expr(
            Expr::Function {
                ty: FnType {
                    arg: arg_ty,
                    result: result_type,
                },
                compiled,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_scope(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [expr] = values
            .as_ref()
            .into_unnamed()
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(match cty {
            CompiledType::Expr => {
                let expr = self.enter_scope().compile(expr).await?;
                Compiled::Expr(
                    Expr::Scope {
                        expr: Box::new(expr),
                        data: span,
                    }
                    .init(self)
                    .await?,
                )
            }
            CompiledType::Pattern => Compiled::Pattern(self.compile(expr).await?),
        })
    }
    async fn macro_import(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, span) = get_complex(ast);
        let path = self
            .eval_ast(
                values.as_ref().into_single_named("path")?,
                Some(Type::String),
            )
            .await?
            .expect_string()?;
        let path = if path.starts_with('.') {
            ast.data()
                .filename
                .parent()
                .expect("no parent dir??")
                .join(path)
        } else {
            todo!("absolute import")
        };
        let value: Value = self.import(path)?;
        Ok(Compiled::Expr(
            Expr::Constant { value, data: span }.init(self).await?,
        ))
    }
    async fn macro_use(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, span) = get_complex(ast);
        let namespace = values.as_ref().into_single_named("namespace")?;
        let namespace: Value = self.eval_ast(namespace, None).await?;
        match namespace.clone() {
            Value::Tuple(namespace) => {
                for (name, value) in namespace.into_iter() {
                    let name = name.ok_or_else(|| eyre!("cant use unnamed fields"))?;
                    self.add_local(name.as_str(), value);
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
                    .init(self)
                    .await?,
                ),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_make_unit(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [] = values.as_ref().into_named([])?;
        Ok(match cty {
            CompiledType::Pattern => Compiled::Pattern(Pattern::Unit { data: span }.init()?),
            CompiledType::Expr => Compiled::Expr(
                Expr::Constant {
                    value: Value::Unit,
                    data: span,
                }
                .init(self)
                .await?,
            ),
        })
    }
    async fn macro_call(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_eq!(cty, CompiledType::Expr);
        let [f, arg] = values
            .as_ref()
            .into_named(["f", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let f = self.compile(f).await?;
        let arg = self.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Call {
                f: Box::new(f),
                arg: Box::new(arg),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    /// this function might succeed (no promises)
    async fn macro_function_type(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, span) = get_complex(ast);
        #[allow(unused_variables)]
        let ([arg, result], [contexts]) = values
            .as_ref()
            .into_named_opt(["arg", "result"], ["contexts"])?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: Value::Type(Type::Function(Box::new(FnType {
                    // TODO when evaluating, expect Some(Type), not None
                    arg: self
                        .eval_ast(arg, None)
                        .await?
                        .expect_type()
                        .wrap_err("arg is expected to be a type")?,
                    result: self
                        .eval_ast(result, None)
                        .await?
                        .expect_type()
                        .wrap_err("result is expected to be a type")?,
                }))),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_field_access(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, span) = get_complex(ast);
        let [obj, field] = values.as_ref().into_named(["obj", "field"])?;
        let field = field
            .as_ident()
            .ok_or_else(|| eyre!("field expected to be an identifier, got {field}"))?;
        // My hair is very crusty today
        Ok(Compiled::Expr(
            Expr::FieldAccess {
                obj: Box::new(self.compile(obj).await?),
                field: field.to_owned(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_quote(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (values, _span) = get_complex(ast);
        let expr = values.as_ref().into_single_named("expr")?;
        fn quote<'a>(kast: &'a mut Kast, ast: &'a Ast) -> BoxFuture<'a, eyre::Result<Expr>> {
            async move {
                Ok(match ast {
                    Ast::Complex {
                        definition,
                        values,
                        data,
                    } => {
                        if definition.name == "builtin macro unquote" {
                            let expr = values
                                .as_ref()
                                .into_single_named("expr")
                                .wrap_err_with(|| "wrong args to unquote")?;
                            kast.compile(expr).await?
                        } else {
                            Expr::Ast {
                                definition: definition.clone(),
                                values: {
                                    let mut result = Tuple::empty();
                                    for (name, value) in values.as_ref().into_iter() {
                                        let value = quote(kast, value).boxed().await?;
                                        result.add(name, value);
                                    }
                                    result
                                },
                                data: data.clone(),
                            }
                            .init(kast)
                            .await?
                        }
                    }
                    _ => {
                        Expr::Constant {
                            value: Value::Ast(ast.clone()),
                            data: ast.data().clone(),
                        }
                        .init(kast)
                        .await?
                    }
                })
            }
            .boxed()
        }
        Ok(Compiled::Expr(quote(self, expr).await?))
    }
    async fn macro_field(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [name, value] = values.as_ref().into_named(["name", "value"])?;
        let name = name
            .as_ident()
            .ok_or_else(|| eyre!("{name} is not an ident"))?;
        Ok(match cty {
            CompiledType::Expr => Compiled::Expr(
                Expr::Tuple {
                    tuple: Tuple::single_named(name, self.compile(value).await?),
                    data: span,
                }
                .init(self)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(
                Pattern::Tuple {
                    tuple: Tuple::single_named(name, self.compile(value).await?),
                    data: span,
                }
                .init()?,
            ),
        })
    }
    async fn macro_tuple(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let macro_name = match ast {
            Ast::Complex { definition, .. } => definition.name.as_str(),
            _ => unreachable!(),
        };
        let fields = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut tuple = Tuple::empty();
        let named_field_def_name = "builtin macro field";
        for field in fields {
            match field {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == named_field_def_name => {
                    let [name, value] = values
                        .as_ref()
                        .into_named(["name", "value"])
                        .wrap_err_with(|| "field macro wrong args")?;
                    tuple.add_named(
                        name.as_ident()
                            .ok_or_else(|| eyre!("{name} is not an ident"))?
                            .to_owned(),
                        self.compile_into(cty, value).await?,
                    );
                }
                _ => tuple.add_unnamed(self.compile_into(cty, field).await?),
            }
        }
        Ok(match cty {
            CompiledType::Expr => Compiled::Expr(
                Expr::Tuple {
                    tuple: tuple.map(|field| field.expect_expr().unwrap()),
                    data: ast.data().clone(),
                }
                .init(self)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(
                Pattern::Tuple {
                    tuple: tuple.map(|field| field.expect_pattern().unwrap()),
                    data: ast.data().clone(),
                }
                .init()?,
            ),
        })
    }
    async fn macro_placeholder(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [] = values
            .as_ref()
            .into_named([])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(match cty {
            CompiledType::Expr => Compiled::Expr(
                Expr::Constant {
                    value: Value::Type(Type::Infer(inference::Var::new())),
                    data: span,
                }
                .init(self)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(Pattern::Placeholder { data: span }.init()?),
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompiledType {
    Expr,
    Pattern,
}

pub enum Compiled {
    Expr(Expr),
    Pattern(Pattern),
}

impl Compiled {
    fn ty_mut(&mut self) -> &mut Type {
        match self {
            Compiled::Expr(e) => &mut e.data_mut().ty,
            Compiled::Pattern(p) => &mut p.data_mut().ty,
        }
    }
    fn expect_expr(self) -> Option<Expr> {
        match self {
            Compiled::Expr(e) => Some(e),
            Compiled::Pattern(_) => None,
        }
    }
    fn expect_pattern(self) -> Option<Pattern> {
        match self {
            Compiled::Expr(_) => None,
            Compiled::Pattern(p) => Some(p),
        }
    }
}

type BuiltinMacro = for<'a> fn(
    kast: &'a mut Kast,
    cty: CompiledType,
    ast: &'a Ast,
) -> BoxFuture<'a, eyre::Result<Compiled>>;

fn get_complex(ast: &Ast) -> (&Tuple<Ast>, Span) {
    match ast {
        Ast::Complex {
            definition: _,
            values,
            data: span,
        } => (values, span.clone()),
        _ => unreachable!(),
    }
}

impl Expr<Span> {
    /// Initialize expr data
    pub fn init(self, kast: &Kast) -> BoxFuture<'_, eyre::Result<Expr>> {
        let r#impl = async {
            Ok(match self {
                Expr::Use {
                    namespace,
                    data: span,
                } => Expr::Use {
                    namespace,
                    data: ExprData {
                        ty: Type::Unit,
                        span,
                    },
                },
                Expr::Tuple { tuple, data: span } => Expr::Tuple {
                    data: ExprData {
                        ty: Type::Tuple({
                            let mut result = Tuple::empty();
                            for (name, field) in tuple.as_ref() {
                                result.add(name, field.data().ty.clone());
                            }
                            result
                        }),
                        span,
                    },
                    tuple,
                },
                Expr::FieldAccess {
                    obj,
                    field,
                    data: span,
                } => Expr::FieldAccess {
                    data: ExprData {
                        ty: match obj.data().ty.inferred() {
                            Ok(ty) => match &ty {
                                Type::Tuple(fields) => match fields.get_named(&field) {
                                    Some(field_ty) => field_ty.clone(),
                                    None => eyre::bail!("{ty} does not have field {field:?}"),
                                },
                                _ => eyre::bail!("can not get fields of type {ty}"),
                            },
                            Err(_) => todo!("lazy inferring field access type"),
                        },
                        span,
                    },
                    obj,
                    field,
                },
                Expr::Ast {
                    definition,
                    values,
                    data: span,
                } => {
                    for value in values.values() {
                        // TODO clone???
                        value.data().ty.clone().make_same(Type::Ast)?;
                    }
                    Expr::Ast {
                        data: ExprData {
                            ty: Type::Ast,
                            span,
                        },
                        definition,
                        values,
                    }
                }
                Expr::Recursive {
                    mut body,
                    data: span,
                } => {
                    body.data_mut().ty.make_same(Type::Unit)?;
                    let mut fields = Tuple::empty();
                    body.collect_bindings(&mut |binding| {
                        fields.add_named(binding.name.raw().to_owned(), binding.ty.clone());
                    });
                    Expr::Recursive {
                        data: ExprData {
                            ty: Type::Tuple(fields),
                            span,
                        },
                        body,
                    }
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: span,
                } => Expr::Function {
                    data: ExprData {
                        ty: Type::Function(Box::new(ty.clone())),
                        span,
                    },
                    ty,
                    compiled,
                },
                Expr::Template {
                    compiled,
                    data: span,
                } => Expr::Template {
                    data: ExprData {
                        ty: Type::Template(compiled.clone()),
                        span,
                    },
                    compiled,
                },
                Expr::Scope { expr, data: span } => Expr::Scope {
                    data: ExprData {
                        ty: expr.data().ty.clone(),
                        span,
                    },
                    expr,
                },
                Expr::Binding {
                    binding,
                    data: span,
                } => Expr::Binding {
                    data: ExprData {
                        ty: binding.ty.clone(),
                        span,
                    },
                    binding,
                },
                Expr::Then {
                    mut list,
                    data: span,
                } => {
                    let mut last = None;
                    for expr in &mut list {
                        if let Some(prev) = last.replace(expr) {
                            prev.data_mut().ty.make_same(Type::Unit)?;
                        }
                    }
                    let result_ty = last.map_or(Type::Unit, |prev| prev.data().ty.clone());
                    Expr::Then {
                        list,
                        data: ExprData {
                            ty: result_ty,
                            span,
                        },
                    }
                }
                Expr::Constant { value, data: span } => Expr::Constant {
                    data: ExprData {
                        ty: value.ty(),
                        span,
                    },
                    value,
                },
                Expr::Number { raw, data: span } => Expr::Number {
                    raw,
                    data: ExprData {
                        ty: Type::Infer(inference::Var::new()),
                        span,
                    },
                },
                Expr::Native {
                    mut name,
                    data: span,
                } => {
                    name.data_mut().ty.make_same(Type::String)?;
                    Expr::Native {
                        name,
                        data: ExprData {
                            ty: Type::Infer(inference::Var::new()),
                            span,
                        },
                    }
                }
                Expr::Let {
                    is_const_let,
                    mut pattern,
                    mut value,
                    data: span,
                } => {
                    pattern
                        .data_mut()
                        .ty
                        .make_same(value.data_mut().ty.clone())?;
                    Expr::Let {
                        is_const_let,
                        pattern,
                        value,
                        data: ExprData {
                            ty: Type::Unit,
                            span,
                        },
                    }
                }
                Expr::Call {
                    f,
                    arg: args,
                    data: span,
                } => {
                    let mut f = f.auto_instantiate(kast).await?;
                    let result_ty = Type::Infer(inference::Var::new());
                    f.data_mut().ty.make_same(Type::Function(Box::new(FnType {
                        arg: args.data().ty.clone(),
                        result: result_ty.clone(),
                    })))?;
                    Expr::Call {
                        f: Box::new(f),
                        arg: args,
                        data: ExprData {
                            ty: result_ty,
                            span,
                        },
                    }
                }
                Expr::Instantiate {
                    template: template_ir,
                    arg: arg_ir,
                    data: span,
                } => {
                    let template_ty = template_ir
                        .data()
                        .ty
                        .inferred()
                        .expect("template must be inferred")
                        .expect_template()?;
                    // TODO why am I cloning kast?
                    // TODO why eval, if then arg should be value??
                    let arg = kast.clone().eval(&arg_ir).await?;

                    kast.advance_executor();
                    let compiled: Arc<CompiledFn> = match &*template_ty.lock().unwrap() {
                        Some(compiled) => compiled.clone(),
                        None => panic!("template is not compiled yet"),
                    };
                    let mut template_kast = kast.with_scope(Arc::new(Scope::new()));
                    template_kast.bind_pattern_match(&compiled.arg, arg);
                    let result_ty =
                        template_kast.substitute_type_bindings(&compiled.body.data().ty);

                    Expr::Instantiate {
                        template: template_ir,
                        arg: arg_ir,
                        data: ExprData {
                            ty: result_ty,
                            span,
                        },
                    }
                }
            })
        };
        r#impl.boxed()
    }
}

impl Expr {
    async fn auto_instantiate(self, kast: &Kast) -> eyre::Result<Self> {
        let mut result = self;
        loop {
            let data = result.data();
            let is_template = data
                .ty
                .inferred()
                .map_or(false, |ty| matches!(ty, Type::Template { .. }));
            if !is_template {
                break;
            }
            result = Expr::Instantiate {
                arg: Box::new(
                    Expr::Constant {
                        value: Value::Type(Type::Infer(inference::Var::new())), // TODO not only type
                        data: data.span.clone(),
                    }
                    .init(kast)
                    .await?,
                ),
                data: data.span.clone(),
                template: Box::new(result),
            }
            .init(kast)
            .await?;
        }
        Ok(result)
    }
}
