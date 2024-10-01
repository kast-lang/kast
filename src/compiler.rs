use super::*;

use std::{collections::HashMap, sync::Arc};

#[derive(Clone)]
pub struct State {
    builtin_macros: HashMap<&'static str, BuiltinMacro>,
}

impl State {
    pub fn new() -> Self {
        let mut builtin_macros = HashMap::new();
        macro_rules! populate {
            ($($name:ident),*$(,)?) => {
                $(
                    let name = stringify!($name).strip_prefix("macro_").unwrap();
                    let closure: BuiltinMacro = |kast: &mut Kast, cty, values, span| Kast::$name(kast, cty, values, span).boxed();
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
            macro_function_def,
            macro_struct_def,
        );
        Self { builtin_macros }
    }
}

pub trait Compilable: Sized {
    const CTY: CompiledType;
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self>;
    fn r#macro(
        macro_name: &str,
        f: BuiltinMacro,
    ) -> impl for<'a> Fn(&'a mut Kast, &'a Tuple<Ast>, Span) -> BoxFuture<'a, eyre::Result<Self>>
    {
        move |kast, values, span| {
            let macro_name = macro_name.to_owned();
            async move {
                Ok(Self::from_compiled(
                    f(kast, Self::CTY, values, span)
                        .await
                        .wrap_err_with(|| format!("in builtin macro {macro_name:?}"))?,
                ))
            }
            .boxed()
        }
    }
    fn from_compiled(compiled: Compiled) -> Self;
}

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
                        Value::Binding(binding) => Expr::Binding {
                            binding: binding.clone(),
                            data: span.clone(),
                        }
                        .init()?,
                        _ => Expr::Constant {
                            value: value.clone(),
                            data: span.clone(),
                        }
                        .init()?,
                    }
                }
                Token::String {
                    raw: _,
                    contents,
                    typ: _,
                } => Expr::Constant {
                    value: Value::String(contents.clone()),
                    data: span.clone(),
                }
                .init()?,
                Token::Number { raw } => Expr::Number {
                    raw: raw.clone(),
                    data: span.clone(),
                }
                .init()?,
                Token::Comment { .. } | Token::Punctuation { .. } | Token::Eof => unreachable!(),
            },
            Ast::Complex {
                definition,
                values,
                data: span,
            } => {
                if let Some(builtin_macro_name) = definition.name.strip_prefix("builtin macro ") {
                    let r#macro = *kast
                        .compiler
                        .builtin_macros
                        .get(builtin_macro_name)
                        .ok_or_else(|| eyre!("builtin macro {builtin_macro_name:?} not found"))?;
                    Self::r#macro(builtin_macro_name, r#macro)(kast, values, span.clone()).await?
                } else {
                    todo!()
                }
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
        })
    }
}

impl Compilable for Pattern {
    const CTY: CompiledType = CompiledType::Pattern;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::Expr(_) => unreachable!(),
            Compiled::Pattern(p) => p,
        }
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        Ok(match ast {
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
                data: span,
            } => {
                if let Some(builtin_macro_name) = definition.name.strip_prefix("builtin macro ") {
                    let r#macro = *kast
                        .compiler
                        .builtin_macros
                        .get(builtin_macro_name)
                        .ok_or_else(|| eyre!("builtin macro {builtin_macro_name:?} not found"))?;
                    Self::r#macro(builtin_macro_name, r#macro)(kast, values, span.clone()).await?
                } else {
                    todo!()
                }
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
        })
    }
}

impl Kast {
    pub async fn compile<T: Compilable>(&mut self, ast: &Ast) -> eyre::Result<T> {
        T::compile(self, ast)
            .await
            .wrap_err_with(|| format!("while compiling {}", ast.show_short()))
    }
    async fn compile_into(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        Ok(match ty {
            CompiledType::Expr => Compiled::Expr(self.compile(ast).await?),
            CompiledType::Pattern => Compiled::Pattern(self.compile(ast).await?),
        })
    }
    async fn macro_type_ascribe(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        _span: Span,
    ) -> eyre::Result<Compiled> {
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
    async fn macro_const_let(
        &mut self,
        ty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(ty, CompiledType::Expr);
        let let_expr = match self
            .macro_let(CompiledType::Expr, values, span.clone())
            .await?
        {
            Compiled::Expr(e) => e,
            _ => unreachable!(),
        };
        self.eval(&let_expr).await?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: Value::Unit,
                data: span,
            }
            .init()?,
        ))
    }
    async fn macro_let(
        &mut self,
        ty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(ty, CompiledType::Expr);
        let [pattern, value] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Let {
                pattern: self.compile(pattern).await?,
                value: Box::new(self.compile(value).await?),
                data: span,
            }
            .init()?,
        ))
    }
    async fn macro_native(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
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
            .init()?,
        ))
    }
    async fn macro_then(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let (a, b) = values
            .as_ref()
            .into_single_named("a")
            .map(|a| (a, None))
            .or_else(|_| {
                values
                    .as_ref()
                    .into_named(["a", "b"])
                    .map(|[a, b]| (a, Some(b)))
            })
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut a: Expr = self.compile(a).await?;
        a.data_mut().ty.make_same(Type::Unit)?;
        a.collect_bindings(&mut |binding| {
            self.interpreter
                .insert_local(binding.name.raw(), Value::Binding(binding.clone()))
        });
        let b: Option<Expr> = match b {
            Some(b) => Some(self.compile(b).await?),
            None => None,
        };
        Ok(Compiled::Expr(match b {
            None => a,
            Some(b) => Expr::Then {
                a: Box::new(a),
                b: Box::new(b),
                data: span,
            }
            .init()?,
        }))
    }
    async fn macro_struct_def(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        self.interpreter.enter_recursive_scope();
        let body = self.compile(body).await?;
        self.interpreter.exit_scope();
        Ok(Compiled::Expr(
            Expr::Recursive {
                body: Box::new(body),
                data: span,
            }
            .init()?,
        ))
    }
    async fn macro_function_def(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let ([body], [arg, contexts, result_type]) = values
            .as_ref()
            .into_named_opt(["body"], ["arg", "contexts", "result_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let arg: Pattern = match arg {
            Some(arg) => self.compile(arg).await?,
            None => Pattern::Unit { data: span.clone() }.init()?,
        };
        self.interpreter.enter_scope();
        arg.collect_bindings(&mut |binding| {
            self.interpreter
                .insert_local(binding.name.raw(), Value::Binding(binding.clone()))
        });
        let arg_ty = arg.data().ty.clone();
        let result_type = match result_type {
            Some(ast) => self.eval_ast(ast, Some(Type::Type)).await?.expect_type()?,
            None => Type::Infer(inference::Var::new()),
        };
        let _ = contexts; // TODO
        let compiled = Arc::new(Mutex::new(None));
        self.executor
            .spawn({
                let mut kast = self.clone();
                let compiled = compiled.clone();
                let body: Ast = body.clone();
                let result_type = result_type.clone();
                async move {
                    let mut body: Expr = kast.compile(&body).await?;
                    body.data_mut().ty.make_same(result_type)?;
                    let old_compiled = compiled
                        .lock()
                        .unwrap()
                        .replace(Arc::new(CompiledFn { body, arg }));
                    assert!(old_compiled.is_none(), "function compiled twice wtf?");
                    Ok(())
                }
                .map_err(|err: eyre::Report| panic!("{err:?}"))
            })
            .detach();
        self.interpreter.exit_scope();
        Ok(Compiled::Expr(
            Expr::Function {
                ty: FnType {
                    arg: arg_ty,
                    result: result_type,
                },
                compiled,
                data: span,
            }
            .init()?,
        ))
    }
    async fn macro_scope(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        let [expr] = values
            .as_ref()
            .into_unnamed()
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(match cty {
            CompiledType::Expr => {
                let expr = self.compile(expr).await?;
                Compiled::Expr(
                    Expr::Scope {
                        expr: Box::new(expr),
                        data: span,
                    }
                    .init()?,
                )
            }
            CompiledType::Pattern => Compiled::Pattern(self.compile(expr).await?),
        })
    }
    async fn macro_call(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
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
            .init()?,
        ))
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
}

type BuiltinMacro = for<'a, 'b> fn(
    kast: &'a mut Kast,
    cty: CompiledType,
    values: &'a Tuple<Ast>,
    span: Span,
) -> BoxFuture<'a, eyre::Result<Compiled>>;
