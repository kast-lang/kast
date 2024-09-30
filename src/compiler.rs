use super::*;

use std::{collections::HashMap, sync::Arc};

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
                    builtin_macros.insert(name, Kast::$name as _);
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
        );
        Self { builtin_macros }
    }
}

pub trait Compilable: Sized {
    const CTY: CompiledType;
    fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self>;
    fn r#macro(
        macro_name: &str,
        f: BuiltinMacro,
    ) -> impl Fn(&mut Kast, &Tuple<Ast>, Span) -> eyre::Result<Self> + '_ {
        move |kast, values, span| {
            Ok(Self::from_compiled(
                f(kast, Self::CTY, values, span)
                    .wrap_err_with(|| format!("in builtin macro {macro_name:?}"))?,
            ))
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
    fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
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
                    Self::r#macro(builtin_macro_name, r#macro)(kast, values, span.clone())?
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
    fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
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
                .init(),
                Token::String {
                    raw: _,
                    contents,
                    typ,
                } => todo!(),
                Token::Number { raw } => todo!(),
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
                    Self::r#macro(builtin_macro_name, r#macro)(kast, values, span.clone())?
                } else {
                    todo!()
                }
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
        })
    }
}

impl Kast {
    pub fn compile<T: Compilable>(&mut self, ast: &Ast) -> eyre::Result<T> {
        T::compile(self, ast).wrap_err_with(|| format!("while compiling {}", ast.show_short()))
    }
    fn compile_into(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        Ok(match ty {
            CompiledType::Expr => Compiled::Expr(self.compile(ast)?),
            CompiledType::Pattern => Compiled::Pattern(self.compile(ast)?),
        })
    }
    fn macro_type_ascribe(
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
            .wrap_err_with(|| "Failed to evaluate the type")?
            .expect_type()?;
        let mut value = self.compile_into(cty, value)?;
        value.ty_mut().make_same(ty)?;
        Ok(value)
    }
    fn macro_const_let(
        &mut self,
        ty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(ty, CompiledType::Expr);
        let let_expr = match self.macro_let(CompiledType::Expr, values, span.clone())? {
            Compiled::Expr(e) => e,
            _ => unreachable!(),
        };
        self.eval(&let_expr)?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: Value::Unit,
                data: span,
            }
            .init()?,
        ))
    }
    fn macro_let(
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
                pattern: self.compile(pattern)?,
                value: Box::new(self.compile(value)?),
                data: span,
            }
            .init()?,
        ))
    }
    fn macro_native(
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
        let name = self.compile(name)?;
        Ok(Compiled::Expr(
            Expr::Native {
                name: Box::new(name),
                data: span,
            }
            .init()?,
        ))
    }
    fn macro_then(
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
        let mut a: Expr = self.compile(a)?;
        a.data_mut().ty.make_same(Type::Unit)?;
        self.interpreter.enter_scope();
        a.collect_bindings(&mut |binding| {
            self.interpreter
                .insert_local(binding.name.raw(), Value::Binding(binding.clone()))
        });
        let b: Option<Expr> = b.map(|b| self.compile(b)).transpose()?;
        self.interpreter.exit_scope();
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
    fn macro_scope(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let [expr] = values
            .as_ref()
            .into_unnamed()
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let expr = self.compile(expr)?;
        Ok(Compiled::Expr(
            Expr::Scope {
                expr: Box::new(expr),
                data: span,
            }
            .init()?,
        ))
    }
    fn macro_call(
        &mut self,
        cty: CompiledType,
        values: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Expr);
        let [f, args] = values
            .as_ref()
            .into_named(["f", "args"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let f = self.compile(f)?;
        let args = self.compile(args)?;
        Ok(Compiled::Expr(
            Expr::Call {
                f: Box::new(f),
                args: Box::new(args),
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

type BuiltinMacro = fn(
    kast: &mut Kast,
    cty: CompiledType,
    values: &Tuple<Ast>,
    span: Span,
) -> eyre::Result<Compiled>;
