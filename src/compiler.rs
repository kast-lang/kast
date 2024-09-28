use super::*;

use std::{collections::HashMap, sync::Arc};

pub struct State {
    builtin_macros: HashMap<&'static str, BuiltinMacro>,
    pub locals: HashMap<String, Arc<Binding>>,
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
        populate!(macro_native, macro_type_ascribe, macro_let);
        Self {
            builtin_macros,
            locals: HashMap::new(),
        }
    }
}

pub trait Compilable: Sized {
    const CTY: CompiledType;
    fn compile(kast: &mut Kast, ast: Ast) -> Self;
    fn r#macro(f: BuiltinMacro) -> impl Fn(&mut Kast, Tuple<Ast>, Span) -> Self {
        move |kast, values, span| Self::from_compiled(f(kast, Self::CTY, values, span))
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
    fn compile(kast: &mut Kast, ast: Ast) -> Self {
        match ast {
            Ast::Simple { token, data: span } => match token {
                Token::Ident {
                    raw: _,
                    name,
                    is_raw: _,
                } => Expr::Binding {
                    binding: kast
                        .compiler
                        .locals
                        .get(name.as_str())
                        .expect("todo err name not found")
                        .clone(),
                    data: span,
                }
                .init(),
                Token::String {
                    raw: _,
                    contents,
                    typ: _,
                } => Expr::Constant {
                    value: Value::String(contents),
                    data: span,
                }
                .init(),
                Token::Number { raw } => Expr::Number { raw, data: span }.init(),
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
                        .unwrap_or_else(|| {
                            panic!("todo err builtin macro {builtin_macro_name:?} not found")
                        });
                    return Self::r#macro(r#macro)(kast, values, span);
                }
                todo!()
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
        }
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
    fn compile(kast: &mut Kast, ast: Ast) -> Self {
        match ast {
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
                    data: span,
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
                        .unwrap_or_else(|| {
                            panic!("todo err builtin macro {builtin_macro_name:?} not found")
                        });
                    return Self::r#macro(r#macro)(kast, values, span);
                }
                todo!()
            }
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
        }
    }
}

impl Kast {
    pub fn compile<T: Compilable>(&mut self, ast: Ast) -> T {
        T::compile(self, ast)
    }
    pub fn compile_into(&mut self, ty: CompiledType, ast: Ast) -> Compiled {
        match ty {
            CompiledType::Expr => Compiled::Expr(self.compile(ast)),
            CompiledType::Pattern => Compiled::Pattern(self.compile(ast)),
        }
    }
    pub fn compile_pattern(&mut self, ast: Ast) -> Pattern {
        self.compile(ast)
    }
    pub fn compile_expr(&mut self, ast: Ast) -> Expr {
        self.compile(ast)
    }
    fn macro_type_ascribe(
        &mut self,
        cty: CompiledType,
        values: Tuple<Ast>,
        _span: Span,
    ) -> Compiled {
        let [value, ty] = values
            .into_named(["value", "type"])
            .unwrap_or_else(|_| panic!("todo err"));
        let ty = self
            .eval_ast(ty)
            .into_ty()
            .unwrap_or_else(|value| panic!("todo err {value} is not a type"));
        let mut value = self.compile_into(cty, value);
        value.ty_mut().make_same(ty);
        value
    }
    fn macro_let(&mut self, ty: CompiledType, values: Tuple<Ast>, span: Span) -> Compiled {
        assert_eq!(ty, CompiledType::Expr);
        let [pattern, value] = values
            .into_named(["pattern", "value"])
            .unwrap_or_else(|_| panic!("todo err"));
        Compiled::Expr(
            Expr::Let {
                pattern: self.compile_pattern(pattern),
                value: Box::new(self.compile_expr(value)),
                data: span,
            }
            .init(),
        )
    }
    fn macro_native(&mut self, cty: CompiledType, values: Tuple<Ast>, span: Span) -> Compiled {
        assert_eq!(cty, CompiledType::Expr);
        Compiled::Expr(
            Expr::Native {
                name: Box::new(
                    self.compile_expr(
                        values
                            .into_single_named("name")
                            .unwrap_or_else(|_| panic!("todo err")),
                    ),
                ),
                data: span,
            }
            .init(),
        )
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

type BuiltinMacro =
    fn(kast: &mut Kast, cty: CompiledType, values: Tuple<Ast>, span: Span) -> Compiled;
