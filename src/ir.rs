use std::sync::Arc;

use super::*;

pub struct ExprData {
    pub ty: Type,
    pub span: Span,
}

pub enum Expr<Data = ExprData> {
    Binding {
        binding: Arc<Binding>,
        data: Data,
    },
    Then {
        a: Box<Expr>,
        b: Box<Expr>,
        data: Data,
    },
    Constant {
        value: Value,
        data: Data,
    },
    Number {
        raw: String,
        data: Data,
    },
    Native {
        /// Name - expr that should evaluate to a string
        name: Box<Expr>,
        data: Data,
    },
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        data: Data,
    },
    Call {
        f: Box<Expr>,
        args: Box<Expr>,
        data: Data,
    },
    Scope {
        expr: Box<Expr>,
        data: Data,
    },
}

impl Expr {
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Arc<Binding>)) {
        match self {
            Expr::Binding { .. }
            | Expr::Scope { .. }
            | Expr::Then { .. }
            | Expr::Constant { .. }
            | Expr::Number { .. }
            | Expr::Native { .. }
            | Expr::Call { .. } => {}
            Expr::Let {
                pattern,
                value: _,
                data: _,
            } => pattern.collect_bindings(consumer),
        }
    }
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a>(&'a Expr);
        impl std::fmt::Display for Show<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Scope { .. } => write!(f, "scope expr")?,
                    Expr::Binding { binding, data: _ } => write!(f, "binding {:?}", binding.name)?,
                    Expr::Then { .. } => write!(f, "then expr")?,
                    Expr::Constant { value: _, data: _ } => write!(f, "const expr")?,
                    Expr::Number { raw, data: _ } => write!(f, "number literal {raw:?}")?,
                    Expr::Native { name: _, data: _ } => write!(f, "native expr")?,
                    Expr::Let {
                        pattern: _,
                        value: _,
                        data: _,
                    } => write!(f, "let expr")?,
                    Expr::Call { .. } => write!(f, "call expr")?,
                }
                write!(f, " at {}", self.0.data().span)
            }
        }
        Show(self)
    }
}

impl<Data> Expr<Data> {
    pub fn data(&self) -> &Data {
        let (Expr::Binding { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Expr::Binding { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }) = self;
        data
    }
}

impl Expr<Span> {
    /// Initialize expr data
    pub fn init(self) -> eyre::Result<Expr> {
        Ok(match self {
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
                mut a,
                b,
                data: span,
            } => {
                a.data_mut().ty.make_same(Type::Unit)?;
                let result_ty = b.data().ty.clone();
                Expr::Then {
                    a,
                    b,
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
                mut pattern,
                mut value,
                data: span,
            } => {
                pattern
                    .data_mut()
                    .ty
                    .make_same(value.data_mut().ty.clone())?;
                Expr::Let {
                    pattern,
                    value,
                    data: ExprData {
                        ty: Type::Unit,
                        span,
                    },
                }
            }
            Expr::Call {
                mut f,
                args,
                data: span,
            } => {
                let result_ty = Type::Infer(inference::Var::new());
                f.data_mut().ty.make_same(Type::Function(Box::new(FnType {
                    arg: args.data().ty.clone(),
                    result: result_ty.clone(),
                })))?;
                Expr::Call {
                    f,
                    args,
                    data: ExprData {
                        ty: result_ty,
                        span,
                    },
                }
            }
        })
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Name {
    raw: String,
    id: Id,
}

impl Name {
    pub fn raw(&self) -> &str {
        &self.raw
    }
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            raw: name.into(),
            id: Id::new(),
        }
    }
}

#[derive(Debug)]
pub struct Binding {
    pub name: Name,
    pub ty: Type,
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<binding {:?}>", self.name.raw())
    }
}

pub struct PatternData {
    pub ty: Type,
    pub span: Span,
}

pub enum Pattern<Data = PatternData> {
    Binding { binding: Arc<Binding>, data: Data },
}

impl<Data> Pattern<Data> {
    pub fn data_mut(&mut self) -> &mut Data {
        let Self::Binding { data, .. } = self;
        data
    }
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Arc<Binding>)) {
        match self {
            Pattern::Binding { binding, data: _ } => consumer(binding.clone()),
        }
    }
}

impl Pattern<Span> {
    pub fn init(self) -> Pattern {
        match self {
            Pattern::Binding {
                binding,
                data: span,
            } => Pattern::Binding {
                data: PatternData {
                    ty: binding.ty.clone(),
                    span,
                },
                binding,
            },
        }
    }
}
