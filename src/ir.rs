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
}

impl Expr {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a>(&'a Expr);
        impl std::fmt::Display for Show<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Binding { binding, data: _ } => write!(f, "binding {:?}", binding.name)?,
                    Expr::Constant { value: _, data: _ } => write!(f, "const expr")?,
                    Expr::Number { raw, data: _ } => write!(f, "number literal {raw:?}")?,
                    Expr::Native { name: _, data: _ } => write!(f, "native expr")?,
                    Expr::Let {
                        pattern: _,
                        value: _,
                        data: _,
                    } => write!(f, "let expr")?,
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
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Expr::Binding { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }) = self;
        data
    }
}

impl Expr<Span> {
    /// Initialize expr data
    pub fn init(self) -> eyre::Result<Expr> {
        Ok(match self {
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

pub struct Binding {
    pub name: Name,
    pub ty: Type,
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
