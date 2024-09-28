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

impl<Data> Expr<Data> {
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
    pub fn init(self) -> Expr {
        match self {
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
            Expr::Native { name, data: span } => Expr::Native {
                name,
                data: ExprData {
                    ty: Type::Infer(inference::Var::new()),
                    span,
                },
            },
            Expr::Let {
                mut pattern,
                mut value,
                data: span,
            } => {
                pattern.data_mut().ty.make_same(value.data_mut().ty.clone());
                Expr::Let {
                    pattern,
                    value,
                    data: ExprData {
                        ty: Type::Unit,
                        span,
                    },
                }
            }
        }
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
