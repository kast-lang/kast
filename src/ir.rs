use std::sync::Arc;

use super::*;

#[derive(Debug, Clone)]
pub struct ExprData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr<Data = ExprData> {
    Use {
        namespace: Box<Expr>,
        data: Data,
    },
    FieldAccess {
        obj: Box<Expr>,
        field: String,
        data: Data,
    },
    Recursive {
        body: Box<Expr>,
        data: Data,
    },
    Binding {
        binding: Arc<Binding>,
        data: Data,
    },
    Then {
        list: Vec<Expr>,
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
        is_const_let: bool,
        pattern: Pattern,
        value: Box<Expr>,
        data: Data,
    },
    Call {
        f: Box<Expr>,
        arg: Box<Expr>,
        data: Data,
    },
    Scope {
        expr: Box<Expr>,
        data: Data,
    },
    Function {
        ty: FnType,
        compiled: MaybeCompiledFn,
        data: Data,
    },
    Template {
        compiled: MaybeCompiledFn,
        data: Data,
    },
    Instantiate {
        template: Box<Expr>,
        arg: Box<Expr>,
        data: Data,
    },
    Tuple {
        tuple: Tuple<Expr>,
        data: Data,
    },
    Ast {
        definition: Arc<ast::SyntaxDefinition>,
        values: Tuple<Expr>,
        data: Data,
    },
}

#[derive(Debug)]
pub struct CompiledFn {
    pub arg: Pattern,
    pub body: Expr,
}

pub type MaybeCompiledFn = Arc<Mutex<Option<Arc<CompiledFn>>>>;

impl Expr {
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Arc<Binding>)) {
        match self {
            Expr::Binding { .. }
            | Expr::Tuple { .. }
            | Expr::FieldAccess { .. }
            | Expr::Recursive { .. }
            | Expr::Function { .. }
            | Expr::Template { .. }
            | Expr::Scope { .. }
            | Expr::Constant { .. }
            | Expr::Number { .. }
            | Expr::Native { .. }
            | Expr::Ast { .. }
            | Expr::Call { .. }
            | Expr::Instantiate { .. } => {}
            Expr::Let {
                is_const_let,
                pattern,
                value: _,
                data: _,
            } => {
                if !is_const_let {
                    pattern.collect_bindings(consumer);
                }
            }
            Expr::Use {
                namespace: _,
                data: _,
            } => {}
            Expr::Then { list, data: _ } => {
                for expr in list {
                    expr.collect_bindings(consumer);
                }
            }
        }
    }
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a>(&'a Expr);
        impl std::fmt::Display for Show<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Use { .. } => write!(f, "use expr")?,
                    Expr::Tuple { .. } => write!(f, "tuple expr")?,
                    Expr::FieldAccess { .. } => write!(f, "field access expr")?,
                    Expr::Recursive { .. } => write!(f, "recursive expr")?,
                    Expr::Function { .. } => write!(f, "function expr")?,
                    Expr::Template { .. } => write!(f, "template expr")?,
                    Expr::Scope { .. } => write!(f, "scope expr")?,
                    Expr::Binding { binding, data: _ } => write!(f, "binding {:?}", binding.name)?,
                    Expr::Then { .. } => write!(f, "then expr")?,
                    Expr::Constant { value: _, data: _ } => write!(f, "const expr")?,
                    Expr::Number { raw, data: _ } => write!(f, "number literal {raw:?}")?,
                    Expr::Native { name: _, data: _ } => write!(f, "native expr")?,
                    Expr::Ast { .. } => write!(f, "ast expr")?,
                    Expr::Let { .. } => write!(f, "let expr")?,
                    Expr::Call { .. } => write!(f, "call expr")?,
                    Expr::Instantiate { .. } => write!(f, "instantiate expr")?,
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
        | Expr::Use { data, .. }
        | Expr::Tuple { data, .. }
        | Expr::FieldAccess { data, .. }
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
        | Expr::Template { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }
        | Expr::Instantiate { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Expr::Binding { data, .. }
        | Expr::Use { data, .. }
        | Expr::Tuple { data, .. }
        | Expr::FieldAccess { data, .. }
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
        | Expr::Template { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }
        | Expr::Instantiate { data, .. }) = self;
        data
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

#[derive(Debug, Clone)]
pub struct PatternData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pattern<Data = PatternData> {
    /// matches anything
    Placeholder {
        data: Data,
    },
    Unit {
        data: Data,
    },
    Binding {
        binding: Arc<Binding>,
        data: Data,
    },
    Tuple {
        tuple: Tuple<Pattern>,
        data: Data,
    },
}

impl<Data> Pattern<Data> {
    pub fn data(&self) -> &Data {
        let (Self::Placeholder { data, .. }
        | Self::Unit { data, .. }
        | Self::Binding { data, .. }
        | Self::Tuple { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Self::Placeholder { data, .. }
        | Self::Unit { data, .. }
        | Self::Binding { data, .. }
        | Self::Tuple { data, .. }) = self;
        data
    }
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Arc<Binding>)) {
        match self {
            Self::Placeholder { data: _ } => {}
            Self::Unit { data: _ } => {}
            Self::Binding { binding, data: _ } => consumer(binding.clone()),
            Self::Tuple { tuple, data: _ } => {
                for field in tuple.values() {
                    field.collect_bindings(consumer);
                }
            }
        }
    }
}

impl Pattern<Span> {
    pub fn init(self) -> eyre::Result<Pattern> {
        Ok(match self {
            Pattern::Placeholder { data: span } => Pattern::Placeholder {
                data: PatternData {
                    ty: Type::Infer(inference::Var::new()),
                    span,
                },
            },
            Pattern::Unit { data: span } => Pattern::Unit {
                data: PatternData {
                    ty: Type::Unit,
                    span,
                },
            },
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
            Pattern::Tuple { tuple, data: span } => Pattern::Tuple {
                data: PatternData {
                    ty: Type::Tuple(tuple.as_ref().map(|field| field.data().ty.clone())),
                    span,
                },
                tuple,
            },
        })
    }
}
