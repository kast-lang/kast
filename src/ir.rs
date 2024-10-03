use std::sync::Arc;

use super::*;

#[derive(Debug, Clone)]
pub struct ExprData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr<Data = ExprData> {
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
            | Expr::Scope { .. }
            | Expr::Constant { .. }
            | Expr::Number { .. }
            | Expr::Native { .. }
            | Expr::Ast { .. }
            | Expr::Call { .. } => {}
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
            Expr::Then { a, b, data: _ } => {
                a.collect_bindings(consumer);
                b.collect_bindings(consumer);
            }
        }
    }
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a>(&'a Expr);
        impl std::fmt::Display for Show<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Tuple { .. } => write!(f, "tuple expr")?,
                    Expr::FieldAccess { .. } => write!(f, "field access expr")?,
                    Expr::Recursive { .. } => write!(f, "recursive expr")?,
                    Expr::Function { .. } => write!(f, "function expr")?,
                    Expr::Scope { .. } => write!(f, "scope expr")?,
                    Expr::Binding { binding, data: _ } => write!(f, "binding {:?}", binding.name)?,
                    Expr::Then { .. } => write!(f, "then expr")?,
                    Expr::Constant { value: _, data: _ } => write!(f, "const expr")?,
                    Expr::Number { raw, data: _ } => write!(f, "number literal {raw:?}")?,
                    Expr::Native { name: _, data: _ } => write!(f, "native expr")?,
                    Expr::Ast { .. } => write!(f, "ast expr")?,
                    Expr::Let { .. } => write!(f, "let expr")?,
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
        | Expr::Tuple { data, .. }
        | Expr::FieldAccess { data, .. }
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
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
        | Expr::Tuple { data, .. }
        | Expr::FieldAccess { data, .. }
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
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
                mut f,
                arg: args,
                data: span,
            } => {
                let result_ty = Type::Infer(inference::Var::new());
                f.data_mut().ty.make_same(Type::Function(Box::new(FnType {
                    arg: args.data().ty.clone(),
                    result: result_ty.clone(),
                })))?;
                Expr::Call {
                    f,
                    arg: args,
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

#[derive(Debug, Clone)]
pub struct PatternData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pattern<Data = PatternData> {
    Unit { data: Data },
    Binding { binding: Arc<Binding>, data: Data },
    Tuple { tuple: Tuple<Pattern>, data: Data },
}

impl<Data> Pattern<Data> {
    pub fn data(&self) -> &Data {
        let (Self::Unit { data, .. } | Self::Binding { data, .. } | Self::Tuple { data, .. }) =
            self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Self::Unit { data, .. } | Self::Binding { data, .. } | Self::Tuple { data, .. }) =
            self;
        data
    }
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Arc<Binding>)) {
        match self {
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
