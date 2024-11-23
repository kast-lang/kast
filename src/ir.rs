use super::*;

#[derive(Debug, Clone)]
pub struct ExprData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr<Data = ExprData> {
    Unwindable {
        name: Pattern,
        body: Box<Expr>,
        data: Data,
    },
    Unwind {
        name: Box<Expr>,
        value: Box<Expr>,
        data: Data,
    },
    InjectContext {
        context: Box<Expr>,
        data: Data,
    },
    CurrentContext {
        data: Data,
    },
    Unit {
        data: Data,
    },
    FunctionType {
        arg: Box<Expr>,
        contexts: Option<Box<Expr>>,
        result: Box<Expr>,
        data: Data,
    },
    Cast {
        value: Box<Expr>,
        target: Value,
        data: Data,
    },
    Is {
        value: Box<Expr>,
        pattern: Pattern,
        data: Data,
    },
    Match {
        value: Box<Expr>,
        branches: Vec<MatchBranch>,
        data: Data,
    },
    Newtype {
        def: Box<Expr>,
        data: Data,
    },
    Variant {
        name: String,
        value: Option<Box<Expr>>,
        data: Data,
    },
    MakeMultiset {
        values: Vec<Expr>,
        data: Data,
    },
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
        binding: Parc<Binding>,
        data: Data,
    },
    If {
        condition: Box<Expr>,
        then_case: Box<Expr>,
        else_case: Option<Box<Expr>>,
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
    CallMacro {
        r#macro: Box<Expr>,
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
        definition: Parc<ast::SyntaxDefinition>,
        values: Tuple<Expr>,
        data: Data,
    },
}

#[derive(Debug)]
pub struct CompiledFn {
    pub arg: Pattern,
    pub body: Expr,
}

pub type MaybeCompiledFn = Parc<Mutex<Option<Parc<CompiledFn>>>>;

impl Expr {
    pub fn collect_bindings(
        &self,
        consumer: &mut impl FnMut(Parc<Binding>),
        condition: Option<bool>,
    ) {
        match self {
            Expr::Binding { .. }
            | Expr::Unwind { .. }
            | Expr::Unwindable { .. }
            | Expr::CallMacro { .. }
            | Expr::InjectContext { .. }
            | Expr::CurrentContext { .. }
            | Expr::Unit { .. }
            | Expr::Cast { .. }
            | Expr::FunctionType { .. }
            | Expr::Match { .. }
            | Expr::Newtype { .. }
            | Expr::MakeMultiset { .. }
            | Expr::Variant { .. }
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
            | Expr::If { .. }
            | Expr::Instantiate { .. } => {}
            Expr::Is {
                value: _,
                pattern,
                data: _,
            } => {
                if condition == Some(true) {
                    pattern.collect_bindings(consumer);
                }
            }
            Expr::Let {
                is_const_let: _,
                pattern,
                value: _,
                data: _,
            } => {
                // if !is_const_let {
                pattern.collect_bindings(consumer);
                // }
            }
            Expr::Use {
                namespace: _,
                data: _,
            } => {}
            Expr::Then { list, data: _ } => {
                for expr in list {
                    expr.collect_bindings(consumer, None);
                }
            }
        }
    }
}

impl std::borrow::Borrow<Span> for ExprData {
    fn borrow(&self) -> &Span {
        &self.span
    }
}

impl<Data: std::borrow::Borrow<Span>> Expr<Data> {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a, Data>(&'a Expr<Data>);
        impl<Data: std::borrow::Borrow<Span>> std::fmt::Display for Show<'_, Data> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Unwind { .. } => write!(f, "unwind expr")?,
                    Expr::Unwindable { .. } => write!(f, "unwindable expr")?,
                    Expr::CallMacro { .. } => write!(f, "macro call expr")?,
                    Expr::InjectContext { .. } => write!(f, "inject context expr")?,
                    Expr::CurrentContext { .. } => write!(f, "current context expr")?,
                    Expr::Unit { .. } => write!(f, "unit expr")?,
                    Expr::Cast { .. } => write!(f, "cast expr")?,
                    Expr::FunctionType { .. } => write!(f, "fn type expr")?,
                    Expr::Newtype { .. } => write!(f, "newtype expr")?,
                    Expr::Match { .. } => write!(f, "match expr")?,
                    Expr::Is { .. } => write!(f, "is expr")?,
                    Expr::MakeMultiset { .. } => write!(f, "make multiset expr")?,
                    Expr::Variant { .. } => write!(f, "variant expr")?,
                    Expr::Use { .. } => write!(f, "use expr")?,
                    Expr::Tuple { .. } => write!(f, "tuple expr")?,
                    Expr::FieldAccess { .. } => write!(f, "field access expr")?,
                    Expr::Recursive { .. } => write!(f, "recursive expr")?,
                    Expr::Function { .. } => write!(f, "function expr")?,
                    Expr::Template { .. } => write!(f, "template expr")?,
                    Expr::Scope { .. } => write!(f, "scope expr")?,
                    Expr::If { .. } => write!(f, "if expr")?,
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
                write!(f, " at {}", self.0.data().borrow())
            }
        }
        Show(self)
    }
}

impl<Data> Expr<Data> {
    pub fn data(&self) -> &Data {
        let (Expr::Binding { data, .. }
        | Expr::Unwind { data, .. }
        | Expr::Unwindable { data, .. }
        | Expr::CallMacro { data, .. }
        | Expr::CurrentContext { data, .. }
        | Expr::InjectContext { data, .. }
        | Expr::Unit { data, .. }
        | Expr::Cast { data, .. }
        | Expr::FunctionType { data, .. }
        | Expr::Is { data, .. }
        | Expr::Match { data, .. }
        | Expr::Newtype { data, .. }
        | Expr::MakeMultiset { data, .. }
        | Expr::Variant { data, .. }
        | Expr::Use { data, .. }
        | Expr::Tuple { data, .. }
        | Expr::FieldAccess { data, .. }
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
        | Expr::Template { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::If { data, .. }
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
        | Expr::Unwind { data, .. }
        | Expr::Unwindable { data, .. }
        | Expr::CallMacro { data, .. }
        | Expr::CurrentContext { data, .. }
        | Expr::InjectContext { data, .. }
        | Expr::Unit { data, .. }
        | Expr::FunctionType { data, .. }
        | Expr::Cast { data, .. }
        | Expr::Is { data, .. }
        | Expr::Match { data, .. }
        | Expr::Newtype { data, .. }
        | Expr::MakeMultiset { data, .. }
        | Expr::Variant { data, .. }
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
        | Expr::If { data, .. }
        | Expr::Instantiate { data, .. }) = self;
        data
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Name {
    raw: String,
    id: Id,
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.raw)
    }
}

impl Name {
    pub fn raw(&self) -> &str {
        &self.raw
    }
    pub fn id(&self) -> Id {
        self.id
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
    pub hygiene: Hygiene,
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
        binding: Parc<Binding>,
        data: Data,
    },
    Tuple {
        tuple: Tuple<Pattern>,
        data: Data,
    },
    Variant {
        name: String,
        value: Option<Box<Pattern>>,
        data: Data,
    },
}

impl<Data> Pattern<Data> {
    pub fn data(&self) -> &Data {
        let (Self::Placeholder { data, .. }
        | Self::Unit { data, .. }
        | Self::Binding { data, .. }
        | Self::Tuple { data, .. }
        | Self::Variant { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Self::Placeholder { data, .. }
        | Self::Unit { data, .. }
        | Self::Binding { data, .. }
        | Self::Tuple { data, .. }
        | Self::Variant { data, .. }) = self;
        data
    }
    pub fn collect_bindings(&self, consumer: &mut impl FnMut(Parc<Binding>)) {
        match self {
            Self::Placeholder { data: _ } => {}
            Self::Unit { data: _ } => {}
            Self::Binding { binding, data: _ } => consumer(binding.clone()),
            Self::Tuple { tuple, data: _ } => {
                for field in tuple.values() {
                    field.collect_bindings(consumer);
                }
            }
            Self::Variant {
                name: _,
                value,
                data: _,
            } => {
                if let Some(value) = value {
                    value.collect_bindings(consumer);
                }
            }
        }
    }
}
