use super::*;

#[derive(Clone)]
pub struct ExprData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone, derive_macros::ExprDisplay)]
pub struct MatchBranch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Clone, derive_macros::ExprDisplay)]
pub enum Expr<Data = ExprData> {
    Ref {
        place: PlaceExpr,
        data: Data,
    },
    And {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        data: Data,
    },
    Or {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        data: Data,
    },
    Assign {
        pattern: Pattern,
        value: Box<PlaceExpr>,
        data: Data,
    },
    List {
        values: Vec<Expr>,
        data: Data,
    },
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
        value: Box<PlaceExpr>,
        pattern: Pattern,
        data: Data,
    },
    Match {
        value: Box<PlaceExpr>,
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
    Recursive {
        body: Box<Expr>,
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
    String {
        #[display(skip)]
        token: kast_ast::StringToken,
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
        value: Box<PlaceExpr>,
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
        #[display(skip)]
        compiled: MaybeCompiledFn,
        data: Data,
    },
    Template {
        #[display(skip)]
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
        expr_root: bool,
        #[display(skip)]
        definition: Parc<ast::SyntaxDefinition>,
        values: Tuple<Expr>,
        hygiene: Hygiene,
        #[display(skip)]
        def_site: Option<CompilerScope>,
        data: Data,
    },
    ReadPlace {
        place: PlaceExpr,
        data: Data,
    },
}

#[derive(Clone, derive_macros::ExprDisplay)]
pub enum PlaceExpr<Data = ExprData> {
    Binding {
        binding: Parc<Binding>,
        data: Data,
    },
    FieldAccess {
        obj: Box<PlaceExpr>,
        field: String,
        data: Data,
    },
    Temporary {
        value: Box<Expr>,
        data: Data,
    },
    Deref {
        r#ref: Box<Expr>,
        data: Data,
    },
}

impl PlaceExpr {
    pub fn new_temp(e: Expr) -> Self {
        PlaceExpr::Temporary {
            data: e.data().clone(),
            value: Box::new(e),
        }
    }
}

impl From<PlaceExpr> for Expr {
    fn from(place: PlaceExpr) -> Self {
        Self::ReadPlace {
            data: place.data().clone(),
            place,
        }
    }
}

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
            Expr::Ref { .. }
            | Expr::And { .. }
            | Expr::Or { .. }
            | Expr::List { .. }
            | Expr::Assign { .. }
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
            | Expr::Recursive { .. }
            | Expr::Function { .. }
            | Expr::Template { .. }
            | Expr::Scope { .. }
            | Expr::Constant { .. }
            | Expr::Number { .. }
            | Expr::String { .. }
            | Expr::ReadPlace { .. }
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

impl<Data> PlaceExpr<Data> {
    pub fn data(&self) -> &Data {
        match self {
            Self::Deref { data, .. }
            | Self::Temporary { data, .. }
            | PlaceExpr::Binding { data, .. }
            | PlaceExpr::FieldAccess { data, .. } => data,
        }
    }
    pub fn data_mut(&mut self) -> &mut Data {
        match self {
            Self::Deref { data, .. }
            | Self::Temporary { data, .. }
            | PlaceExpr::Binding { data, .. }
            | PlaceExpr::FieldAccess { data, .. } => data,
        }
    }
}

impl<Data: std::borrow::Borrow<Span>> PlaceExpr<Data> {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a, Data>(&'a PlaceExpr<Data>);
        impl<Data: std::borrow::Borrow<Span>> std::fmt::Display for Show<'_, Data> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    PlaceExpr::Temporary { value, data: _ } => {
                        write!(f, "temporary {}", value.show_short())?
                    }
                    PlaceExpr::Binding { binding, data: _ } => {
                        write!(f, "binding {:?}", binding.symbol)?
                    }
                    PlaceExpr::FieldAccess { .. } => write!(f, "field access expr")?,
                    PlaceExpr::Deref { .. } => write!(f, "deref expr")?,
                }
                write!(f, " at {}", self.0.data().borrow())
            }
        }
        Show(self)
    }
}

impl<Data: std::borrow::Borrow<Span>> Expr<Data> {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a, Data>(&'a Expr<Data>);
        impl<Data: std::borrow::Borrow<Span>> std::fmt::Display for Show<'_, Data> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Expr::Ref { .. } => write!(f, "ref expr")?,
                    Expr::And { .. } => write!(f, "and expr")?,
                    Expr::Or { .. } => write!(f, "or expr")?,
                    Expr::Assign { .. } => write!(f, "assign expr")?,
                    Expr::Unwind { .. } => write!(f, "unwind expr")?,
                    Expr::List { .. } => write!(f, "list expr")?,
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
                    Expr::Recursive { .. } => write!(f, "recursive expr")?,
                    Expr::Function { .. } => write!(f, "function expr")?,
                    Expr::Template { .. } => write!(f, "template expr")?,
                    Expr::Scope { .. } => write!(f, "scope expr")?,
                    Expr::If { .. } => write!(f, "if expr")?,
                    Expr::Then { .. } => write!(f, "then expr")?,
                    Expr::Constant { value: _, data: _ } => write!(f, "const expr")?,
                    Expr::Number { raw, data: _ } => write!(f, "number literal {raw:?}")?,
                    Expr::String { token, data: _ } => write!(f, "string literal {:?}", token.raw)?,
                    Expr::Native { name: _, data: _ } => write!(f, "native expr")?,
                    Expr::Ast { .. } => write!(f, "ast expr")?,
                    Expr::Let { .. } => write!(f, "let expr")?,
                    Expr::Call { .. } => write!(f, "call expr")?,
                    Expr::Instantiate { .. } => write!(f, "instantiate expr")?,
                    Expr::ReadPlace { .. } => write!(f, "readref expr")?,
                }
                write!(f, " at {}", self.0.data().borrow())
            }
        }
        Show(self)
    }
}

impl<Data> Expr<Data> {
    pub fn data(&self) -> &Data {
        let (Expr::And { data, .. }
        | Expr::Ref { data, .. }
        | Expr::Or { data, .. }
        | Expr::Assign { data, .. }
        | Expr::Unwind { data, .. }
        | Expr::List { data, .. }
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
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
        | Expr::Template { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::If { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::String { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }
        | Expr::ReadPlace { data, .. }
        | Expr::Instantiate { data, .. }) = self;
        data
    }
    pub fn data_mut(&mut self) -> &mut Data {
        let (Expr::And { data, .. }
        | Expr::Ref { data, .. }
        | Expr::Or { data, .. }
        | Expr::Assign { data, .. }
        | Expr::List { data, .. }
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
        | Expr::Ast { data, .. }
        | Expr::Recursive { data, .. }
        | Expr::Function { data, .. }
        | Expr::Template { data, .. }
        | Expr::Scope { data, .. }
        | Expr::Then { data, .. }
        | Expr::Constant { data, .. }
        | Expr::Number { data, .. }
        | Expr::String { data, .. }
        | Expr::Native { data, .. }
        | Expr::Let { data, .. }
        | Expr::Call { data, .. }
        | Expr::If { data, .. }
        | Expr::ReadPlace { data, .. }
        | Expr::Instantiate { data, .. }) = self;
        data
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Symbol {
    name: std::sync::Arc<str>,
    id: Id,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

impl Symbol {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn id(&self) -> Id {
        self.id
    }
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into().into(),
            id: Id::new(),
        }
    }
}

pub struct Binding {
    pub symbol: Symbol,
    pub ty: Type,
    pub mutable: bool,
    pub compiler_scope: CompilerScope,
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<binding {:?} {:?}>",
            self.symbol.name(),
            self.symbol.id(),
        )
    }
}

#[derive(Clone)]
pub struct PatternData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum PatternBindMode {
    Claim,
    Ref,
    // RefMut,
}

impl std::fmt::Display for PatternBindMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, derive_macros::ExprDisplay)]
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
        bind_mode: PatternBindMode,
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
            Self::Binding {
                binding,
                bind_mode: _,
                data: _,
            } => consumer(binding.clone()),
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
