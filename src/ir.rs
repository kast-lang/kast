use super::*;

// TODO should be same ExprData - can have assignee expr with effects
#[derive(Clone)]
pub struct AssigneeExprData {
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct ExprData {
    pub ty: Type,
    pub span: Span,
    pub contexts: Contexts,
}

impl SubstituteBindings for ExprData {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Self {
            ty: self.ty.substitute_bindings(kast, cache),
            span: self.span,
            contexts: self.contexts.substitute_bindings(kast, cache),
        }
    }
}

#[derive(Clone)]
pub struct TypeExprData {
    pub span: Span,
}

impl SubstituteBindings for TypeExprData {
    type Target = Self;

    fn substitute_bindings(self, _kast: &Kast, _cache: &mut RecurseCache) -> Self::Target {
        Self { span: self.span }
    }
}

impl std::borrow::Borrow<Span> for TypeExprData {
    fn borrow(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings)]
pub struct MatchBranch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(
    Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings, derive_macros::Data,
)]
pub enum TypeExpr<Data = TypeExprData> {
    Ref {
        inner: Box<TypeExpr>,
        data: Data,
    },
    List {
        inner: Box<TypeExpr>,
        data: Data,
    },
    Unit {
        data: Data,
    },
    Tuple {
        fields: Tuple<TypeExpr>,
        data: Data,
    },
    Function {
        arg: Box<TypeExpr>,
        result: Box<TypeExpr>,
        contexts: Box<TypeExpr>,
        data: Data,
    },
    Expr {
        expr: Box<Expr>,
        data: Data,
    },
}

impl<Data: std::borrow::Borrow<Span>> TypeExpr<Data> {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a, Data>(&'a TypeExpr<Data>);
        impl<Data: std::borrow::Borrow<Span>> std::fmt::Display for Show<'_, Data> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    TypeExpr::Ref { .. } => write!(f, "Ref")?,
                    TypeExpr::List { .. } => write!(f, "List")?,
                    TypeExpr::Unit { .. } => write!(f, "Unit")?,
                    TypeExpr::Tuple { .. } => write!(f, "Tuple")?,
                    TypeExpr::Function { .. } => write!(f, "Function")?,
                    TypeExpr::Expr { .. } => write!(f, "Expr")?,
                }
                write!(f, " at {}", self.0.data().borrow())
            }
        }
        Show(self)
    }
}

#[derive(
    Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings, derive_macros::Data,
)]
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
        assignee: AssigneeExpr,
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
        #[substitute(skip)]
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
        // TODO only need name?
        #[display(skip)]
        #[substitute(skip)]
        compiler_scope: CompilerScope,
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
        #[substitute(skip)]
        raw: String,
        data: Data,
    },
    String {
        #[display(skip)]
        #[substitute(skip)]
        token: kast_ast::StringToken,
        data: Data,
    },
    Native {
        /// Name - expr that should evaluate to a string
        // name: Box<Expr>,
        #[substitute(skip)] // TODO I use interpolation in js here?
        name: String,
        #[display(skip)]
        #[substitute(skip)]
        compiler_scope: CompilerScope,
        data: Data,
    },
    Let {
        #[substitute(skip)]
        is_const_let: bool,
        pattern: Pattern,
        value: Option<Box<PlaceExpr>>,
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
        #[substitute(skip)] // TODO
        compiled: MaybeCompiledFn,
        data: Data,
    },
    Template {
        #[display(skip)]
        #[substitute(skip)] // TODO
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
        #[substitute(skip)]
        expr_root: bool,
        #[display(skip)]
        #[substitute(skip)]
        definition: Parc<ast::SyntaxDefinition>,
        values: Tuple<PlaceExpr>,
        #[substitute(skip)]
        hygiene: Hygiene,
        #[display(skip)]
        #[substitute(skip)]
        def_site: Option<CompilerScope>,
        data: Data,
    },
    ReadPlace {
        place: PlaceExpr,
        data: Data,
    },
    TargetDependent {
        branches: Vec<TargetDependentBranch<Expr>>,
        data: Data,
    },
    Type {
        expr: TypeExpr,
        data: Data,
    },
}

#[derive(
    Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings, derive_macros::Data,
)]
pub enum AssigneeExpr<Data = AssigneeExprData> {
    Placeholder {
        data: Data,
    },
    Unit {
        data: Data,
    },
    Tuple {
        tuple: Tuple<AssigneeExpr>,
        data: Data,
    },
    Place {
        place: PlaceExpr,
        data: Data,
    },
    Let {
        pattern: Pattern,
        data: Data,
    },
}

#[derive(
    Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings, derive_macros::Data,
)]
pub enum PlaceExpr<Data = ExprData> {
    #[substitute(with = substitute_place_binding)]
    Binding {
        binding: Parc<Binding>,
        data: Data,
    },
    FieldAccess {
        obj: Box<PlaceExpr>,
        #[substitute(skip)]
        field: tuple::Member<'static>,
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
    fn substitute_place_binding(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        match self {
            PlaceExpr::Binding { binding, data } => {
                match kast.scopes.interpreter.get(&binding.symbol) {
                    Some(value) => {
                        let value = value.clone_value().unwrap();
                        return Self::Temporary {
                            value: Box::new(Expr::Constant {
                                data: ExprData {
                                    ty: value.ty(),
                                    span: binding.symbol.span.clone(),
                                    contexts: Contexts::empty(),
                                },
                                value,
                            }),
                            data: data.substitute_bindings(kast, cache),
                        };
                    }
                    None => Self::Binding {
                        binding,
                        data: data.substitute_bindings(kast, cache),
                    },
                }
            }
            _ => unreachable!(),
        }
    }
}

impl PlaceExpr {
    pub fn new_temp(e: Expr) -> Self {
        PlaceExpr::Temporary {
            data: e.data().clone(),
            value: Box::new(e),
        }
    }
}

impl From<Expr> for TypeExpr {
    fn from(expr: Expr) -> Self {
        TypeExpr::Expr {
            data: TypeExprData {
                span: expr.data().span.clone(),
            },
            expr: Box::new(expr),
        }
    }
}

impl From<PlaceExpr> for TypeExpr {
    fn from(expr: PlaceExpr) -> Self {
        let expr: Expr = expr.into();
        expr.into()
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

impl From<PlaceExpr> for AssigneeExpr {
    fn from(place: PlaceExpr) -> Self {
        let data = place.data();
        Self::Place {
            data: AssigneeExprData {
                ty: data.ty.clone(),
                span: data.span.clone(),
            },
            place,
        }
    }
}

pub struct CompiledFn {
    pub arg: Pattern,
    pub body: Expr,
}

pub type MaybeCompiledFn = executor::Spawned<Parc<CompiledFn>>;

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
            | Expr::Type { .. }
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
            Expr::Assign {
                assignee,
                value: _,
                data: _,
            } => {
                assignee.collect_new_bindings(consumer);
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
            Expr::TargetDependent { .. } => {}
        }
    }
}

impl std::borrow::Borrow<Span> for ExprData {
    fn borrow(&self) -> &Span {
        &self.span
    }
}

impl<Data> AssigneeExpr<Data> {
    pub fn collect_new_bindings(&self, consumer: &mut impl FnMut(Parc<Binding>)) {
        match self {
            AssigneeExpr::Placeholder { data: _ } => {}
            AssigneeExpr::Unit { data: _ } => {}
            AssigneeExpr::Tuple { tuple, data: _ } => {
                for field in tuple.values() {
                    field.collect_new_bindings(consumer);
                }
            }
            AssigneeExpr::Place { place: _, data: _ } => {}
            AssigneeExpr::Let { pattern, data: _ } => pattern.collect_bindings(consumer),
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
                    Expr::Type { .. } => write!(f, "type expr")?,
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
                    Expr::Native {
                        name: _,
                        compiler_scope: _,
                        data: _,
                    } => write!(f, "native expr")?,
                    Expr::Ast { .. } => write!(f, "ast expr")?,
                    Expr::Let { .. } => write!(f, "let expr")?,
                    Expr::Call { .. } => write!(f, "call expr")?,
                    Expr::Instantiate { .. } => write!(f, "instantiate expr")?,
                    Expr::ReadPlace { .. } => write!(f, "readref expr")?,
                    Expr::TargetDependent { .. } => write!(f, "target dependent")?,
                }
                write!(f, " at {}", self.0.data().borrow())
            }
        }
        Show(self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Symbol {
    pub name: std::sync::Arc<str>,
    pub span: Span,
    pub id: Id,
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {}", self.name, self.span)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Symbol {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn id(&self) -> Id {
        self.id
    }
}

pub struct Binding {
    pub symbol: Symbol,
    pub ty: Type,
    pub mutability: Mutability,
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

impl SubstituteBindings for PatternData {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Self {
            ty: self.ty.substitute_bindings(kast, cache),
            span: self.span,
        }
    }
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

#[derive(
    Clone, derive_macros::ExprDisplay, derive_macros::ExprSubstituteBindings, derive_macros::Data,
)]
pub enum Pattern<Data = PatternData> {
    /// matches anything
    Placeholder {
        data: Data,
    },
    Unit {
        data: Data,
    },
    Binding {
        #[substitute(skip)]
        binding: Parc<Binding>,
        #[substitute(skip)]
        bind_mode: PatternBindMode,
        data: Data,
    },
    Tuple {
        tuple: Tuple<Pattern>,
        data: Data,
    },
    Variant {
        #[substitute(skip)]
        name: String,
        value: Option<Box<Pattern>>,
        data: Data,
    },
}

impl<Data> Pattern<Data> {
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
