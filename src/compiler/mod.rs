use super::*;

mod builtins;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Hygiene {
    DefSite,
}

impl std::fmt::Debug for Hygiene {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DefSite => write!(f, "DefSite"),
        }
    }
}

impl std::fmt::Display for Hygiene {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

fn ast_as_member(ast: &Ast) -> Option<tuple::Member<'_>> {
    Some(match ast {
        kast_ast::Ast::Simple { token, data: _ } => match token {
            Token::Ident { name, .. } => tuple::Member::Named(std::borrow::Cow::Borrowed(name)),
            Token::Number { raw } => {
                let index: usize = raw.parse().ok()?;
                tuple::Member::Unnamed(index)
            }
            _ => return None,
        },
        _ => return None,
    })
}

// TODO: Use string to fix type infer bug. Important
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AstData {
    pub span: Span,
    pub hygiene: Hygiene,
    pub def_site: Option<CompilerScope>,
}

impl ast::HasSpan for AstData {
    fn span(&self) -> &Span {
        &self.span
    }
}

pub type Ast<T = AstData> = ast::Ast<T>;

pub fn init_ast(ast: Ast<Span>) -> Ast {
    ast.map_data(|span| AstData {
        span,
        hygiene: Hygiene::DefSite,
        def_site: None,
    })
}

use std::collections::HashMap;

#[derive(Clone)]
pub struct State {
    bindings_mutability: Mutability,
}

impl State {
    pub fn new() -> Self {
        Self {
            bindings_mutability: Mutability::ReadOnly,
        }
    }
}

struct ListCollector<'a> {
    macro_name: &'a str,
    a: &'a str,
    b: &'a str,
}

struct ListCollected<'a> {
    list: Vec<&'a Ast>,
    all_binary: bool,
}

impl ListCollector<'_> {
    fn collect<'a>(&self, ast: &'a Ast) -> eyre::Result<ListCollected<'a>> {
        let mut result = Vec::new();
        let mut node = ast;
        let mut reverse = false;
        let mut all_binary = true;
        loop {
            match node {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == self.macro_name => {
                    let ([a], [b]) = values
                        .as_ref()
                        .into_named_opt([self.a], [self.b])
                        .wrap_err_with(|| "Macro received incorrect arguments")?;
                    if b.is_none() {
                        all_binary = false;
                    }
                    match definition.associativity {
                        ast::Associativity::Left => {
                            reverse = true;
                            node = a;
                            if let Some(b) = b {
                                result.push(b);
                            }
                        }
                        ast::Associativity::Right => {
                            if let Some(b) = b {
                                node = b;
                                result.push(a);
                            } else {
                                node = a;
                            }
                        }
                    }
                }
                _ => break,
            }
        }
        result.push(node);
        if reverse {
            result.reverse();
        }
        Ok(ListCollected {
            list: result,
            all_binary,
        })
    }
}

pub struct Cache {
    builtins: builtins::Builtins,
    syntax_definitions: Mutex<HashMap<Parc<ast::SyntaxDefinition>, std::task::Poll<Value>>>,
    pub casts: Mutex<CastMap>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            builtins: builtins::Builtins::new(),
            syntax_definitions: Default::default(),
            casts: Default::default(),
        }
    }

    pub fn register_syntax(&self, definition: &Parc<ast::SyntaxDefinition>) {
        let mut definitions = self.syntax_definitions.lock().unwrap();
        if definitions.get(definition).is_none() {
            definitions.insert(definition.clone(), std::task::Poll::Pending);
            tracing::trace!("registered syntax {:?}", definition.name);
        }
    }

    fn find_macro(&self, definition: &Parc<ast::SyntaxDefinition>) -> eyre::Result<Macro> {
        Ok(
            if let Some(builtin_macro_name) = definition.name.strip_prefix("builtin macro ") {
                let r#impl = *self
                    .builtins
                    .get(builtin_macro_name)
                    .ok_or_else(|| eyre!("builtin macro {builtin_macro_name:?} not found"))?;
                Macro::Builtin {
                    name: builtin_macro_name.to_owned(),
                    r#impl,
                }
            } else {
                let name = definition.name.as_str();
                if let Some(r#macro) = self.syntax_definitions.lock().unwrap().get(definition) {
                    let r#macro = match r#macro {
                        std::task::Poll::Pending => {
                            eyre::bail!("{name} can not be used until it is defined")
                        }
                        std::task::Poll::Ready(value) => value,
                    };
                    match r#macro.clone().into_inferred()? {
                        ValueShape::Macro(f) => Macro::UserDefined(f.f.clone()),
                        _ => Macro::Value(r#macro.clone()),
                    }
                } else {
                    eyre::bail!("{name:?} was not defined??? how is that even possible?");
                }
            },
        )
    }
}

enum Macro {
    Builtin {
        name: String,
        r#impl: builtins::BuiltinMacro,
    },
    UserDefined(Function),
    /// Not really a macro :)
    Value(Value),
}

#[async_trait]
pub trait Compilable: Sized {
    const CTY: CompiledType;
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self>;
    async fn compile_call(
        kast: &mut Kast,
        f: Value,
        args: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Self>;
    fn r#macro(
        macro_name: &str,
        f: builtins::BuiltinMacro,
    ) -> impl for<'a> Fn(&'a mut Kast, &'a Ast) -> BoxFuture<'a, eyre::Result<Self>> + Send {
        move |kast, ast| {
            let macro_name = macro_name.to_owned();
            async move {
                Ok(Self::from_compiled(
                    f(kast, Self::CTY, ast)
                        .await
                        .wrap_err_with(|| format!("in builtin macro {macro_name:?}"))?,
                ))
            }
            .boxed()
        }
    }
    async fn expand_macros(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        let Ast::Complex {
            definition,
            values,
            data,
        } = ast
        else {
            unreachable!()
        };
        Ok(match kast.cache.compiler.find_macro(definition)? {
            Macro::Builtin { name, r#impl } => Self::r#macro(&name, r#impl)(kast, ast).await?,
            Macro::UserDefined(r#macro) => {
                let arg = ValueShape::Tuple(TupleValue::new_unnamed(
                    values
                        .as_ref()
                        .map(|ast| ValueShape::Ast(kast.set_def_site(ast)).into()),
                    Type::new_not_inferred("macro arg"),
                ))
                .into();
                // hold on
                let expanded = kast.call_fn(r#macro, arg).await?;
                let expanded = match expanded.clone().into_inferred()? {
                    ValueShape::Ast(ast) => ast,
                    _ => eyre::bail!(
                        "macro {name} did not expand to an ast, but to {expanded}",
                        name = definition.name,
                    ),
                };
                kast.compile(&expanded).await?
            }
            Macro::Value(value) => {
                Self::compile_call(kast, value, values, data.span.clone()).await?
            } // _ => eyre::bail!("{macro} is not a macro"),
        })
    }
    fn from_compiled(compiled: Compiled) -> Self;
}

enum SingleTokenExpr {
    Place(PlaceExpr),
    Expr(Expr),
}

impl From<SingleTokenExpr> for PlaceExpr {
    fn from(value: SingleTokenExpr) -> Self {
        match value {
            SingleTokenExpr::Place(e) => e,
            SingleTokenExpr::Expr(e) => PlaceExpr::new_temp(e),
        }
    }
}
impl From<SingleTokenExpr> for Expr {
    fn from(value: SingleTokenExpr) -> Self {
        match value {
            SingleTokenExpr::Place(e) => e.into(),
            SingleTokenExpr::Expr(e) => e,
        }
    }
}

impl SingleTokenExpr {
    async fn compile(kast: &mut Kast, token: &Token, data: &AstData) -> eyre::Result<Self> {
        Ok(match token {
            Token::Ident {
                raw: _,
                name,
                is_raw: _,
            } => {
                let value = kast
                    .scopes
                    .compiler
                    .lookup(name, data.hygiene, kast.spawn_id)
                    .await
                    .ok_or_else(|| eyre!("{name:?} not found"))?;
                match value.clone().inferred() {
                    Some(ValueShape::Binding(binding)) => SingleTokenExpr::Place(
                        PlaceExpr::Binding {
                            binding: binding.clone(),
                            data: data.span.clone(),
                        }
                        .init(kast)
                        .await?,
                    ),
                    _ => SingleTokenExpr::Expr(
                        Expr::Constant {
                            value: value.clone(),
                            data: data.span.clone(),
                        }
                        .init(kast)
                        .await?,
                    ),
                }
            }
            Token::String(token) => SingleTokenExpr::Expr(
                Expr::String {
                    token: token.clone(),
                    data: data.span.clone(),
                }
                .init(kast)
                .await?,
            ),
            Token::Number { raw } => SingleTokenExpr::Expr(
                Expr::Number {
                    raw: raw.clone(),
                    data: data.span.clone(),
                }
                .init(kast)
                .await?,
            ),
            Token::Comment { .. } | Token::Punctuation { .. } | Token::Eof => unreachable!(),
        })
    }
}

#[async_trait]
impl Compilable for PlaceExpr {
    const CTY: CompiledType = CompiledType::PlaceExpr;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::PlaceExpr(e) => e,
            _ => unreachable!(),
        }
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        Ok(match ast {
            kast_ast::Ast::Simple { token, data } => {
                SingleTokenExpr::compile(kast, token, data).await?.into()
            }
            kast_ast::Ast::Complex { .. } => Self::expand_macros(kast, ast).await?,
            kast_ast::Ast::SyntaxDefinition { .. } | kast_ast::Ast::FromScratch { .. } => {
                PlaceExpr::new_temp(kast.compile(ast).await?)
            }
        })
    }
    async fn compile_call(
        kast: &mut Kast,
        f: Value,
        args: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Self> {
        Ok(Self::new_temp(
            Expr::compile_call(kast, f, args, span).await?,
        ))
    }
}

impl TryFrom<SingleTokenExpr> for AssigneeExpr {
    type Error = eyre::Report;
    fn try_from(value: SingleTokenExpr) -> Result<Self, Self::Error> {
        Ok(match value {
            SingleTokenExpr::Place(e) => Self::Place {
                data: AssigneeExprData {
                    ty: e.data().ty.clone(),
                    span: e.data().span.clone(),
                },
                place: e,
            },
            SingleTokenExpr::Expr(_) => eyre::bail!("expected assignee expr"),
        })
    }
}

#[async_trait]
impl Compilable for AssigneeExpr {
    const CTY: CompiledType = CompiledType::AssigneeExpr;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::AssigneeExpr(e) => e,
            _ => unreachable!(),
        }
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        Ok(match ast {
            kast_ast::Ast::Simple { token, data } => SingleTokenExpr::compile(kast, token, data)
                .await?
                .try_into()?,
            kast_ast::Ast::Complex { .. } => Self::expand_macros(kast, ast).await?,
            kast_ast::Ast::SyntaxDefinition { .. } | kast_ast::Ast::FromScratch { .. } => {
                eyre::bail!("expected assignee expr");
            }
        })
    }
    async fn compile_call(
        _kast: &mut Kast,
        _f: Value,
        _args: &Tuple<Ast>,
        _span: Span,
    ) -> eyre::Result<Self> {
        eyre::bail!("expected assignee expr")
    }
}

#[async_trait]
impl Compilable for Expr {
    const CTY: CompiledType = CompiledType::Expr;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::Expr(e) => e,
            _ => unreachable!(),
        }
    }
    async fn compile_call(
        kast: &mut Kast,
        f: Value,
        args: &Tuple<Ast>,
        span: Span,
    ) -> eyre::Result<Self> {
        Ok(Expr::Call {
            // TODO not constant?
            f: Box::new(
                Expr::Constant {
                    value: f.clone(),
                    data: span.clone(),
                }
                .init(kast)
                .await?,
            ),
            arg: Box::new(
                Expr::Tuple {
                    tuple: {
                        let mut tuple = Tuple::empty();
                        for (member, field) in args.as_ref() {
                            tuple.add_member(member, kast.compile(field).await?);
                        }
                        tuple
                    },
                    data: span.clone(),
                }
                .init(kast)
                .await?,
            ),
            data: span,
        }
        .init(kast)
        .await?)
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        Ok(match ast {
            Ast::Simple { token, data } => {
                SingleTokenExpr::compile(kast, token, data).await?.into()
            }
            Ast::Complex { .. } => Self::expand_macros(kast, ast).await?,
            Ast::SyntaxDefinition { def, data } => {
                kast.insert_syntax(def.clone())?;
                kast.add_local(
                    kast.scopes
                        .compiler
                        .new_symbol(&def.name, data.span.clone()),
                    ValueShape::SyntaxDefinition(def.clone()).into(),
                );
                kast.cache.compiler.register_syntax(def);

                Expr::Constant {
                    value: ValueShape::Unit.into(),
                    data: data.span.clone(),
                }
                .init(kast)
                .await?
            }
            Ast::FromScratch { next, data } => {
                // kast.compiler.syntax_definitions.lock().unwrap().clear();
                if let Some(next) = next {
                    kast.compile(next).await?
                } else {
                    Expr::Constant {
                        value: ValueShape::Unit.into(),
                        data: data.span.clone(),
                    }
                    .init(kast)
                    .await?
                }
            }
        })
    }
}

async fn compile_ident_pattern(
    kast: &mut Kast,
    ident_ast: &Ast,
    bind_mode: PatternBindMode,
) -> eyre::Result<Pattern> {
    let Ast::Simple {
        token:
            kast_ast::Token::Ident {
                raw: _,
                name,
                is_raw: _,
            },
        data: AstData {
            span,
            hygiene,
            def_site: _,
        },
    } = ident_ast
    else {
        eyre::bail!("expected identifier");
    };
    let compiler_scope = match hygiene {
        Hygiene::DefSite => kast.scopes.compiler.clone(),
    };
    let binding = Parc::new(Binding {
        symbol: kast.new_symbol(name, span.clone()),
        ty: Type::new_not_inferred(&format!("{name} at {span}")),
        mutability: kast.compiler.bindings_mutability,
        compiler_scope,
    });
    Ok(Pattern::Binding {
        binding,
        bind_mode,
        data: span.clone(),
    }
    .init()?)
}

#[async_trait]
impl Compilable for Pattern {
    const CTY: CompiledType = CompiledType::Pattern;
    fn from_compiled(compiled: Compiled) -> Self {
        match compiled {
            Compiled::Pattern(p) => p,
            _ => unreachable!(),
        }
    }
    async fn compile_call(
        _kast: &mut Kast,
        f: Value,
        _args: &Tuple<Ast>,
        _span: Span,
    ) -> eyre::Result<Self> {
        eyre::bail!("{f} is not a macro")
    }
    async fn compile(kast: &mut Kast, ast: &Ast) -> eyre::Result<Self> {
        tracing::debug!("compiling {}...", ast.show_short());
        let result = match ast {
            Ast::Simple { token, data: _ } => match token {
                Token::Ident { .. } => {
                    compile_ident_pattern(kast, ast, PatternBindMode::Claim).await?
                }
                Token::String { .. } => todo!(),
                Token::Number { raw: _ } => todo!(),
                Token::Comment { .. } | Token::Punctuation { .. } | Token::Eof => unreachable!(),
            },
            Ast::Complex { .. } => Self::expand_macros(kast, ast).await?,
            Ast::SyntaxDefinition { def: _, data: _ } => todo!(),
            Ast::FromScratch { next: _, data: _ } => todo!(),
        };
        tracing::debug!("compiled {}", ast.show_short());
        Ok(result)
    }
}

impl Kast {
    fn inject_conditional_bindings(&mut self, expr: &Expr, condition: bool) {
        expr.collect_bindings(
            &mut |binding| {
                self.inject_binding(&binding);
            },
            Some(condition),
        );
    }
    fn inject_binding(&mut self, binding: &Parc<Binding>) {
        binding.compiler_scope.insert(
            binding.symbol.name(),
            &binding.symbol.span,
            ValueShape::Binding(binding.clone()).into(),
        );
        self.scopes.interpreter.insert(
            &binding.symbol,
            ValueShape::Binding(binding.clone()).into(),
            binding.mutability,
        );
    }
    fn inject_bindings(&mut self, pattern: &Pattern) {
        pattern.collect_bindings(&mut |binding| self.inject_binding(&binding));
    }
    fn inject_assignee_bindings(&mut self, assignee: &AssigneeExpr) {
        assignee.collect_new_bindings(&mut |binding| self.inject_binding(&binding));
    }
    pub fn set_def_site(&self, ast: &Ast) -> Ast {
        let mut ast = ast.clone();
        // println!("set def site of {ast}???");
        let def_site = &mut ast.data_mut().def_site;
        if def_site.is_none() {
            *def_site = Some(self.scopes.compiler.clone());
            // println!("set def site of {ast} = {:?}", self.scopes.compiler.id());
        }
        ast
    }
    pub async fn compile<T: Compilable>(&mut self, ast: &Ast) -> eyre::Result<T> {
        let mut kast = match &ast.data().def_site {
            Some(scope) => self.with_scopes(self.scopes.enter_def_site(scope.clone())),
            None => self.clone(),
        };
        let result = T::compile(&mut kast, ast)
            .boxed()
            .await
            .wrap_err_with(|| format!("while compiling {}", ast.show_short()))?;
        tracing::trace!("compiled {ast}");
        Ok(result)
    }
    async fn compile_into(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        Ok(match ty {
            CompiledType::Expr => Compiled::Expr(self.compile(ast).await?),
            CompiledType::Pattern => Compiled::Pattern(self.compile(ast).await?),
            CompiledType::PlaceExpr => Compiled::PlaceExpr(self.compile(ast).await?),
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(self.compile(ast).await?),
        })
    }
    fn compile_fn_body(
        &mut self,
        arg: Pattern,
        body: &Ast,
        contexts: Contexts,
        result_type: Type,
    ) -> MaybeCompiledFn {
        self.cache.executor.spawn({
            let mut kast = self.spawn_clone();
            let body: Ast = body.clone();
            let result_type = result_type.clone();
            async move {
                let mut body: Expr = kast.compile(&body).await?;
                body.data_mut().ty.make_same(result_type)?;
                body.data_mut().contexts.0.make_same(contexts.0)?;
                Ok(Parc::new(CompiledFn { body, arg }))
            }
            .map_err(|err: eyre::Report| err.wrap_err("Failed to compile fn"))
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompiledType {
    Expr,
    PlaceExpr,
    AssigneeExpr,
    Pattern,
}

pub enum Compiled {
    AssigneeExpr(AssigneeExpr),
    Expr(Expr),
    PlaceExpr(PlaceExpr),
    Pattern(Pattern),
}

impl Compiled {
    fn ty_mut(&mut self) -> &mut Type {
        match self {
            Compiled::AssigneeExpr(e) => &mut e.data_mut().ty,
            Compiled::Expr(e) => &mut e.data_mut().ty,
            Compiled::PlaceExpr(e) => &mut e.data_mut().ty,
            Compiled::Pattern(p) => &mut p.data_mut().ty,
        }
    }
    fn expect_expr(self) -> Option<Expr> {
        match self {
            Compiled::Expr(e) => Some(e),
            _ => None,
        }
    }
    fn expect_assignee_expr(self) -> Option<AssigneeExpr> {
        match self {
            Compiled::AssigneeExpr(e) => Some(e),
            _ => None,
        }
    }
    #[allow(dead_code)]
    fn expect_place_expr(self) -> Option<PlaceExpr> {
        match self {
            Self::PlaceExpr(e) => Some(e),
            _ => None,
        }
    }
    fn expect_pattern(self) -> Option<Pattern> {
        match self {
            Compiled::Pattern(p) => Some(p),
            _ => None,
        }
    }
}

fn get_complex(ast: &Ast) -> (&Tuple<Ast>, Span) {
    match ast {
        Ast::Complex {
            definition: _,
            values,
            data,
        } => (values, data.span.clone()),
        _ => unreachable!(),
    }
}

fn infer_type_variant(description: &str, name: String, value_ty: Option<Type>) -> Type {
    let var = inference::Var::new(description);
    var.add_check(move |ty: &TypeShape| {
        let ty = ty.clone().expect_variant()?;
        match ty.variants.iter().find(|variant| variant.name == name) {
            Some(variant) => match (&variant.value, &value_ty) {
                (None, None) => Ok(inference::CheckResult::Completed),
                (None, Some(_)) => {
                    eyre::bail!("variant {name} did not expect a value")
                }
                (Some(_), None) => {
                    eyre::bail!("variant {name} expected a value")
                }
                (Some(expected_ty), Some(actual_ty)) => {
                    expected_ty.clone().make_same(actual_ty.clone())?;
                    Ok(inference::CheckResult::Completed)
                }
            },
            None => {
                eyre::bail!("variant {name} not found in type {ty}")
            }
        }
    })
    .expect("checks failed");
    var.into()
}

// All comments fully authored by the Kuviman.
impl Kast {
    async fn cast_result_ty<'a>(
        &self,
        value: impl FnOnce() -> BoxFuture<'a, eyre::Result<Value>>,
        target: Value,
    ) -> eyre::Result<Type> {
        Ok(match target.clone().inferred() {
            Some(ValueShape::Template(template)) => {
                let kast = self.enter_scope();
                let value = value().await?;
                kast.await_compiled(&template.compiled)
                    .await?
                    .body
                    .data()
                    .ty
                    .infer_as(TypeShape::Type)?;
                let ty = kast.call_fn(template, value).await?;
                ty.into_type()?
            }
            Some(ValueShape::Type(ty)) => ty,
            _ => eyre::bail!("casting to {} is not possible", target.ty()),
        })
    }
}

impl AssigneeExpr<Span> {
    /// Initialize expr data
    pub fn init(self, _kast: &Kast) -> BoxFuture<'_, eyre::Result<AssigneeExpr>> {
        let r#impl = async {
            Ok(match self {
                Self::Placeholder { data: span } => AssigneeExpr::Placeholder {
                    data: AssigneeExprData {
                        ty: Type::new_not_inferred("placeholder assignee"),
                        span,
                    },
                },
                Self::Unit { data: span } => AssigneeExpr::Unit {
                    data: AssigneeExprData {
                        ty: TypeShape::Unit.into(),
                        span,
                    },
                },
                // NOTE: We should increase the activation watermark on stream
                Self::Tuple { tuple, data: span } => AssigneeExpr::Tuple {
                    data: AssigneeExprData {
                        ty: TypeShape::Tuple(TupleType {
                            name: inference::MaybeNotInferred::new_not_inferred("tuple"),
                            fields: tuple.as_ref().map(|field| field.data().ty.clone()),
                        })
                        .into(),
                        span,
                    },
                    tuple,
                },
                Self::Place { place, data: span } => AssigneeExpr::Place {
                    data: AssigneeExprData {
                        ty: place.data().ty.clone(),
                        span,
                    },
                    place,
                },
                Self::Let {
                    pattern,
                    data: span,
                } => AssigneeExpr::Let {
                    data: AssigneeExprData {
                        ty: pattern.data().ty.clone(),
                        span,
                    },
                    pattern,
                },
            })
        };
        r#impl.boxed()
    }
}

impl PlaceExpr<Span> {
    pub fn init(self, _kast: &Kast) -> BoxFuture<'_, eyre::Result<PlaceExpr>> {
        let r#impl = async {
            Ok(match self {
                PlaceExpr::Deref { r#ref, data: span } => {
                    let value_ty = Type::new_not_inferred(&format!("deref at {span}"));
                    r#ref
                        .data()
                        .ty
                        .clone()
                        .make_same(TypeShape::Ref(value_ty.clone()).into())?;
                    PlaceExpr::Deref {
                        data: ExprData {
                            contexts: r#ref.data().contexts.clone(),
                            ty: value_ty,
                            span,
                        },
                        r#ref,
                    }
                }
                PlaceExpr::Temporary { value, data: span } => PlaceExpr::Temporary {
                    data: ExprData {
                        ty: value.data().ty.clone(),
                        span,
                        contexts: value.data().contexts.clone(),
                    },
                    value,
                },
                PlaceExpr::Binding {
                    binding,
                    data: span,
                } => PlaceExpr::Binding {
                    data: ExprData {
                        ty: binding.ty.clone(),
                        span,
                        contexts: Contexts::empty_growable(),
                    },
                    binding,
                },
                PlaceExpr::FieldAccess {
                    obj,
                    field,
                    data: span,
                } => PlaceExpr::FieldAccess {
                    data: ExprData {
                        ty: {
                            let ty = Type::new_not_inferred("obj.field");
                            obj.data().ty.var().add_check({
                                let ty = ty.clone();
                                let field = field.clone();
                                move |obj_ty| {
                                    match &obj_ty {
                                        TypeShape::Tuple(obj_ty) => {
                                            match obj_ty.fields.get(field.as_ref()) {
                                                Some(field_ty) => {
                                                    ty.clone().make_same(field_ty.clone())?
                                                }
                                                None => {
                                                    eyre::bail!(
                                                        "{obj_ty} does not have field {field:?}"
                                                    )
                                                }
                                            }
                                        }
                                        TypeShape::SyntaxModule => {
                                            ty.infer_as(TypeShape::SyntaxDefinition)?
                                        }
                                        _ => eyre::bail!("can not get fields of type {obj_ty}"),
                                    }
                                    Ok(inference::CheckResult::Completed)
                                }
                            })?;
                            ty
                        },
                        span,
                        contexts: obj.data().contexts.clone(),
                    },
                    obj,
                    field,
                },
            })
        };
        r#impl.boxed()
    }
}

impl Expr<Span> {
    /// Initialize expr data
    pub fn init(self, kast: &Kast) -> BoxFuture<'_, eyre::Result<Expr>> {
        let r#impl = async {
            Ok(match self {
                Expr::Ref { place, data: span } => {
                    let place_ty = place.data().ty.clone();
                    let ty = Type::new_not_inferred_with_default(
                        &format!("& expr at {span}"),
                        TypeShape::Ref(place_ty.clone()),
                    );
                    ty.var().add_check(move |inferred| {
                        match inferred {
                            TypeShape::Type => {
                                place_ty.clone().make_same(TypeShape::Type.into())?;
                            }
                            TypeShape::Ref(inferred_inner_ty) => {
                                inferred_inner_ty.clone().make_same(place_ty.clone())?
                            }
                            _ => eyre::bail!("ref expr inferred as {inferred}"),
                        }
                        Ok(inference::CheckResult::Completed)
                    })?;
                    Expr::Ref {
                        data: ExprData {
                            ty,
                            span,
                            contexts: place.data().contexts.clone(),
                        },
                        place,
                    }
                }
                Expr::ReadPlace { place, data: span } => Expr::ReadPlace {
                    data: ExprData {
                        ty: place.data().ty.clone(),
                        span,
                        contexts: place.data().contexts.clone(), // TODO read effect
                    },
                    place,
                },
                Expr::And {
                    lhs,
                    rhs,
                    data: span,
                } => {
                    lhs.data().ty.infer_as(TypeShape::Bool)?;
                    rhs.data().ty.infer_as(TypeShape::Bool)?;
                    Expr::And {
                        data: ExprData {
                            ty: TypeShape::Bool.into(),
                            span,
                            contexts: Contexts::merge_two(
                                &lhs.data().contexts,
                                &rhs.data().contexts,
                            )?,
                        },
                        lhs,
                        rhs,
                    }
                }
                Expr::Or {
                    lhs,
                    rhs,
                    data: span,
                } => {
                    lhs.data().ty.infer_as(TypeShape::Bool)?;
                    rhs.data().ty.infer_as(TypeShape::Bool)?;
                    Expr::Or {
                        data: ExprData {
                            ty: TypeShape::Bool.into(),
                            span,
                            contexts: Contexts::merge_two(
                                &lhs.data().contexts,
                                &rhs.data().contexts,
                            )?,
                        },
                        lhs,
                        rhs,
                    }
                }
                Expr::Assign {
                    assignee,
                    value,
                    data: span,
                } => {
                    assignee
                        .data()
                        .ty
                        .clone()
                        .make_same(value.data().ty.clone())?;
                    Expr::Assign {
                        data: ExprData {
                            ty: TypeShape::Unit.into(),
                            span,
                            contexts: value.data().contexts.clone(), // TODO write effect
                        },
                        assignee,
                        value,
                    }
                }
                Expr::List { values, data: span } => {
                    let mut element_ty =
                        Type::new_not_inferred(&format!("element type of list expr at {span}"));
                    for value in &values {
                        element_ty.make_same(value.data().ty.clone())?;
                    }
                    let ty = Type::new_not_inferred_with_default(
                        &format!("list expr at {span}"),
                        TypeShape::List(element_ty.clone()),
                    );
                    ty.var().add_check(move |inferred| {
                        match inferred {
                            TypeShape::Type => {
                                element_ty.clone().make_same(TypeShape::Type.into())?;
                            }
                            TypeShape::List(inferred_elem_ty) => {
                                inferred_elem_ty.clone().make_same(element_ty.clone())?
                            }
                            _ => eyre::bail!("list expr inferred as {inferred}"),
                        }
                        Ok(inference::CheckResult::Completed)
                    })?;
                    Expr::List {
                        data: ExprData {
                            ty,
                            span,
                            contexts: Contexts::merge(
                                values.iter().map(|value| &value.data().contexts),
                            )?,
                        },
                        values,
                    }
                }
                Expr::Unwind {
                    name,
                    value,
                    data: span,
                } => {
                    name.data()
                        .ty
                        .infer_as(TypeShape::UnwindHandle(value.data().ty.clone()))?;
                    Expr::Unwind {
                        data: ExprData {
                            ty: Type::new_not_inferred(&format!("unwind expr at {span}")), // TODO never
                            span,
                            contexts: Contexts::merge_two(
                                &name.data().contexts,
                                &value.data().contexts,
                            )?, // TODO unwind effect so it checks that its inside unwindable block
                        },
                        name,
                        value,
                    }
                }
                Expr::Unwindable {
                    name,
                    body,
                    data: span,
                } => {
                    name.data()
                        .ty
                        .infer_as(TypeShape::UnwindHandle(body.data().ty.clone()))?;
                    Expr::Unwindable {
                        data: ExprData {
                            ty: body.data().ty.clone(),
                            span,
                            contexts: body.data().contexts.clone(), // TODO unwind effect should be
                                                                    // removed
                        },
                        name,
                        body,
                    }
                }
                Expr::InjectContext {
                    context,
                    data: span,
                } => Expr::InjectContext {
                    data: ExprData {
                        ty: TypeShape::Unit.into(),
                        span,
                        contexts: context.data().contexts.clone(),
                    },
                    context,
                },
                Expr::CurrentContext { data: span } => {
                    let context_ty = Type::new_not_inferred(&format!("current expr at {span}"));
                    Expr::CurrentContext {
                        data: ExprData {
                            ty: context_ty.clone(),
                            span,
                            contexts: Contexts::single(context_ty)?,
                        },
                    }
                }
                Expr::Unit { data: span } => Expr::Unit {
                    data: ExprData {
                        ty: Type::new_not_inferred_with_default(
                            &format!("unit expr at {span}"),
                            TypeShape::Unit,
                        ),
                        span,
                        contexts: Contexts::empty_growable(),
                    },
                },
                Expr::FunctionType {
                    arg,
                    contexts,
                    result,
                    data: span,
                } => {
                    arg.data().ty.infer_as(TypeShape::Type)?;
                    // TODO contexts.data().ty.expect_inferred(TypeShape::Contexts)?;
                    result.data().ty.infer_as(TypeShape::Type)?;
                    Expr::FunctionType {
                        data: ExprData {
                            ty: TypeShape::Type.into(),
                            span,
                            contexts: Contexts::merge(
                                [&arg.data().contexts, &result.data().contexts]
                                    .into_iter()
                                    .chain(
                                        contexts
                                            .as_ref()
                                            .into_iter()
                                            .map(|contexts| &contexts.data().contexts),
                                    ),
                            )?,
                        },
                        arg,
                        contexts,
                        result,
                    }
                }
                Expr::Cast {
                    value,
                    target,
                    data: span,
                } => {
                    Expr::Cast {
                        target: target.clone(),
                        data: ExprData {
                            ty: kast
                                .cast_result_ty(
                                    || async { kast.enter_scope().eval(&value).await }.boxed(),
                                    target,
                                )
                                .await?,
                            span,
                            contexts: value.data().contexts.clone(),
                        },
                        value, // TODO not evaluate twice if template?
                    }
                }
                // programming in brainfuck -- I don't want to do that, it's not that painful...
                Expr::Match {
                    value,
                    branches,
                    data: span,
                } => {
                    let mut result_ty = Type::new_not_inferred(&format!("match expr at {span}"));
                    let mut value_ty = value.data().ty.clone();
                    for branch in &branches {
                        // println!("{} ++ {}", branch.pattern.data().ty, value_ty);
                        value_ty
                            .make_same(branch.pattern.data().ty.clone())
                            .wrap_err_with(|| {
                                eyre!("match branch pattern at {}", branch.pattern.data().span)
                            })?;
                        result_ty
                            .make_same(branch.body.data().ty.clone())
                            .wrap_err_with(|| {
                                eyre!("match branch body at {}", branch.pattern.data().span)
                            })?;
                    }
                    Expr::Match {
                        data: ExprData {
                            ty: result_ty,
                            span,
                            contexts: {
                                let mut contexts = value.data().contexts.clone();
                                for MatchBranch {
                                    pattern: _,
                                    body: branch_body,
                                } in &branches
                                {
                                    contexts = Contexts::merge_two(
                                        &contexts,
                                        &branch_body.data().contexts,
                                    )?;
                                }
                                contexts
                            },
                        },
                        value,
                        branches,
                    }
                }
                Expr::Is {
                    value,
                    pattern,
                    data: span,
                } => {
                    value
                        .data()
                        .ty
                        .clone()
                        .make_same(pattern.data().ty.clone())?;
                    Expr::Is {
                        data: ExprData {
                            ty: TypeShape::Bool.into(),
                            span,
                            contexts: value.data().contexts.clone(),
                        },
                        value,
                        pattern,
                    }
                }
                Expr::Newtype { def, data: span } => Expr::Newtype {
                    data: ExprData {
                        ty: TypeShape::Type.into(),
                        span,
                        contexts: def.data().contexts.clone(),
                    },
                    def,
                },
                Expr::MakeMultiset { values, data: span } => Expr::MakeMultiset {
                    data: ExprData {
                        ty: TypeShape::Multiset.into(),
                        span,
                        contexts: Contexts::merge(
                            values.iter().map(|value| &value.data().contexts),
                        )?,
                    },
                    values,
                },
                Expr::Variant {
                    name,
                    value,
                    data: span,
                } => {
                    let value_ty = value.as_ref().map(|value| value.data().ty.clone());
                    Expr::Variant {
                        data: ExprData {
                            ty: infer_type_variant(
                                &format!("variant at {span}"),
                                name.clone(),
                                value_ty,
                            ),
                            span,
                            contexts: match &value {
                                Some(value) => value.data().contexts.clone(),
                                None => Contexts::empty_growable(),
                            },
                        },
                        name,
                        value,
                    }
                }
                Expr::Use {
                    namespace,
                    data: span,
                } => Expr::Use {
                    data: ExprData {
                        ty: TypeShape::Unit.into(),
                        span,
                        contexts: namespace.data().contexts.clone(),
                    },
                    namespace,
                },
                Expr::Tuple { tuple, data: span } => Expr::Tuple {
                    data: ExprData {
                        contexts: Contexts::merge(
                            tuple.values().map(|value| &value.data().contexts),
                        )?,
                        ty: {
                            let ty = inference::Var::new_with_default(
                                &format!("tuple at {span}"),
                                TypeShape::Tuple(TupleType {
                                    name: inference::MaybeNotInferred::new_not_inferred("tuple"),
                                    fields: {
                                        let mut result = Tuple::empty();
                                        for (member, field) in tuple.as_ref() {
                                            result.add_member(member, field.data().ty.clone());
                                        }
                                        result
                                    },
                                }),
                            );
                            ty.add_check({
                                // TODO this is the todo you are looking for
                                let tuple = tuple.clone();
                                move |inferred| {
                                    match inferred {
                                        TypeShape::Type => {
                                            for (_name, field) in tuple.as_ref() {
                                                field.data().ty.infer_as(TypeShape::Type)?;
                                            }
                                        }
                                        TypeShape::Tuple(inferred) => {
                                            for (_name, (original, inferred)) in
                                                tuple.as_ref().zip(inferred.fields.as_ref())?
                                            {
                                                original
                                                    .data()
                                                    .ty
                                                    .clone()
                                                    .make_same(inferred.clone())?;
                                            }
                                        }
                                        _ => {
                                            eyre::bail!("tuple inferred to be {inferred}???");
                                        }
                                    }
                                    Ok(inference::CheckResult::Completed)
                                }
                            })?;
                            ty.into()
                        },
                        span,
                    },
                    tuple,
                },
                Expr::Ast {
                    expr_root,
                    definition,
                    values,
                    hygiene,
                    def_site,
                    data: span,
                } => {
                    for value in values.values() {
                        // TODO clone???
                        value.data().ty.infer_as(TypeShape::Ast)?;
                    }
                    Expr::Ast {
                        expr_root,
                        data: ExprData {
                            ty: TypeShape::Ast.into(),
                            span,
                            contexts: Contexts::merge(
                                values.values().map(|value| &value.data().contexts),
                            )?,
                        },
                        definition,
                        values,
                        hygiene,
                        def_site,
                    }
                }
                Expr::Recursive {
                    mut body,
                    compiler_scope,
                    data: span,
                } => {
                    body.data_mut().ty.infer_as(TypeShape::Unit)?;
                    let mut fields = Tuple::empty();
                    body.collect_bindings(
                        &mut |binding| {
                            fields.add_named(binding.symbol.name().to_owned(), binding.ty.clone());
                        },
                        None,
                    );
                    // tracing::info!("rec fields = {fields}");
                    Expr::Recursive {
                        data: ExprData {
                            ty: TypeShape::Tuple(TupleType {
                                name: inference::MaybeNotInferred::new_not_inferred("recursive"),
                                fields,
                            })
                            .into(),
                            span,
                            contexts: body.data().contexts.clone(),
                        },
                        compiler_scope,
                        body,
                    }
                }
                Expr::Function {
                    ty,
                    compiled,
                    data: span,
                } => Expr::Function {
                    data: ExprData {
                        ty: TypeShape::Function(Box::new(ty.clone())).into(),
                        span,
                        contexts: Contexts::empty_growable(),
                    },
                    ty,
                    compiled,
                },
                Expr::Template {
                    compiled,
                    data: span,
                } => Expr::Template {
                    data: ExprData {
                        ty: TypeShape::Template(compiled.clone()).into(),
                        span,
                        contexts: Contexts::empty_growable(),
                    },
                    compiled,
                },
                Expr::Scope { expr, data: span } => Expr::Scope {
                    data: ExprData {
                        ty: expr.data().ty.clone(),
                        span,
                        contexts: expr.data().contexts.clone(),
                    },
                    expr,
                },
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data: span,
                } => Expr::If {
                    data: ExprData {
                        ty: {
                            let ty = match &else_case {
                                Some(else_case) => else_case.data().ty.clone(),
                                None => TypeShape::Unit.into(),
                            };
                            then_case.data().ty.clone().make_same(ty.clone())?;
                            ty
                        },
                        span,
                        contexts: Contexts::merge(
                            [&condition.data().contexts, &then_case.data().contexts]
                                .into_iter()
                                .chain(
                                    else_case
                                        .as_ref()
                                        .map(|else_case| &else_case.data().contexts),
                                ),
                        )?,
                    },
                    condition,
                    then_case,
                    else_case,
                },
                Expr::Then {
                    mut list,
                    data: span,
                } => {
                    let mut last = None;
                    for expr in &mut list {
                        if let Some(prev) = last.replace(expr) {
                            prev.data_mut().ty.infer_as(TypeShape::Unit)?;
                        }
                    }
                    let result_ty =
                        last.map_or_else(|| TypeShape::Unit.into(), |prev| prev.data().ty.clone());
                    Expr::Then {
                        data: ExprData {
                            ty: result_ty,
                            span,
                            contexts: {
                                let mut contexts = Contexts::empty_growable();
                                for expr in list.iter().rev() {
                                    if let Expr::InjectContext { context, data: _ } = expr {
                                        contexts = Contexts::remove_injected(
                                            &contexts,
                                            &context.data().ty,
                                        )?;
                                    }
                                    contexts = Contexts::merge([&contexts, &expr.data().contexts])?;
                                }
                                contexts
                            },
                        },
                        list,
                    }
                }
                Expr::Constant { value, data: span } => Expr::Constant {
                    data: ExprData {
                        ty: value.ty(),
                        span,
                        contexts: Contexts::empty_growable(),
                    },
                    value,
                },
                Expr::Number { raw, data: span } => Expr::Number {
                    raw: raw.clone(),
                    data: ExprData {
                        ty: {
                            let default_number_type_context_type = kast
                                .cache
                                .interpreter
                                .natives
                                .get_named(
                                    kast.current_name.clone(), // TODO
                                    "default_number_type",
                                    TypeShape::Type.into(),
                                )?
                                .unwrap()
                                .into_type()?;
                            let default_number_type_context = kast
                                .interpreter
                                .contexts
                                .lock()
                                .unwrap()
                                .get_runtime(default_number_type_context_type)?
                                .ok_or_else(|| {
                                    eyre!("default number type context not available")
                                })?;
                            let f = default_number_type_context
                                .into_inferred()
                                .unwrap()
                                .as_tuple()
                                .unwrap()
                                .clone()
                                .into_values()
                                .get_named("default_number_type")
                                .unwrap()
                                .clone();
                            let ty = kast.call(f, ValueShape::String(raw).into()).await?;
                            let ty = ty.into_type()?;
                            match ty.inferred() {
                                Ok(inferred) => {
                                    Type::new_not_inferred_with_default("number literal", inferred)
                                }
                                Err(_) => ty,
                            }
                        },
                        contexts: Contexts::empty_growable(), // TODO if custom type
                        span,
                    },
                },
                Expr::String { token, data: span } => Expr::String {
                    data: ExprData {
                        ty: {
                            let token_typ = token.typ;
                            let ty = Type::new_not_inferred_with_default(
                                "string token",
                                match token_typ {
                                    kast_ast::StringType::SingleQuoted => TypeShape::Char,
                                    kast_ast::StringType::DoubleQuoted => TypeShape::String,
                                },
                            );
                            #[derive(Copy, Clone)]
                            struct Checker {
                                token_typ: kast_ast::StringType,
                            }
                            impl Checker {
                                fn check(
                                    self,
                                    ty: &TypeShape,
                                ) -> eyre::Result<inference::CheckResult>
                                {
                                    let token_typ = self.token_typ;
                                    match ty {
                                        TypeShape::Char
                                            if token_typ == kast_ast::StringType::SingleQuoted => {}
                                        TypeShape::String
                                            if token_typ == kast_ast::StringType::DoubleQuoted => {}
                                        TypeShape::Ref(inner) => {
                                            inner.var().add_check(move |ty| self.check(ty))?;
                                        }
                                        _ => {
                                            eyre::bail!(
                                                "{token_typ:?} string literal inferred as {ty}"
                                            )
                                        }
                                    }
                                    Ok(inference::CheckResult::Completed)
                                }
                            }
                            ty.var()
                                .add_check(move |ty| Checker { token_typ }.check(ty))?;
                            ty
                        },
                        contexts: Contexts::empty_growable(), // TODO if custom type
                        span,
                    },
                    token,
                },
                Expr::Native {
                    mut name,
                    data: span,
                } => {
                    name.data_mut().ty.infer_as(TypeShape::String)?;
                    Expr::Native {
                        name,
                        data: ExprData {
                            ty: Type::new_not_inferred(&format!("native at {span}")),
                            contexts: Contexts::empty_growable(),
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
                    if let Some(value) = &mut value {
                        pattern
                            .data_mut()
                            .ty
                            .make_same(value.data_mut().ty.clone())?;
                    }
                    Expr::Let {
                        data: ExprData {
                            ty: TypeShape::Unit.into(),
                            span,
                            contexts: match &value {
                                Some(value) => value.data().contexts.clone(),
                                None => Contexts::empty_growable(),
                            },
                        },
                        is_const_let,
                        pattern,
                        value,
                    }
                }
                Expr::CallMacro {
                    r#macro,
                    arg,
                    data: span,
                } => Expr::CallMacro {
                    data: ExprData {
                        ty: TypeShape::Ast.into(),
                        span,
                        contexts: Contexts::merge_two(
                            &r#macro.data().contexts,
                            &arg.data().contexts,
                        )?,
                    },
                    r#macro,
                    arg,
                },
                Expr::Call { f, arg, data: span } => {
                    let mut f = f.auto_instantiate(kast).await?;
                    let result_ty = Type::new_not_inferred(&format!("call at {span}"));
                    let contexts = Contexts::new_not_inferred();
                    f.data_mut()
                        .ty
                        .infer_as(TypeShape::Function(Box::new(FnType {
                            arg: arg.data().ty.clone(),
                            contexts: contexts.clone(),
                            result: result_ty.clone(),
                        })))?;
                    // TODO remove this in future builds
                    // ^ this comment is fake
                    // TODO check contexts
                    // contexts.var().add_check({
                    //     let current_contexts = kast.interpreter.contexts.clone();
                    //     move |contexts| contexts.check_available(&current_contexts.lock().unwrap())
                    // });
                    Expr::Call {
                        data: ExprData {
                            ty: result_ty,
                            span,
                            contexts: Contexts::merge([
                                &f.data().contexts,
                                &arg.data().contexts,
                                &contexts,
                            ])?,
                        },
                        f: Box::new(f),
                        arg,
                    }
                }
                Expr::Instantiate {
                    template: template_ir,
                    arg: arg_ir,
                    data: span,
                } => {
                    let template_ty = template_ir
                        .data()
                        .ty
                        .inferred()
                        .expect("template must be inferred")
                        .expect_template()?;
                    // TODO why am I cloning kast?
                    // TODO why eval, if then arg should be value??

                    let result_ty = Type::new_not_inferred("template instantiation result");
                    kast.cache.executor.spawn({
                        let mut result_ty = result_ty.clone();
                        let mut kast = kast.spawn_clone();
                        let arg_ir = arg_ir.clone();
                        async move {
                            let compiled = kast
                                .await_compiled(&template_ty)
                                .await
                                .context("template instantiation")?;

                            arg_ir
                                .data()
                                .ty
                                .clone()
                                .make_same(compiled.arg.data().ty.clone())?;

                            let arg = kast.eval(&arg_ir).await?;

                            let mut template_kast = kast.with_scopes(Scopes::new(
                                kast.spawn_id,
                                ScopeType::NonRecursive,
                                None,
                            ));
                            template_kast.pattern_match(
                                &compiled.arg,
                                OwnedPlace::new_temp(arg).get_ref(),
                            )?;
                            result_ty.make_same(
                                compiled
                                    .body
                                    .data()
                                    .ty
                                    .clone()
                                    .substitute_bindings(&template_kast, &mut RecurseCache::new()),
                            )?;
                            Ok(())
                        }
                    });

                    Expr::Instantiate {
                        data: ExprData {
                            ty: result_ty,
                            span,
                            contexts: Contexts::merge([
                                &template_ir.data().contexts,
                                &arg_ir.data().contexts,
                            ])?,
                        },
                        template: template_ir,
                        arg: arg_ir,
                    }
                }
            })
        };
        r#impl.boxed()
    }
}

impl Expr {
    async fn auto_instantiate(self, kast: &Kast) -> eyre::Result<Self> {
        let mut result = self;
        loop {
            let data = result.data();
            let is_template = data
                .ty
                .inferred()
                .is_ok_and(|ty| matches!(ty, TypeShape::Template { .. }));
            if !is_template {
                break;
            }
            result = Expr::Instantiate {
                arg: Box::new(
                    Expr::Constant {
                        value: Value::new_not_inferred(&format!(
                            "auto instantiate arg in {}",
                            data.span,
                        )),
                        data: data.span.clone(),
                    }
                    .init(kast)
                    .await?,
                ),
                data: data.span.clone(),
                template: Box::new(result),
            }
            .init(kast)
            .await?;
        }
        Ok(result)
    }
}

impl Pattern<Span> {
    pub fn init(self) -> eyre::Result<Pattern> {
        Ok(match self {
            Pattern::Variant {
                name,
                value,
                data: span,
            } => {
                let value_ty = value.as_ref().map(|value| value.data().ty.clone());
                Pattern::Variant {
                    name: name.clone(),
                    value,
                    data: PatternData {
                        ty: infer_type_variant(&format!("variant at {span}"), name, value_ty),
                        span,
                    },
                }
            }
            Pattern::Placeholder { data: span } => Pattern::Placeholder {
                data: PatternData {
                    ty: Type::new_not_inferred(&format!("_ at {span}")),
                    span,
                },
            },
            Pattern::Unit { data: span } => Pattern::Unit {
                data: PatternData {
                    ty: TypeShape::Unit.into(),
                    span,
                },
            },
            Pattern::Binding {
                binding,
                bind_mode,
                data: span,
            } => Pattern::Binding {
                data: PatternData {
                    ty: match bind_mode {
                        PatternBindMode::Claim => binding.ty.clone(),
                        PatternBindMode::Ref => {
                            let ty = Type::new_not_inferred("ref match");
                            binding.ty.infer_as(TypeShape::Ref(ty.clone()))?;
                            ty
                        }
                    },
                    span,
                },
                bind_mode,
                binding,
            },
            Pattern::Tuple { tuple, data: span } => Pattern::Tuple {
                data: PatternData {
                    ty: TypeShape::Tuple(TupleType {
                        name: inference::MaybeNotInferred::new_not_inferred("tuple"),
                        fields: tuple.as_ref().map(|field| field.data().ty.clone()),
                    })
                    .into(),
                    span,
                },
                tuple,
            },
        })
    }
}
