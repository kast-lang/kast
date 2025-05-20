use super::*;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
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
#[derive(Clone, Hash, PartialEq, Eq)]
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
    builtin_macros: HashMap<&'static str, BuiltinMacro>,
    syntax_definitions: Mutex<HashMap<Parc<ast::SyntaxDefinition>, std::task::Poll<Value>>>,
    pub casts: Mutex<CastMap>,
}

impl Cache {
    pub fn new() -> Self {
        let mut builtin_macros = HashMap::new();
        macro_rules! populate {
            ($($name:ident),*$(,)?) => {
                $(
                    let name = stringify!($name).strip_prefix("macro_").unwrap();
                    let closure: BuiltinMacro = |kast: &mut Kast, cty, ast| Kast::$name(kast, cty, ast).boxed();
                    builtin_macros.insert(name, closure);
                )*
            }
        }
        populate!(
            macro_native,
            macro_type_ascribe,
            macro_context_ascribe,
            macro_const_let,
            macro_let_assign,
            macro_let,
            macro_call,
            macro_then,
            macro_if,
            macro_match,
            macro_variant,
            macro_newtype,
            macro_merge,
            macro_scope,
            macro_macro,
            macro_function_def,
            macro_struct_def,
            macro_tuple,
            macro_field,
            macro_quote,
            macro_field_access,
            macro_function_type,
            macro_make_unit,
            macro_use,
            macro_syntax_module,
            macro_impl_syntax,
            macro_import,
            macro_include,
            macro_template_def,
            macro_instantiate_template,
            macro_placeholder,
            macro_is,
            macro_cast,
            macro_impl_cast,
            macro_with_context,
            macro_current_context,
            macro_comptime,
            macro_compile_ast,
            macro_call_macro,
            macro_unwindable,
            macro_unwind,
            macro_list,
            macro_mutable_pattern,
            macro_assign,
            macro_and,
            macro_or,
            macro_ref,
            macro_deref,
            macro_ref_pattern,
            macro_typeof,
        );
        Self {
            builtin_macros,
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
                    .builtin_macros
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
        r#impl: BuiltinMacro,
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
        f: BuiltinMacro,
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
                let arg = ValueShape::Tuple(
                    values
                        .as_ref()
                        .map(|ast| ValueShape::Ast(kast.set_def_site(ast)).into())
                        .into(),
                )
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
                        for (name, field) in args.as_ref() {
                            tuple.add(name, kast.compile(field).await?);
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
                    Symbol::new(&def.name),
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
        symbol: Symbol::new(name),
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

macro_rules! assert_expr {
    ($this:expr, $cty:expr, $ast:expr) => {
        let cty = $cty;
        let ast = $ast;
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                $this.compile(ast).await?,
            )));
        }
        if cty != CompiledType::Expr {
            eyre::bail!("Expected expr");
        }
    };
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
    async fn macro_type_ascribe(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let [value, ty] = values
            .as_ref()
            .into_named(["value", "type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ty = self
            .eval_ast::<Value>(ty, Some(TypeShape::Type.into()))
            .await
            .wrap_err_with(|| "Failed to evaluate the type")?
            .into_type()?;
        let mut value = self.compile_into(cty, value).await?;
        value.ty_mut().make_same(ty)?;
        Ok(value)
    }
    async fn macro_context_ascribe(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let [expr, contexts] = values
            .as_ref()
            .into_named(["expr", "contexts"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let contexts = self
            .eval_ast::<Value>(contexts, None) // TODO contexts
            .await
            .wrap_err_with(|| "Failed to evaluate the contexts")?
            .into_inferred()?
            .into_contexts()?;
        let mut expr = self.compile_into(cty, expr).await?;
        let data: &mut ExprData = match &mut expr {
            Compiled::Expr(e) => e.data_mut(),
            Compiled::PlaceExpr(e) => e.data_mut(),
            Compiled::AssigneeExpr(_) => todo!(),
            Compiled::Pattern(_) => todo!(),
        };
        data.contexts.0.make_same(contexts.0)?;
        Ok(expr)
    }
    async fn macro_const_let(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [pattern, value_ast] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = self.compile(pattern).await?;
        let value = self
            .eval_ast::<Value>(value_ast, Some(pattern.data().ty.clone()))
            .await?;

        // TODO don't clone value
        let matches = pattern
            .r#match(OwnedPlace::new_temp(value.clone()).get_ref(), self)?
            .ok_or_else(|| eyre!("pattern match was not exhaustive???"))?;
        for (binding, value) in matches {
            self.scopes.compiler.insert(binding.symbol.name(), value);
        }

        let value = Box::new(PlaceExpr::new_temp(
            Expr::Constant {
                value,
                data: value_ast.data().span.clone(),
            }
            .init(self)
            .await?,
        ));
        Ok(Compiled::Expr(
            Expr::Let {
                is_const_let: true,
                pattern,
                value: Some(value),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_let(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                self.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let pattern = values
            .as_ref()
            .into_single_named("pattern")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = self.compile(pattern).await?;
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(
                AssigneeExpr::Let {
                    pattern,
                    data: span,
                }
                .init(self)
                .await?,
            ),
            CompiledType::Expr => {
                self.inject_bindings(&pattern);
                Compiled::Expr(
                    Expr::Let {
                        is_const_let: false,
                        pattern,
                        value: None,
                        data: span,
                    }
                    .init(self)
                    .await?,
                )
            }
            _ => eyre::bail!("must be assignee"),
        })
    }
    async fn macro_let_assign(&mut self, ty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, ty, ast);
        let (values, span) = get_complex(ast);
        let [pattern, value] = values
            .as_ref()
            .into_named(["pattern", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let pattern: Pattern = self.compile(pattern).await?;
        let value: PlaceExpr = self.compile(value).await?;
        self.inject_bindings(&pattern);
        Ok(Compiled::Expr(
            Expr::Let {
                is_const_let: false,
                pattern,
                value: Some(Box::new(value)),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_native(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let name = values
            .as_ref()
            .into_single_named("name")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let name = self.compile(name).await?;
        Ok(Compiled::Expr(
            Expr::Native {
                name: Box::new(name),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_newtype(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let def = values
            .as_ref()
            .into_single_named("def")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Newtype {
                def: Box::new(self.compile(def).await?),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_merge(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (macro_name, span) = match ast {
            Ast::Complex {
                definition,
                data: AstData { span, .. },
                ..
            } => (definition.name.as_str(), span.clone()),
            _ => unreachable!(),
        };
        let ListCollected {
            list: value_asts,
            all_binary: _,
        } = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut values = Vec::new();
        for value_ast in value_asts {
            values.push(self.compile(value_ast).await?);
        }
        Ok(match cty {
            CompiledType::Expr => {
                Compiled::Expr(Expr::MakeMultiset { values, data: span }.init(self).await?)
            }
            CompiledType::Pattern => todo!(),
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::AssigneeExpr => eyre::bail!("not assignee"),
        })
    }
    async fn macro_variant(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                self.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let ([name], [ty, value]) = values
            .as_ref()
            .into_named_opt(["name"], ["type", "value"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let name = name
            .as_ident()
            .ok_or_else(|| eyre!("variant name must be an identifier"))?;
        let ty = match ty {
            None => None,
            Some(ty) => Some(
                self.eval_ast::<Value>(ty, Some(TypeShape::Type.into()))
                    .await?
                    .into_type()?,
            ),
        };
        Ok(match cty {
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::AssigneeExpr => eyre::bail!("not assignee"),
            CompiledType::Expr => Compiled::Expr({
                let mut expr = Expr::Variant {
                    name: name.to_owned(),
                    value: match value {
                        Some(value) => Some(Box::new(self.compile(value).await?)),
                        None => None,
                    },
                    data: span,
                }
                .init(self)
                .await?;
                if let Some(ty) = ty {
                    expr.data_mut().ty.make_same(ty)?;
                }
                expr
            }),
            CompiledType::Pattern => Compiled::Pattern({
                let mut pattern = Pattern::Variant {
                    name: name.to_owned(),
                    value: match value {
                        Some(value) => Some(Box::new(self.compile(value).await?)),
                        None => None,
                    },
                    data: span,
                }
                .init()?;
                if let Some(ty) = ty {
                    pattern.data_mut().ty.make_same(ty)?;
                }
                pattern
            }),
        })
    }
    /// Sarah is kinda cool
    /// Kappa
    /// NoKappa
    async fn macro_match(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, branches] = values
            .as_ref()
            .into_named(["value", "branches"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ListCollected {
            list: branch_asts,
            all_binary: _,
        } = ListCollector {
            macro_name: "builtin macro merge",
            a: "a",
            b: "b",
        }
        .collect(branches)?;
        let mut branches = Vec::new();
        for branch in branch_asts {
            match branch {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == "builtin macro function_def" => {
                    let [arg, body] = values.as_ref().into_named(["arg", "body"])?;
                    let mut kast = self.enter_scope();
                    let pattern = kast.compile(arg).await?;
                    branches.push(MatchBranch {
                        body: {
                            let mut kast = kast.enter_scope();
                            kast.inject_bindings(&pattern);
                            kast.compile(body).await?
                        },
                        pattern,
                    });
                }
                _ => eyre::bail!("match branches wrong syntax"),
            }
        }
        Ok(Compiled::Expr(
            Expr::Match {
                value: Box::new(self.compile(value).await?),
                branches,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_if(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let ([cond, then_case], [else_case]) = values
            .as_ref()
            .into_named_opt(["cond", "then"], ["else"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut then_scope = self.enter_scope();
        let cond: Expr = then_scope.compile(cond).await?;
        Ok(Compiled::Expr(
            Expr::If {
                then_case: {
                    then_scope.inject_conditional_bindings(&cond, true);
                    Box::new(then_scope.compile(then_case).await?)
                },
                else_case: match else_case {
                    Some(else_case) => Some({
                        let mut else_scope = self.enter_scope();
                        else_scope.inject_conditional_bindings(&cond, false);
                        Box::new(else_scope.compile(else_case).await?)
                    }),
                    None => None,
                },
                condition: Box::new(cond),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_then(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let macro_name = match ast {
            Ast::Complex { definition, .. } => definition.name.as_str(),
            _ => unreachable!(),
        };
        assert_expr!(self, cty, ast);
        let ListCollected {
            list: ast_list,
            all_binary,
        } = ListCollector {
            macro_name,
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut expr_list = Vec::with_capacity(ast_list.len());
        for ast in ast_list {
            expr_list.push(self.compile(ast).await?);
        }
        let expr = Expr::Then {
            list: expr_list,
            data: ast.data().span.clone(),
        }
        .init(self)
        .await?;
        if !all_binary {
            expr.data().ty.infer_as(TypeShape::Unit)?;
        }
        Ok(Compiled::Expr(expr))
    }
    async fn macro_impl_syntax(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let [def, r#impl] = values
            .as_ref()
            .into_named(["def", "impl"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let def = self
            .eval_ast::<Value>(def, Some(TypeShape::SyntaxDefinition.into()))
            .await?
            .into_inferred()?
            .into_syntax_definition()?;
        tracing::trace!("defined syntax {:?}", def.name);
        let r#impl = self.eval_ast(r#impl, None).await?; // TODO should be a macro?
        self.cache
            .compiler
            .syntax_definitions
            .lock()
            .unwrap()
            .insert(def, std::task::Poll::Ready(r#impl)); // TODO check previous value?
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Unit.into(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_syntax_module(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = self.enter_recursive_scope();
        inner
            .eval_ast::<Value>(body, Some(TypeShape::Unit.into()))
            .await?
            .into_inferred()?
            .into_unit()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::SyntaxModule(Parc::new(inner.scope_syntax_definitions())).into(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_struct_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let body = values
            .as_ref()
            .into_single_named("body")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = self.enter_recursive_scope();
        let body = inner.compile(body).await?;
        Ok(Compiled::Expr(
            Expr::Recursive {
                body: Box::new(body),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_macro(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let def = values
            .as_ref()
            .into_single_named("def")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        // TODO expect some type here?
        let def = self.eval_ast::<Value>(def, None).await?;
        let def = def.into_inferred()?.into_function()?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Macro(def).into(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_template_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let ([arg, body], [r#where]) = values
            .as_ref()
            .into_named_opt(["arg", "body"], ["where"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let _ = r#where; // TODO

        let mut inner = self.enter_scope();
        let arg: Pattern = inner.compile(arg).await?;
        inner.inject_bindings(&arg);
        let compiled = inner.compile_fn_body(
            arg,
            body,
            Contexts::empty(),
            Type::new_not_inferred("template result"),
        );
        Ok(Compiled::Expr(
            Expr::Template {
                compiled,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_instantiate_template(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let [template, arg] = values
            .as_ref()
            .into_named(["template", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let template = self.compile(template).await?;
        let arg = self.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Instantiate {
                template: Box::new(template),
                arg: Box::new(arg),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    fn compile_fn_body(
        &mut self,
        arg: Pattern,
        body: &Ast,
        contexts: Contexts,
        result_type: Type,
    ) -> MaybeCompiledFn {
        let compiled = Parc::new(Mutex::new(None));
        self.cache.executor.spawn({
            let mut kast = self.spawn_clone();
            let compiled = compiled.clone();
            let body: Ast = body.clone();
            let result_type = result_type.clone();
            async move {
                let mut body: Expr = kast.compile(&body).await?;
                body.data_mut().ty.make_same(result_type)?;
                body.data_mut().contexts.0.make_same(contexts.0)?;
                let old_compiled = compiled
                    .lock()
                    .unwrap()
                    .replace(Parc::new(CompiledFn { body, arg }));
                assert!(old_compiled.is_none(), "function compiled twice wtf?");
                Ok(())
            }
            .map_err(|err: eyre::Report| err.wrap_err("Failed to compile fn"))
        });
        compiled
    }
    async fn macro_function_def(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let ([body], [arg, contexts, result_type]) = values
            .as_ref()
            .into_named_opt(["body"], ["arg", "contexts", "result_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut inner = self.enter_scope();
        let arg: Pattern = match arg {
            Some(arg) => inner.compile(arg).await?,
            None => Pattern::Unit { data: span.clone() }.init()?,
        };
        inner.inject_bindings(&arg);
        let arg_ty = arg.data().ty.clone();
        let result_type = match result_type {
            Some(ast) => inner
                .eval_ast::<Value>(ast, Some(TypeShape::Type.into()))
                .await?
                .into_type()?,
            None => Type::new_not_inferred(&format!("result type of fn at {span}")),
        };
        let contexts = match contexts {
            Some(contexts) => self
                .eval_ast::<Value>(
                    contexts, None, // TODO Contexts??
                )
                .await?
                .into_inferred()?
                .into_contexts()?,
            None => Contexts::new_not_inferred(),
        };
        // todon't: Contexts!!
        let compiled = inner.compile_fn_body(arg, body, contexts.clone(), result_type.clone());
        Ok(Compiled::Expr(
            Expr::Function {
                ty: FnType {
                    arg: arg_ty,
                    contexts,
                    result: result_type,
                },
                compiled,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_scope(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [expr] = values
            .as_ref()
            .into_unnamed()
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        tracing::trace!("compiling scoped: {expr}");
        // TODO probably need to enter scope in all cases?
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(self.compile(expr).await?),
            CompiledType::PlaceExpr => Compiled::PlaceExpr(self.compile(expr).await?),
            CompiledType::Expr => {
                let expr = self.enter_scope().compile(expr).await?;
                Compiled::Expr(
                    Expr::Scope {
                        expr: Box::new(expr),
                        data: span,
                    }
                    .init(self)
                    .await?,
                )
            }
            CompiledType::Pattern => Compiled::Pattern(self.compile(expr).await?),
        })
    }
    async fn macro_import(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let path = self
            .eval_ast::<Value>(
                values.as_ref().into_single_named("path")?,
                Some(TypeShape::String.into()),
            )
            .await?
            .into_inferred()?
            .as_str()?
            .to_owned();
        let path = if path.starts_with('.') {
            ast.data()
                .span
                .filename
                .parent()
                .expect("no parent dir??")
                .join(path)
        } else {
            todo!("absolute import")
        };
        let value: Value = self.import(path)?;
        Ok(Compiled::Expr(
            Expr::Constant { value, data: span }.init(self).await?,
        ))
    }
    async fn macro_include(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let path = self
            .eval_ast::<Value>(
                values.as_ref().into_single_named("path")?,
                Some(TypeShape::String.into()),
            )
            .await?
            .into_inferred()?
            .as_str()?
            .to_owned();
        let path = if path.starts_with('.') {
            ast.data()
                .span
                .filename
                .parent()
                .expect("no parent dir??")
                .join(path)
        } else {
            todo!("absolute include")
        };
        Ok(match self.include(path)? {
            Some(ast) => self.compile_into(cty, &ast).await?,
            None => Compiled::Expr(Expr::Unit { data: span }.init(self).await?),
        })
    }
    async fn macro_use(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let namespace = values.as_ref().into_single_named("namespace")?;
        let namespace: Value = self.eval_ast(namespace, None).await?;
        match namespace.clone().inferred() {
            Some(ValueShape::Tuple(namespace)) => {
                for (name, value) in namespace.into_values().into_iter() {
                    let name = name.ok_or_else(|| eyre!("cant use unnamed fields"))?;
                    self.add_local(Symbol::new(name), value);
                }
            }
            _ => eyre::bail!("{namespace} is not a namespace"),
        }
        Ok(Compiled::Expr(
            Expr::Use {
                namespace: Box::new(
                    Expr::Constant {
                        value: namespace,
                        data: span.clone(),
                    }
                    .init(self)
                    .await?,
                ),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_make_unit(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        let [] = values.as_ref().into_named([])?;
        Ok(match cty {
            CompiledType::AssigneeExpr => {
                Compiled::AssigneeExpr(AssigneeExpr::Unit { data: span }.init(self).await?)
            }
            CompiledType::PlaceExpr => Compiled::PlaceExpr(PlaceExpr::new_temp(
                Expr::Unit { data: span }.init(self).await?,
            )),
            CompiledType::Pattern => Compiled::Pattern(Pattern::Unit { data: span }.init()?),
            CompiledType::Expr => Compiled::Expr(Expr::Unit { data: span }.init(self).await?),
        })
    }
    async fn macro_call(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let [f, arg] = values
            .as_ref()
            .into_named(["f", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let f = self.compile(f).await?;
        let arg = self.compile(arg).await?;
        Ok(Compiled::Expr(
            Expr::Call {
                f: Box::new(f),
                arg: Box::new(arg),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_call_macro(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let [r#macro, arg] = values
            .as_ref()
            .into_named(["macro", "arg"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let r#macro = self.compile(r#macro).await?;
        let arg = self.compile(arg).await?;
        // println!("evaled = {:?}", self.eval(&arg).await.unwrap());
        Ok(Compiled::Expr(
            Expr::CallMacro {
                r#macro: Box::new(r#macro),
                arg: Box::new(arg),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_unwindable(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [name, body] = values.as_ref().into_named(["name", "body"])?;
        let (name, body) = {
            let mut kast = self.enter_scope();
            let name: Pattern = kast.compile(name).await?;
            kast.inject_bindings(&name);
            let body = kast.compile(body).await?;
            (name, body)
        };
        Ok(Compiled::Expr(
            Expr::Unwindable {
                name,
                body: Box::new(body),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_unwind(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [name, value] = values.as_ref().into_named(["name", "value"])?;
        let name: Expr = self.compile(name).await?;
        let value: Expr = self.compile(value).await?;
        Ok(Compiled::Expr(
            Expr::Unwind {
                name: Box::new(name),
                value: Box::new(value),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    /// this function might succeed (no promises)
    async fn macro_function_type(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let ([arg, result], [contexts]) = values
            .as_ref()
            .into_named_opt(["arg", "result"], ["contexts"])?;
        Ok(Compiled::Expr(
            Expr::FunctionType {
                arg: Box::new(self.compile(arg).await?),
                contexts: match contexts {
                    Some(contexts) => Some(Box::new(self.compile(contexts).await?)),
                    None => None,
                },
                result: Box::new(self.compile(result).await?),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_field_access(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::Expr {
            return Ok(Compiled::Expr(self.compile::<PlaceExpr>(ast).await?.into()));
        }
        assert_eq!(cty, CompiledType::PlaceExpr);
        let (values, span) = get_complex(ast);
        let [obj, field] = values.as_ref().into_named(["obj", "field"])?;
        let field = ast_as_member(field)
            .ok_or_else(|| eyre!("expected a member (ident or index), got {field}"))?;
        // My hair is very crusty today
        Ok(Compiled::PlaceExpr(
            PlaceExpr::FieldAccess {
                obj: Box::new(self.compile(obj).await?),
                field: field.into_owned(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_quote(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let expr = values.as_ref().into_single_named("expr")?;
        fn quote<'a>(
            kast: &'a mut Kast,
            expr_root: bool,
            ast: &'a Ast,
        ) -> BoxFuture<'a, eyre::Result<PlaceExpr>> {
            async move {
                Ok(match ast {
                    Ast::Complex {
                        definition,
                        values,
                        data:
                            AstData {
                                span,
                                hygiene,
                                def_site,
                            },
                    } => {
                        if definition.name == "builtin macro unquote" {
                            let expr = values
                                .as_ref()
                                .into_single_named("expr")
                                .wrap_err_with(|| "wrong args to unquote")?;
                            kast.compile(expr).await?
                        } else {
                            PlaceExpr::new_temp(
                                Expr::Ast {
                                    expr_root,
                                    definition: definition.clone(),
                                    values: {
                                        let mut result = Tuple::empty();
                                        for (name, value) in values.as_ref().into_iter() {
                                            let value = quote(kast, false, value).boxed().await?;
                                            result.add(name, value);
                                        }
                                        result
                                    },
                                    hygiene: *hygiene,
                                    def_site: def_site.clone(),
                                    data: span.clone(),
                                }
                                .init(kast)
                                .await?,
                            )
                        }
                    }
                    _ => PlaceExpr::new_temp(
                        Expr::Constant {
                            value: ValueShape::Ast(match expr_root {
                                true => kast.set_def_site(ast),
                                false => ast.clone(),
                            })
                            .into(),
                            data: ast.data().span.clone(),
                        }
                        .init(kast)
                        .await?,
                    ),
                })
            }
            .boxed()
        }
        Ok(Compiled::Expr(
            // TODO `($x) moves x
            Expr::ReadPlace {
                place: quote(self, true, expr).await?,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_field(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        self.macro_tuple(cty, ast).await
    }
    async fn macro_tuple(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                self.compile(ast).await?,
            )));
        }
        let ListCollected {
            list: fields,
            all_binary: _,
        } = ListCollector {
            macro_name: "builtin macro tuple",
            a: "a",
            b: "b",
        }
        .collect(ast)?;
        let mut tuple = Tuple::empty();
        for field in fields {
            match field {
                Ast::Complex {
                    definition,
                    values,
                    data: _,
                } if definition.name == "builtin macro field" => {
                    let ([name], [value]) = values
                        .as_ref()
                        .into_named_opt(["name"], ["value"])
                        .wrap_err_with(|| "field macro wrong args")?;
                    let (name, ty): (&Ast, Option<Type>) = match name {
                        Ast::Complex {
                            definition,
                            values,
                            data: _,
                        } if definition.name == "builtin macro type_ascribe" => {
                            let [name, ty] = values
                                .as_ref()
                                .into_named(["value", "type"])
                                .wrap_err_with(|| "type ascribe macro wrong args")?;
                            (
                                name,
                                Some(
                                    self.eval_ast::<Value>(ty, Some(TypeShape::Type.into()))
                                        .await?
                                        .into_type()?,
                                ),
                            )
                        }
                        _ => (name, None),
                    };
                    let value = value.unwrap_or(name);
                    let name_span = name.data().span.clone();
                    let name = name
                        .as_ident()
                        .ok_or_else(|| eyre!("{name} is not an ident"))?
                        .to_owned();
                    let mut compiled = self.compile_into(cty, value).await?;
                    let ty = match ty {
                        Some(ty) => ty,
                        None => Type::new_not_inferred(&format!("Field {name} at {name_span}")),
                    };
                    compiled.ty_mut().make_same(ty)?;
                    tuple.add_named(name, compiled);
                }
                _ => tuple.add_unnamed(self.compile_into(cty, field).await?),
            }
        }
        Ok(match cty {
            CompiledType::AssigneeExpr => Compiled::AssigneeExpr(
                AssigneeExpr::Tuple {
                    tuple: tuple.map(|field| field.expect_assignee_expr().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init(self)
                .await?,
            ),
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::Expr => Compiled::Expr(
                Expr::Tuple {
                    tuple: tuple.map(|field| field.expect_expr().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init(self)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(
                Pattern::Tuple {
                    tuple: tuple.map(|field| field.expect_pattern().unwrap()),
                    data: ast.data().span.clone(),
                }
                .init()?,
            ),
        })
    }
    async fn macro_is(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, span) = get_complex(ast);
        assert_expr!(self, cty, ast);
        let [value, pattern] = values
            .as_ref()
            .into_named(["value", "pattern"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Is {
                value: Box::new(self.compile(value).await?),
                pattern: self.compile(pattern).await?,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_cast(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, target] = values
            .as_ref()
            .into_named(["value", "target"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(Compiled::Expr(
            Expr::Cast {
                value: Box::new(self.compile(value).await?),
                target: {
                    tracing::trace!("cast target: {target}");
                    let target = self.eval_ast(target, None).await?;
                    tracing::trace!(" = {target}");
                    target
                },
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_impl_cast(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [value, target, r#impl] = values
            .as_ref()
            .into_named(["value", "target", "impl"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let value = self.eval_ast::<Value>(value, None).await?;
        let target = self.eval_ast::<Value>(target, None).await?;
        let mut r#impl: Expr = self.compile(r#impl).await?;
        let impl_ty = self
            .cast_result_ty(|| future::ready(Ok(value.clone())).boxed(), target.clone())
            .await?;
        // right now I have a small brain moment
        r#impl.data_mut().ty.make_same(impl_ty)?;
        let r#impl = self.eval(&r#impl).await?;
        self.cache
            .compiler
            .casts
            .lock()
            .unwrap()
            .impl_cast(value, target, r#impl)?;
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Unit.into(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_placeholder(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::PlaceExpr {
            return Ok(Compiled::PlaceExpr(PlaceExpr::new_temp(
                self.compile(ast).await?,
            )));
        }
        let (values, span) = get_complex(ast);
        let [] = values
            .as_ref()
            .into_named([])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        Ok(match cty {
            CompiledType::AssigneeExpr => {
                Compiled::AssigneeExpr(AssigneeExpr::Placeholder { data: span }.init(self).await?)
            }
            CompiledType::PlaceExpr => eyre::bail!("not a place expr"),
            CompiledType::Expr => Compiled::Expr(
                Expr::Constant {
                    value: Value::new_not_inferred(&format!("placeholder at {span}")),
                    data: span,
                }
                .init(self)
                .await?,
            ),
            CompiledType::Pattern => Compiled::Pattern(Pattern::Placeholder { data: span }.init()?),
        })
    }
    async fn macro_with_context(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let ([new_context], [expr]) = values
            .as_ref()
            .into_named_opt(["new_context"], ["expr"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        if expr.is_some() {
            todo!();
        }
        // god pls forgive me the sin of creating kast
        let context: Expr = self.compile(new_context).await?;
        // no context
        // I wish I had done this in LUA
        Ok(Compiled::Expr(
            Expr::InjectContext {
                context: Box::new(context),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_current_context(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let ([], [context_type]) = values
            .as_ref()
            .into_named_opt([], ["context_type"])
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let mut expr = Expr::CurrentContext { data: span }.init(self).await?;
        if let Some(context_type) = context_type {
            let ty = self
                .eval_ast::<Value>(context_type, Some(TypeShape::Type.into()))
                .await?
                .into_type()?;
            expr.data_mut().ty.make_same(ty)?;
        }
        Ok(Compiled::Expr(expr))
    }
    async fn macro_comptime(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let value = values
            .as_ref()
            .into_single_named("value")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let value = self.eval_ast(value, None).await?;
        Ok(Compiled::Expr(
            Expr::Constant { value, data: span }.init(self).await?,
        ))
    }
    async fn macro_compile_ast(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let value = values
            .as_ref()
            .into_single_named("ast")
            .wrap_err_with(|| "Macro received incorrect arguments")?;
        let ast = self
            .eval_ast::<Value>(value, Some(TypeShape::Ast.into()))
            .await?
            .into_inferred()?
            .into_ast()?;
        self.compile_into(cty, &ast).await
    }
    async fn macro_mutable_pattern(
        &mut self,
        cty: CompiledType,
        ast: &Ast,
    ) -> eyre::Result<Compiled> {
        let (values, _span) = get_complex(ast);
        let pattern = values.as_ref().into_single_named("pattern")?;
        let mut kast = self.clone();
        kast.compiler.bindings_mutability = Mutability::Mutable;
        kast.compile_into(cty, pattern).await
    }
    async fn macro_or(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [lhs, rhs] = values.as_ref().into_named(["lhs", "rhs"])?;
        Ok(Compiled::Expr(
            Expr::Or {
                lhs: Box::new(self.compile(lhs).await?),
                rhs: Box::new(self.compile(rhs).await?),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_and(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [lhs, rhs] = values.as_ref().into_named(["lhs", "rhs"])?;
        Ok(Compiled::Expr(
            Expr::And {
                lhs: Box::new(self.compile(lhs).await?),
                rhs: Box::new(self.compile(rhs).await?),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_assign(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let [assignee, value] = values.as_ref().into_named(["assignee", "value"])?;
        let assignee: AssigneeExpr = self.compile(assignee).await?;
        let value: PlaceExpr = self.compile(value).await?;
        self.inject_assignee_bindings(&assignee);
        Ok(Compiled::Expr(
            Expr::Assign {
                assignee,
                value: Box::new(value),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_typeof(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let expr = values.as_ref().into_single_named("expr")?;
        let ty = self
            .enter_scope()
            .compile::<Expr>(expr)
            .await?
            .data()
            .ty
            .clone();
        Ok(Compiled::Expr(
            Expr::Constant {
                value: ValueShape::Type(ty).into(),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_ref_pattern(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_eq!(cty, CompiledType::Pattern);
        let (values, _span) = get_complex(ast);
        let ident = values.as_ref().into_single_named("ident")?;
        Ok(Compiled::Pattern(
            compile_ident_pattern(self, ident, PatternBindMode::Ref).await?,
        ))
    }
    async fn macro_ref(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let place = values.as_ref().into_single_named("place")?;
        Ok(Compiled::Expr(
            Expr::Ref {
                place: self.compile(place).await?,
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_deref(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        if cty == CompiledType::Expr {
            return Ok(Compiled::Expr(self.compile::<PlaceExpr>(ast).await?.into()));
        }
        if cty == CompiledType::AssigneeExpr {
            return Ok(Compiled::AssigneeExpr(
                self.compile::<PlaceExpr>(ast).await?.into(),
            ));
        }
        assert_eq!(cty, CompiledType::PlaceExpr);
        let (values, span) = get_complex(ast);
        let r#ref = values.as_ref().into_single_named("ref")?;
        Ok(Compiled::PlaceExpr(
            PlaceExpr::Deref {
                r#ref: Box::new(self.compile(r#ref).await?),
                data: span,
            }
            .init(self)
            .await?,
        ))
    }
    async fn macro_list(&mut self, cty: CompiledType, ast: &Ast) -> eyre::Result<Compiled> {
        assert_expr!(self, cty, ast);
        let (values, span) = get_complex(ast);
        let ([], [values]) = values.as_ref().into_named_opt([], ["values"])?;
        let values_asts = match values {
            Some(values) => {
                let ListCollected {
                    list: values_asts,
                    all_binary: _,
                } = ListCollector {
                    macro_name: "builtin macro tuple",
                    a: "a",
                    b: "b",
                }
                .collect(values)?;
                values_asts
            }
            None => vec![],
        };
        let mut values = Vec::new();
        for value_ast in values_asts {
            values.push(self.compile(value_ast).await?);
        }
        Ok(Compiled::Expr(
            Expr::List { values, data: span }.init(self).await?,
        ))
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

type BuiltinMacro = for<'a> fn(
    kast: &'a mut Kast,
    cty: CompiledType,
    ast: &'a Ast,
) -> BoxFuture<'a, eyre::Result<Compiled>>;

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
        let variants = ty.clone().expect_variant()?;
        match variants.iter().find(|variant| variant.name == name) {
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
                        ty: TypeShape::Tuple(tuple.as_ref().map(|field| field.data().ty.clone()))
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
                                        TypeShape::Tuple(fields) => {
                                            match fields.get(field.as_ref()) {
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
                                TypeShape::Tuple({
                                    let mut result = Tuple::empty();
                                    for (name, field) in tuple.as_ref() {
                                        result.add(name, field.data().ty.clone());
                                    }
                                    result
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
                                                tuple.as_ref().zip(inferred.as_ref())?
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
                            ty: TypeShape::Tuple(fields).into(),
                            span,
                            contexts: body.data().contexts.clone(),
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
                                .get("default_number_type", TypeShape::Type.into())?
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

                    let compiled = kast.await_compiled(&template_ty).await?;

                    arg_ir
                        .data()
                        .ty
                        .clone()
                        .make_same(compiled.arg.data().ty.clone())?;
                    let arg = kast.clone().eval(&arg_ir).await?;

                    let mut template_kast =
                        kast.with_scopes(Scopes::new(kast.spawn_id, ScopeType::NonRecursive, None));
                    template_kast
                        .pattern_match(&compiled.arg, OwnedPlace::new_temp(arg).get_ref())?;
                    let result_ty = compiled
                        .body
                        .data()
                        .ty
                        .clone()
                        .substitute_bindings(&template_kast, &mut RecurseCache::new());

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
                    ty: TypeShape::Tuple(tuple.as_ref().map(|field| field.data().ty.clone()))
                        .into(),
                    span,
                },
                tuple,
            },
        })
    }
}
