use super::*;
use eyre::OptionExt as _;
use linked_hash_map::LinkedHashMap;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum JavaScriptEngineType {
    Node,
    Browser,
}

mod ir {
    use super::*;

    pub struct Number(String);

    impl From<i32> for Number {
        fn from(value: i32) -> Self {
            Self(value.to_string())
        }
    }
    impl From<i64> for Number {
        fn from(value: i64) -> Self {
            Self(value.to_string())
        }
    }
    impl From<f64> for Number {
        fn from(value: f64) -> Self {
            Self(value.to_string())
        }
    }

    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Name(pub String);

    impl Name {
        pub fn context() -> Self {
            Self("ctx".into())
        }
        pub fn unique(s: &str) -> Self {
            Self(format!("{s}__{}", Id::new()))
        }
        pub fn for_binding(binding: &Binding) -> Self {
            Self(format!(
                "{}__{}",
                escape_ident(&binding.symbol.name),
                binding.symbol.id,
            ))
        }
    }

    #[must_use]
    pub enum Expr {
        Binding(Name),
        Undefined,
        Bool(bool),
        Number(Number),
        String(String),
        List(Vec<Expr>),
        Object { fields: LinkedHashMap<Name, Expr> },
        Fn { args: Vec<Name>, body: Vec<Stmt> },
        Index { obj: Box<Expr>, index: Box<Expr> },
        Not(Box<Expr>),
        Call { f: Box<Expr>, args: Vec<Expr> },
        Raw(String),
    }

    #[must_use]
    pub enum Stmt {
        Return(Expr),
        If {
            condition: Expr,
            then_case: Vec<Stmt>,
            else_case: Vec<Stmt>,
        },
        Throw(Expr),
        Assign {
            /// TODO special expr?
            assignee: Expr,
            value: Expr,
        },
        Expr(Expr),
        Try {
            body: Vec<Stmt>,
            catch_var: Name,
            catch_body: Vec<Stmt>,
        },
    }

    impl Expr {
        pub fn binding(binding: &Binding) -> Self {
            Self::Binding(Name::for_binding(binding))
        }
        pub fn new_variant(name: impl Into<String>, value: Option<Self>) -> Self {
            let mut fields = LinkedHashMap::new();
            fields.insert(Name("variant".into()), ir::Expr::String(name.into()));
            if let Some(value) = value {
                fields.insert(Name("value".into()), value.into());
            }
            Self::Object { fields }
        }
        pub fn variant_name(value: Self) -> Self {
            Self::Index {
                obj: value,
                index: Self::String("variant".into()),
            }
        }
        pub fn variant_value(value: Self) -> Self {
            Self::Index {
                obj: value,
                index: Self::String("value".into()),
            }
        }
        pub fn scope(stmts: Vec<Stmt>) -> Self {
            Self::Call {
                f: Box::new(Self::Fn {
                    args: vec![],
                    body: stmts,
                }),
                args: vec![],
            }
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub enum ShowOptions {
        Minified,
        Pretty, // TODO { max_line_length: usize },
    }

    impl Expr {
        pub fn show(&self, options: ShowOptions) -> impl std::fmt::Display + '_ {
            struct Show<'a> {
                expr: &'a Expr,
                options: ShowOptions,
            }
            impl std::fmt::Display for Show<'_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.expr {
                        Expr::Object { fields } => {
                            write!(f, "{{")?;
                            pad_adapter::padded_items(
                                f,
                                fields,
                                pad_adapter::OuterMode::Surrounded,
                                pad_adapter::Moded {
                                    normal: pad_adapter::Separator::Seq(","),
                                    alternate: pad_adapter::Separator::Seq(","),
                                },
                                |f, (name, value)| {
                                    write!(f, "{name:?}:")?;
                                    if f.alternate() {
                                        write!(f, " ")?;
                                    }
                                    f.write(value.show(self.options))?;
                                    Ok(())
                                },
                            )?;
                            write!(f, "}}")?;
                        }
                        _ => todo!(),
                    }
                    Ok(())
                }
            }
            Show {
                expr: self,
                options,
            }
        }
    }
}

struct Transpiler {
    kast: Kast,
    scopes: Parc<Scopes>,
    engine_type: JavaScriptEngineType,
    captured: std::collections::HashSet<PlaceRef>,
    captured_unprocessed: Vec<PlaceRef>,
    type_ids: HashMap<Hashable<Type>, Id>,
}

fn escape_ident(ident: &str) -> String {
    ident
        .chars()
        .map(|c| {
            if c.is_alphanumeric() {
                return c;
            }
            '_'
        })
        .fold(String::new(), |acc, c| {
            if acc.ends_with('_') && c == '_' {
                acc
            } else {
                let mut acc = acc;
                acc.push(c);
                acc
            }
        })
}

fn binding_name(binding: &Binding) -> String {
    format!(
        "{}__{}",
        escape_ident(&binding.symbol.name),
        binding.symbol.id,
    )
}

impl Transpiler {
    fn new(kast: &mut Kast, engine_type: JavaScriptEngineType) -> Self {
        Self {
            kast: kast.clone(),
            scopes: Parc::new(kast.scopes.clone()),
            engine_type,
            captured: Default::default(),
            captured_unprocessed: Default::default(),
            type_ids: HashMap::new(),
        }
    }

    // TODO maybe assignee/place expr?
    fn make_ref(&mut self, value: ir::Expr) -> eyre::Result<ir::Expr> {
        let new_value_var = ir::Name::unique("new_value");
        let mut fields = LinkedHashMap::new();
        fields.insert(
            ir::Name("get".into()),
            ir::Expr::Fn {
                args: vec![],
                body: vec![ir::Stmt::Return(Box::new(value.clone()))],
            },
        );
        fields.insert(
            ir::Name("set".into()),
            ir::Expr::Fn {
                args: vec![new_value_var.clone()],
                body: vec![ir::Stmt::Assign {
                    assignee: value,
                    value: ir::Expr::Binding(new_value_var),
                }],
            },
        );
        Ok(ir::Expr::Object { fields })
    }

    fn context_name(&mut self, context_ty: &Type) -> String {
        format!(
            "type{}",
            self.type_ids
                .entry(Hashable(context_ty.clone()))
                .or_insert_with(Id::new)
        )
    }

    async fn read_place(&mut self, place: &PlaceExpr) -> eyre::Result<ir::Expr> {
        let place = self.eval_place(place).await?;
        Ok(ir::Expr::Call {
            f: Box::new(ir::Expr::Index {
                obj: Box::new(place),
                index: Box::new(ir::Expr::String("get".into())),
            }),
            args: vec![],
        })
    }

    fn eval_place<'a>(&'a mut self, expr: &'a PlaceExpr) -> BoxFuture<'a, eyre::Result<ir::Expr>> {
        async move {
            Ok(match expr {
                PlaceExpr::Binding { binding, data: _ } => {
                    match self.scopes.interpreter.get(&binding.symbol) {
                        Some(place) => {
                            todo!()
                            // self.ensure_captured(&place);
                            // self.make_ref(&format!("captured{}", place.place.id))?;
                        }
                        None => self.make_ref(ir::Expr::binding(binding))?,
                    }
                }
                PlaceExpr::FieldAccess {
                    obj,
                    field,
                    data: _,
                } => {
                    let temp_var = ir::Name::unique("obj");
                    ir::Expr::scope(vec![
                        ir::Stmt::Assign {
                            assignee: ir::Expr::Binding(temp_var.clone()),
                            value: self.read_place(obj)?, // The value is a reference type since it has fields
                        },
                        ir::Stmt::Return(Box::new(self.make_ref(ir::Expr::Index {
                            obj: Box::new(ir::Expr::Binding(temp_var)),
                            index: Box::new(ir::Expr::String(field.to_string())),
                        }))),
                    ])
                }
                PlaceExpr::Temporary { value, data: _ } => {
                    let var = ir::Name::unique("temp");
                    ir::Expr::scope(vec![
                        ir::Stmt::Assign {
                            assignee: ir::Expr::Binding(var.clone()),
                            value: self.eval_expr(value).await?,
                        },
                        ir::Stmt::Return(self.make_ref(ir::Expr::Binding(var))?),
                    ])
                }
                PlaceExpr::Deref { r#ref, data: _ } => {
                    self.eval_expr(r#ref).await? // It's already a reference - which is what a place is
                }
            })
        }
        .boxed()
    }

    fn eval_expr<'a>(&'a mut self, expr: &'a Expr) -> BoxFuture<'a, eyre::Result<ir::Expr>> {
        let r#impl = async move {
            Ok(match expr {
                Expr::Type { .. } => ir::Expr::Undefined,
                Expr::Ref { place, data: _ } => self.eval_place(place).await?,
                Expr::And { lhs, rhs, data } => todo!(),
                Expr::Or { lhs, rhs, data } => todo!(),
                Expr::Assign {
                    assignee,
                    value,
                    data: _,
                } => {
                    let temp_var = ir::Name::unique("var");
                    ir::Expr::scope({
                        let mut stmts = Vec::new();
                        stmts.push(ir::Stmt::Assign {
                            assignee: ir::Expr::Binding(temp_var.clone()),
                            value: self.read_place(value).await?,
                        });
                        self.assign(assignee, ir::Expr::Binding(temp_var), &mut stmts)
                            .await?;
                        stmts
                    })
                }
                Expr::List { values, data: _ } => ir::Expr::List({
                    let mut result = Vec::new();
                    for value in values {
                        result.push(self.eval_expr(value).await?);
                    }
                    result
                }),
                Expr::Unwindable {
                    name,
                    body,
                    data: _,
                } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    let handle_var = ir::Name::new("unwindable");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::Expr::Binding(handle_var.clone()),
                        value: ir::Expr::Raw("Symbol()".into()),
                    });
                    self.pattern_match(ir::Expr::Binding(handle_var.clone()), name, &mut stmts)?;
                    let catch_var = ir::Name("e".into());
                    stmts.push(ir::Stmt::Try {
                        body: vec![ir::Stmt::Return(self.eval_expr(body).await?)],
                        catch_var: catch_var.clone(),
                        catch_body: vec![
                            ir::Stmt::If {
                                condition: ir::Expr::NotEq(Box::new(ir::Expr::Index {
                                    obj: Box::new(ir::Expr::Binding(catch_var)),
                                    index: Box::new(ir::Expr::String("handle".into())),
                                })),
                                then_case: vec![ir::Stmt::Throw(ir::Expr::Binding(catch_var))],
                                else_case: vec![],
                            },
                            ir::Stmt::Return(ir::Expr::Index {
                                obj: Box::new(ir::Expr::Binding(catch_var)),
                                index: Box::new(ir::Expr::String("value".into())),
                            }),
                        ],
                    });
                    stmts
                }),
                Expr::Unwind {
                    name,
                    value,
                    data: _,
                } => ir::Expr::scope(vec![ir::Stmt::Throw(ir::Expr::Object {
                    fields: {
                        let mut fields = LinkedHashMap::new();
                        fields.insert(ir::Name("handle".into()), self.eval_expr(name).await?);
                        fields.insert(ir::Name("value".into()), self.eval_expr(value).await?);
                        fields
                    },
                })]),
                Expr::InjectContext { context, data } => ir::Expr::scope(vec![ir::Stmt::Assign {
                    assignee: ir::Expr::Index {
                        obj: Box::new(ir::Expr::Binding(ir::Name::context())),
                        index: Box::new(ir::Expr::String(self.context_name(&data.ty))),
                    },
                    value: self.eval_expr(context).await?,
                }]),
                Expr::CurrentContext { data } => ir::Expr::Index {
                    obj: Box::new(ir::Expr::Binding(ir::Name::context())),
                    index: Box::new(ir::Expr::String(self.context_name(&data.ty))),
                },
                Expr::Unit { data: _ } => ir::Expr::Undefined,
                Expr::FunctionType {
                    arg,
                    contexts,
                    result,
                    data,
                } => todo!(),
                Expr::Cast {
                    value,
                    target,
                    data,
                } => todo!(),
                Expr::Is {
                    value,
                    pattern,
                    data: _,
                } => {
                    self.begin_scope()?;
                    let var = format!("var{}", Id::new());
                    write!(self, "{var}=")?;
                    self.read_place(value).await?;
                    let check_var = format!("check{}", Id::new());
                    write!(self, ";{check_var}=")?;
                    self.pattern_check(&var, pattern)?;
                    write!(self, "if({check_var}){{")?;
                    self.pattern_match(&var, pattern)?;
                    write!(self, "}}return {{check_var}}")?;
                    self.end_scope()?;
                }
                Expr::Match {
                    value,
                    branches,
                    data: _,
                } => {
                    self.begin_scope()?;
                    let var = format!("var{}", Id::new());
                    write!(self, "{var}=")?;
                    self.read_place(value).await?;
                    write!(self, ";")?;
                    for branch in branches {
                        write!(self, "if(")?;
                        self.pattern_check(&var, &branch.pattern)?;
                        write!(self, "){{")?;
                        self.pattern_match(&var, &branch.pattern)?;
                        write!(self, ";return ")?;
                        self.eval_expr(&branch.body).await?;
                        write!(self, "}}")?;
                    }
                    self.end_scope()?;
                }
                Expr::Newtype { def: _, data: _ } => write!(self, "undefined")?,
                Expr::Variant { name, value, data } => {
                    self.variant(
                        name,
                        value
                            .as_ref()
                            .map(|value| async move |this: &mut Self| this.eval_expr(value).await),
                    )
                    .await?;
                }
                Expr::MakeMultiset { values, data } => todo!(),
                Expr::Use { namespace, data } => todo!(),
                Expr::Recursive {
                    body,
                    compiler_scope,
                    data,
                } => todo!(),
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data,
                } => {
                    write!(self, "(")?;
                    self.eval_expr(condition).await?;
                    write!(self, "?")?;
                    self.eval_expr(then_case).await?;
                    write!(self, ":")?;
                    match else_case {
                        Some(else_case) => self.eval_expr(else_case).await?,
                        None => write!(self, "undefined")?,
                    }
                    write!(self, ")")?;
                }
                Expr::Then { list, data: _ } => {
                    self.begin_scope()?;
                    for (index, expr) in list.iter().enumerate() {
                        if index == list.len() - 1 {
                            write!(self, "return ")?;
                            self.eval_expr(expr).await?;
                        } else {
                            self.eval_expr(expr).await?;
                            write!(self, ";")?;
                        }
                    }
                    self.end_scope()?;
                }
                Expr::Constant { value, data: _ } => self.transpile(value).await?,
                Expr::Number { raw, data: _ } => {
                    write!(self, "{raw}")?; // TODO
                }
                Expr::String { token, data: _ } => write!(self, "{:?}", token.contents)?,
                Expr::Native {
                    name,
                    compiler_scope,
                    data: _,
                } => {
                    let mut code: &str = name;
                    while !code.is_empty() {
                        if let Some(index) = code.find('$') {
                            write!(self, "{}", &code[..index])?;
                            code = code[index..].strip_prefix("$(").ok_or_eyre("expected $(")?;
                            let Some(index) = code.find(')') else {
                                eyre::bail!("unclosed $(");
                            };
                            let var = &code[..index];
                            code = code[index..].strip_prefix(')').unwrap();
                            let value = compiler_scope
                                .lookup(var, Hygiene::DefSite, self.kast.spawn_id)
                                .await
                                .ok_or_eyre(format!("{var} not found???"))?;
                            self.transpile(&value).await?;
                        } else {
                            write!(self, "{}", code)?;
                            break;
                        }
                    }
                }
                Expr::Let {
                    is_const_let: _,
                    pattern,
                    value,
                    data: _,
                } => {
                    let temp_var = format!("temp{}", Id::new());
                    write!(self, "{temp_var}=")?;
                    self.begin_scope()?;
                    let value = value.as_ref().expect("no value in let?");
                    self.read_place(value).await?;
                    write!(self, ";")?;
                    self.pattern_match(&temp_var, pattern)?;
                    self.end_scope()?;
                }
                Expr::Call { f, arg, data: _ } => {
                    write!(self, "(")?;
                    self.eval_expr(f).await?;
                    write!(self, ")({CONTEXT_VAR},")?;
                    self.eval_expr(arg).await?;
                    write!(self, ")")?;
                }
                Expr::CallMacro { r#macro, arg, data } => todo!(),
                Expr::Scope { expr, data: _ } => {
                    self.eval_expr(expr).await?; // TODO
                }
                Expr::Function {
                    ty: _,
                    compiled,
                    data: _,
                } => {
                    self.transpile_compiled_fn(compiled).await?;
                }
                Expr::Template { compiled, data } => todo!(),
                Expr::Instantiate {
                    template,
                    arg,
                    data: _,
                } => {
                    write!(self, "(")?;
                    self.eval_expr(template).await?;
                    write!(self, ")(")?;
                    self.eval_expr(arg).await?;
                    write!(self, ")")?;
                }
                Expr::Tuple { tuple, data: _ } => {
                    write!(self, "({{")?;
                    for (index, (member, field)) in tuple.as_ref().into_iter().enumerate() {
                        if index != 0 {
                            write!(self, ",")?;
                        }
                        write!(self, "{:?}:", member.to_string())?;
                        self.eval_expr(field).await?;
                    }
                    write!(self, "}})")?;
                }
                Expr::Ast {
                    expr_root,
                    definition,
                    values,
                    hygiene,
                    def_site,
                    data,
                } => todo!(),
                Expr::ReadPlace { place, data: _ } => {
                    self.read_place(place).await?;
                }
                Expr::TargetDependent { branches, data: _ } => {
                    // println!("{expr}");
                    let body = self
                        .kast
                        .select_target_dependent_branch(branches, Target::JavaScript)
                        .await?;
                    // println!("{body}");
                    self.eval_expr(body).await?
                }
            })
        };
        async move {
            r#impl
                .await
                .wrap_err_with(|| format!("while transpiling {}", expr.show_short()))
        }
        .boxed()
    }

    fn transpile<'a>(&'a mut self, value: &'a Value) -> BoxFuture<'a, eyre::Result<ir::Expr>> {
        async move {
            let value = value.as_inferred()?;
            let value: &ValueShape = &value;
            Ok(match value {
                ValueShape::Unit => ir::Expr::Undefined,
                ValueShape::Bool(value) => ir::Expr::Bool(*value),
                ValueShape::Int32(value) => ir::Expr::Number((*value).into()),
                ValueShape::Int64(value) => ir::Expr::Number((*value).into()),
                ValueShape::Float64(value) => ir::Expr::Number((**value).into()),
                ValueShape::Char(c) => ir::Expr::String(c.to_string()),
                ValueShape::String(s) => ir::Expr::String(s.clone()),
                ValueShape::List(list) => ir::Expr::List({
                    let mut result = Vec::new();
                    for place in list.values.iter() {
                        let value = place.read_value()?;
                        result.push(self.transpile(&value).await?);
                    }
                    result
                }),
                ValueShape::Tuple(tuple) => ir::Expr::Object {
                    fields: {
                        let mut result = LinkedHashMap::new();
                        for (member, value) in tuple.values()?.into_iter() {
                            result.insert(member.to_string(), self.transpile(&value).await?);
                        }
                        result
                    },
                },
                ValueShape::Function(f) => self.transpile_fn(f).await?,
                ValueShape::Template(t) => {
                    self.transpile_fn(t).await? // TODO it should have memoization
                }
                ValueShape::Macro(_) => todo!(),
                ValueShape::NativeFunction(native_function) => todo!("{}", native_function.name),
                ValueShape::Binding(binding) => ir::Expr::binding(binding),
                ValueShape::Variant(variant) => ir::Expr::new_variant(
                    &variant.name,
                    match &variant.value {
                        None => None,
                        Some(place) => {
                            let value = place.read_value()?;
                            Some(self.transpile(&value).await?)
                        }
                    },
                ),
                ValueShape::Multiset(_) => todo!(),
                ValueShape::Contexts(_) => todo!(),
                ValueShape::Ast(_) => todo!(),
                ValueShape::Expr(expr) => todo!(),
                ValueShape::Type(_) => {
                    ir::Expr::Undefined // TODO
                }
                ValueShape::SyntaxModule(_) => todo!(),
                ValueShape::SyntaxDefinition(_) => todo!(),
                ValueShape::UnwindHandle(unwind_handle) => todo!(),
                ValueShape::Symbol(symbol) => todo!(),
                ValueShape::HashMap(hash_map) => todo!(),
                ValueShape::Ref(place_ref) => todo!(),
                ValueShape::Target(target) => todo!(),
            })
        }
        .boxed()
    }

    fn assign<'a>(
        &'a mut self,
        assignee: &'a AssigneeExpr,
        value: ir::Expr,
        stmts: &mut Vec<ir::Stmt>,
    ) -> BoxFuture<'a, eyre::Result<()>> {
        async move {
            match assignee {
                AssigneeExpr::Placeholder { data: _ } => {}
                AssigneeExpr::Unit { data: _ } => {}
                AssigneeExpr::Tuple { tuple, data: _ } => {
                    for (member, field_assignee) in tuple.iter() {
                        self.assign(
                            field_assignee,
                            ir::Expr::Index {
                                obj: Box::new(value.clone()),
                                index: Box::new(ir::Expr::String(member.to_string())),
                            },
                            stmts,
                        )
                        .await?;
                    }
                }
                AssigneeExpr::Place { place, data: _ } => {
                    stmts.push(ir::Stmt::Expr(ir::Expr::Call {
                        f: Box::new(ir::Expr::Index {
                            obj: Box::new(self.eval_place(place).await?),
                            index: Box::new(ir::Expr::String("set".into())),
                        }),
                        args: vec![value],
                    }));
                }
                AssigneeExpr::Let { pattern, data: _ } => {
                    self.pattern_match(value, pattern, stmts)?
                }
            }
            Ok(())
        }
        .boxed()
    }

    fn pattern_check(&mut self, value: &str, pattern: &Pattern) -> eyre::Result<()> {
        match pattern {
            Pattern::Placeholder { data: _ } => write!(self, "true")?,
            Pattern::Unit { data: _ } => write!(self, "true")?,
            Pattern::Binding { .. } => write!(self, "true")?,
            Pattern::Tuple { tuple, data: _ } => {
                for (index, (member, field)) in tuple.as_ref().into_iter().enumerate() {
                    if index != 0 {
                        write!(self, "&&")?;
                    }
                    self.pattern_check(&format!("{value}[{:?}]", member.to_string()), field)?;
                }
            }
            Pattern::Variant {
                name,
                value: value_pattern,
                data: _,
            } => {
                write!(self, "{value}.variant=={name:?}")?;
                if let Some(value_pattern) = value_pattern {
                    write!(self, "&&")?;
                    self.pattern_check(&format!("{value_pattern}.value"), value_pattern)?;
                }
            }
        }
        Ok(())
    }

    fn pattern_match(
        &mut self,
        value: ir::Expr,
        pattern: &Pattern,
        stmts: &mut Vec<ir::Stmt>,
    ) -> eyre::Result<()> {
        match pattern {
            Pattern::Placeholder { data: _ } => {}
            Pattern::Unit { data: _ } => {}
            Pattern::Binding {
                binding,
                bind_mode: _, // TODO maybe we care?
                data: _,
            } => {
                stmts.push(ir::Stmt::Assign {
                    assignee: ir::Name::for_binding(binding),
                    value,
                });
            }
            Pattern::Tuple { tuple, data: _ } => {
                for (member, field_pattern) in tuple.iter() {
                    self.pattern_match(
                        ir::Expr::Index {
                            obj: value.clone(),
                            index: ir::Expr::String(member.to_string()),
                        },
                        field_pattern,
                        stmts,
                    )?;
                }
            }
            Pattern::Variant {
                name,
                value: value_pattern,
                data: _,
            } => {
                self.assert(ir::Expr::variant_name(value.clone()), stmts);
                if let Some(value_pattern) = value_pattern {
                    self.pattern_match(ir::Expr::variant_value(value), value_pattern, stmts)?;
                }
            }
        }
        Ok(())
    }

    fn assert(&mut self, expr: ir::Expr, stmts: &mut Vec<ir::Stmt>) {
        stmts.push(ir::Stmt::If {
            condition: Box::new(ir::Expr::Not(Box::new(expr))),
            then_case: vec![ir::Stmt::Throw(Box::new(ir::Expr::String(
                "assertion failed".into(),
            )))],
            else_case: vec![],
        });
    }

    fn ensure_captured(&mut self, place: &PlaceRef) {
        if self.captured.insert(place.clone()) {
            self.captured_unprocessed.push(place.clone());
        }
    }

    async fn process_captured(&mut self) -> eyre::Result<()> {
        while let Some(unprocessed) = self.captured_unprocessed.pop() {
            write!(self, "captured{}=", unprocessed.place.id)?;
            self.transpile(&*unprocessed.read_value()?).await?;
            write!(self, ";")?;
        }
        Ok(())
    }

    async fn transpile_fn(&mut self, f: &Function) -> eyre::Result<ir::Expr> {
        let old_scopes = std::mem::replace(&mut self.scopes, f.captured.clone());
        let result = self.transpile_compiled_fn(&f.compiled).await;
        self.scopes = old_scopes;
        result
    }

    async fn transpile_compiled_fn(&mut self, f: &MaybeCompiledFn) -> eyre::Result<ir::Expr> {
        let compiled = self.kast.await_compiled(f).await?;
        let arg = ir::Name::unique("arg");
        let args = vec![ir::Name::context(), arg.clone()];
        let mut body = Vec::new();
        self.pattern_match(ir::Expr::Binding(arg), &compiled.arg, &mut body)?;
        body.push(ir::Stmt::Return(Box::new(
            self.eval_expr(&compiled.body).await?,
        )));
        Ok(ir::Expr::Fn { args, body })
    }

    async fn variant(
        &mut self,
        name: &str,
        value: Option<impl AsyncFnOnce(&mut Self) -> eyre::Result<()>>,
    ) -> eyre::Result<()> {
        write!(self, "{{\"variant\":{:?},\"value\":", name)?;
        match value {
            Some(value) => value(self).await?,
            None => write!(self, "undefined")?,
        }
        write!(self, "}}")?;
        Ok(())
    }
}

impl Kast {
    pub async fn transpile_to_javascript2(
        &mut self,
        engine_type: JavaScriptEngineType,
        value: &Value,
    ) -> eyre::Result<String> {
        let mut transpiler = Transpiler::new(self, engine_type);
        transpiler.begin_scope()?;
        write!(transpiler, "value=")?;
        transpiler.transpile(value).await?;
        write!(transpiler, ";")?;
        transpiler.process_captured().await?;
        write!(transpiler, "return value")?;
        transpiler.end_scope()?;
        Ok(transpiler.code)
    }
}
