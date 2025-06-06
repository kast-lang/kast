use super::*;
use eyre::OptionExt as _;
use linked_hash_map::LinkedHashMap;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum JavaScriptEngineType {
    Node,
    Browser,
}

#[derive(Copy, Clone, Debug)]
pub enum ShowOptions {
    Minified,
    Pretty, // TODO { max_line_length: usize },
}

mod ir {
    use super::*;

    #[derive(Clone)]
    pub struct Number(pub String);

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
    #[derive(Clone)]
    pub enum Expr {
        Binding(Name),
        Undefined,
        Bool(bool),
        Number(Number),
        String(String),
        List(Vec<Expr>),
        Object { fields: LinkedHashMap<String, Expr> },
        Fn { args: Vec<Name>, body: Vec<Stmt> },
        Index { obj: Box<Expr>, index: Box<Expr> },
        NotEq(Box<Expr>, Box<Expr>),
        Call { f: Box<Expr>, args: Vec<Expr> },
        Raw(String),
    }

    #[must_use]
    #[derive(Clone)]
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
            fields.insert("variant".into(), ir::Expr::String(name.into()));
            if let Some(value) = value {
                fields.insert("value".into(), value.into());
            }
            Self::Object { fields }
        }
        pub fn variant_name(value: Self) -> Self {
            Self::Index {
                obj: Box::new(value),
                index: Box::new(Self::String("variant".into())),
            }
        }
        pub fn variant_value(value: Self) -> Self {
            Self::Index {
                obj: Box::new(value),
                index: Box::new(Self::String("value".into())),
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

    fn write_stmts(
        f: &mut std::fmt::Formatter<'_>,
        stmts: &[Stmt],
        options: ShowOptions,
    ) -> std::fmt::Result {
        pad_adapter::padded_items(
            f,
            stmts,
            pad_adapter::OuterMode::Surrounded,
            pad_adapter::Moded {
                normal: pad_adapter::Separator::Seq(";"),
                alternate: pad_adapter::Separator::After(";"),
            },
            |f, stmt| f.write(stmt.show(options)),
        )
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
                        Expr::Binding(name) => write!(f, "{}", name.0)?,
                        Expr::Undefined => write!(f, "undefined")?,
                        Expr::Bool(value) => write!(f, "{value}")?,
                        Expr::Number(value) => write!(f, "{}", value.0)?,
                        Expr::String(value) => write!(f, "{value:?}")?,
                        Expr::List(values) => {
                            write!(f, "[")?;
                            pad_adapter::padded_items(
                                f,
                                values,
                                pad_adapter::OuterMode::Surrounded,
                                pad_adapter::Moded {
                                    normal: pad_adapter::Separator::Seq(","),
                                    alternate: pad_adapter::Separator::Seq(","),
                                },
                                |f, value| {
                                    f.write(value.show(self.options))?;
                                    Ok(())
                                },
                            )?;
                            write!(f, "]")?;
                        }
                        Expr::Fn { args, body } => {
                            write!(f, "((")?;
                            for (index, arg) in args.iter().enumerate() {
                                if index != 0 {
                                    write!(f, ",")?;
                                    if let ShowOptions::Pretty = self.options {
                                        write!(f, " ")?;
                                    }
                                }
                                write!(f, "{}", arg.0)?;
                            }
                            match self.options {
                                ShowOptions::Minified => write!(f, ")=>{{")?,
                                ShowOptions::Pretty => write!(f, ") => {{")?,
                            }
                            write_stmts(f, body, self.options)?;
                            write!(f, "}})")?;
                        }
                        Expr::Index { obj, index } => {
                            f.write(obj.show(self.options))?;
                            write!(f, "[")?;
                            f.write(index.show(self.options))?;
                            write!(f, "]")?;
                        }
                        Expr::NotEq(a, b) => {
                            f.write(a.show(self.options))?;
                            match self.options {
                                ShowOptions::Minified => write!(f, "!==")?,
                                ShowOptions::Pretty => write!(f, " !== ")?,
                            }
                            f.write(b.show(self.options))?;
                        }
                        Expr::Call { f: fun, args } => {
                            f.write(fun.show(self.options))?;
                            write!(f, "(")?;
                            pad_adapter::padded_items(
                                f,
                                args,
                                pad_adapter::OuterMode::Surrounded,
                                pad_adapter::Moded {
                                    normal: pad_adapter::Separator::Seq(","),
                                    alternate: pad_adapter::Separator::Seq(","),
                                },
                                |f, arg| f.write(arg.show(self.options)),
                            )?;
                            write!(f, ")")?;
                        }
                        Expr::Raw(s) => write!(f, "{s}")?,
                    }
                    Ok(())
                }
            }
            struct Wrapper<T>(T, ShowOptions);
            impl<T: std::fmt::Display> std::fmt::Display for Wrapper<T> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.1 {
                        ShowOptions::Minified => write!(f, "{}", self.0),
                        ShowOptions::Pretty => write!(f, "{:#}", self.0),
                    }
                }
            }
            Wrapper(
                Show {
                    expr: self,
                    options,
                },
                options,
            )
        }
    }

    impl Stmt {
        pub fn show(&self, options: ShowOptions) -> impl std::fmt::Display + '_ {
            struct Show<'a> {
                stmt: &'a Stmt,
                options: ShowOptions,
            }
            impl std::fmt::Display for Show<'_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.stmt {
                        Stmt::Return(expr) => {
                            write!(f, "return ")?;
                            f.write(expr.show(self.options))?;
                        }
                        Stmt::If {
                            condition,
                            then_case,
                            else_case,
                        } => {
                            write!(f, "if(")?;
                            f.write(condition.show(self.options))?;
                            write!(f, "){{")?;
                            write_stmts(f, then_case, self.options)?;
                            write!(f, "}}else{{")?;
                            write_stmts(f, else_case, self.options)?;
                            write!(f, "}}")?;
                        }
                        Stmt::Throw(expr) => {
                            write!(f, "throw ")?;
                            f.write(expr.show(self.options))?;
                        }
                        Stmt::Assign { assignee, value } => {
                            f.write(assignee.show(self.options))?;
                            match self.options {
                                ShowOptions::Minified => write!(f, "=")?,
                                ShowOptions::Pretty => write!(f, " = ")?,
                            }
                            f.write(value.show(self.options))?;
                        }
                        Stmt::Expr(expr) => {
                            write!(f, "(")?;
                            f.write(expr.show(self.options))?;
                            write!(f, ")")?;
                        }
                        Stmt::Try {
                            body,
                            catch_var,
                            catch_body,
                        } => {
                            write!(f, "try{{")?;
                            write_stmts(f, body, self.options)?;
                            write!(f, "}}catch({}){{", catch_var.0)?;
                            write_stmts(f, catch_body, self.options)?;
                            write!(f, "}}")?;
                        }
                    }
                    Ok(())
                }
            }
            struct Wrapper<T>(T, ShowOptions);
            impl<T: std::fmt::Display> std::fmt::Display for Wrapper<T> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.1 {
                        ShowOptions::Minified => write!(f, "{}", self.0),
                        ShowOptions::Pretty => write!(f, "{:#}", self.0),
                    }
                }
            }
            Wrapper(
                Show {
                    stmt: self,
                    options,
                },
                options,
            )
        }
    }

    impl Expr {
        pub fn optimize(&self) -> Self {
            match self {
                Self::Binding(_) => self.clone(),
                Self::Undefined => self.clone(),
                Self::Bool(_) => self.clone(),
                Self::Number(_) => self.clone(),
                Self::String(_) => self.clone(),
                Self::List(values) => Self::List(values.into_iter().map(Self::optimize).collect()),
                Self::Object { fields } => Self::Object {
                    fields: fields
                        .iter()
                        .map(|(key, value)| (key.clone(), value.optimize()))
                        .collect(),
                },
                Self::Fn { args, body } => Self::Fn {
                    args: args.clone(),
                    body: optimize_stmts(body),
                },
                Self::Index { obj, index } => {
                    let obj = obj.optimize();
                    let index = index.optimize();
                    fn get_obj_field(obj: &Expr, index: &Expr) -> Option<Expr> {
                        let Expr::Object { fields } = obj else {
                            return None;
                        };
                        let Expr::String(field) = index else {
                            return None;
                        };
                        // TODO what if fields had side effects?
                        let field = fields.get(field).expect("field not found");
                        // already optimized
                        Some(field.clone())
                    }
                    if let Some(result) = get_obj_field(&obj, &index) {
                        return result;
                    }
                    Self::Index {
                        obj: Box::new(obj),
                        index: Box::new(index),
                    }
                }
                Self::NotEq(a, b) => {
                    let a = a.optimize();
                    let b = b.optimize();
                    Self::NotEq(Box::new(a), Box::new(b))
                }
                Self::Call { f, args } => {
                    let f = f.optimize();
                    let args: Vec<Self> = args.iter().map(Self::optimize).collect();
                    fn lambda_return_call(f: &Expr, args: &[Expr]) -> Option<Expr> {
                        if !args.is_empty() {
                            return None;
                        }
                        let Expr::Fn { args: f_args, body } = f else {
                            return None;
                        };
                        if !f_args.is_empty() {
                            return None;
                        }
                        let [stmt] = body.as_slice() else { return None };
                        let Stmt::Return(expr) = stmt else {
                            return None;
                        };
                        // already optimized
                        Some(expr.clone())
                    }
                    if let Some(result) = lambda_return_call(&f, &args) {
                        return result;
                    }
                    Self::Call {
                        f: Box::new(f),
                        args,
                    }
                }
                Self::Raw(_) => self.clone(),
            }
        }
    }

    impl Stmt {
        pub fn optimize(&self) -> Self {
            match self {
                Self::Return(expr) => Self::Return(expr.optimize()),
                Self::If {
                    condition,
                    then_case,
                    else_case,
                } => Self::If {
                    condition: condition.optimize(),
                    then_case: optimize_stmts(then_case),
                    else_case: optimize_stmts(else_case),
                },
                Self::Throw(expr) => Self::Throw(expr.optimize()),
                Self::Assign { assignee, value } => Self::Assign {
                    assignee: assignee.optimize(),
                    value: value.optimize(),
                },
                Self::Expr(expr) => Self::Expr(expr.optimize()),
                Self::Try {
                    body,
                    catch_var,
                    catch_body,
                } => Self::Try {
                    body: optimize_stmts(body),
                    catch_var: catch_var.clone(),
                    catch_body: optimize_stmts(catch_body),
                },
            }
        }
    }

    fn optimize_stmts(stmts: &[Stmt]) -> Vec<Stmt> {
        let mut result = Vec::new();
        for stmt in stmts {
            let stmt = stmt.optimize();
            fn return_lambda_call(stmt: &Stmt) -> Option<Vec<Stmt>> {
                let Stmt::Return(expr) = stmt else {
                    return None;
                };
                let Expr::Call { f, args } = expr else {
                    return None;
                };
                let Expr::Fn { args: f_args, body } = &**f else {
                    return None;
                };
                if args.len() != f_args.len() {
                    return None;
                }
                if !args.is_empty() {
                    return None;
                };
                // already optimized
                Some(body.clone())
            }
            if let Some(stmts) = return_lambda_call(&stmt) {
                result.extend(stmts);
                continue;
            }
            result.push(stmt);
        }
        result
    }
}

struct Transpiler {
    kast: Kast,
    scopes: Parc<Scopes>,
    engine_type: JavaScriptEngineType,
    captured: std::collections::HashSet<PlaceRef>,
    captured_code: Vec<ir::Stmt>,
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
            captured_code: vec![],
            captured_unprocessed: Default::default(),
            type_ids: HashMap::new(),
        }
    }

    // TODO maybe assignee/place expr?
    fn make_ref(&mut self, value: ir::Expr) -> eyre::Result<ir::Expr> {
        let new_value_var = ir::Name::unique("new_value");
        let mut fields = LinkedHashMap::new();
        fields.insert(
            "get".into(),
            ir::Expr::Fn {
                args: vec![],
                body: vec![ir::Stmt::Return(value.clone())],
            },
        );
        fields.insert(
            "set".into(),
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
                            self.ensure_captured(&place);
                            self.make_ref(ir::Expr::Binding(captured_name(&place)))?
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
                            value: self.read_place(obj).await?, // The value is a reference type since it has fields
                        },
                        ir::Stmt::Return(self.make_ref(ir::Expr::Index {
                            obj: Box::new(ir::Expr::Binding(temp_var)),
                            index: Box::new(ir::Expr::String(field.to_string())),
                        })?),
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
                    let handle_var = ir::Name::unique("unwindable");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::Expr::Binding(handle_var.clone()),
                        value: ir::Expr::Raw("Symbol()".into()),
                    });
                    self.pattern_match(
                        ir::Expr::Binding(handle_var.clone()),
                        name,
                        &mut stmts,
                        MatchMode::Assign,
                    )?;
                    let catch_var = ir::Name("e".into());
                    stmts.push(ir::Stmt::Try {
                        body: vec![ir::Stmt::Return(self.eval_expr(body).await?)],
                        catch_var: catch_var.clone(),
                        catch_body: vec![
                            ir::Stmt::If {
                                condition: ir::Expr::NotEq(
                                    Box::new(ir::Expr::Index {
                                        obj: Box::new(ir::Expr::Binding(catch_var.clone())),
                                        index: Box::new(ir::Expr::String("handle".into())),
                                    }),
                                    Box::new(ir::Expr::Binding(handle_var.clone())),
                                ),
                                then_case: vec![ir::Stmt::Throw(ir::Expr::Binding(
                                    catch_var.clone(),
                                ))],
                                else_case: vec![],
                            },
                            ir::Stmt::Return(ir::Expr::Index {
                                obj: Box::new(ir::Expr::Binding(catch_var.clone())),
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
                        fields.insert("handle".into(), self.eval_expr(name).await?);
                        fields.insert("value".into(), self.eval_expr(value).await?);
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
                    // TODO assign first
                    let value = self.read_place(value).await?;
                    self.pattern_check(value, pattern)?
                }
                Expr::Match {
                    value,
                    branches,
                    data: _,
                } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    let var = ir::Name::unique("var");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::Expr::Binding(var.clone()),
                        value: self.read_place(value).await?,
                    });
                    for branch in branches {
                        stmts.push(ir::Stmt::If {
                            condition: self
                                .pattern_check(ir::Expr::Binding(var.clone()), &branch.pattern)?,
                            then_case: vec![ir::Stmt::Return(self.eval_expr(&branch.body).await?)],
                            else_case: vec![],
                        });
                        stmts.push(ir::Stmt::Throw(ir::Expr::String(
                            "non exhaustive match".into(),
                        )));
                    }
                    stmts
                }),
                Expr::Newtype { def: _, data: _ } => ir::Expr::Undefined,
                Expr::Variant { name, value, data } => ir::Expr::new_variant(
                    name,
                    match value {
                        None => None,
                        Some(value) => Some(self.eval_expr(value).await?),
                    },
                ),
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
                } => ir::Expr::scope(vec![ir::Stmt::If {
                    condition: self.eval_expr(condition).await?,
                    then_case: vec![ir::Stmt::Return(self.eval_expr(then_case).await?)],
                    else_case: match else_case {
                        None => vec![],
                        Some(else_case) => vec![ir::Stmt::Return(self.eval_expr(else_case).await?)],
                    },
                }]),
                Expr::Then { list, data: _ } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    for (index, expr) in list.iter().enumerate() {
                        let expr = self.eval_expr(expr).await?;
                        if index == list.len() - 1 {
                            stmts.push(ir::Stmt::Return(expr));
                        } else {
                            stmts.push(ir::Stmt::Expr(expr));
                        }
                    }
                    stmts
                }),
                Expr::Constant { value, data: _ } => self.transpile(value).await?,
                Expr::Number { raw, data: _ } => {
                    ir::Expr::Number(ir::Number(raw.clone())) // TODO based on type
                }
                Expr::String { token, data } => match data
                    .ty
                    .inferred()
                    .map_err(|_| eyre!("string type not inferred"))?
                {
                    TypeShape::String => ir::Expr::String(token.contents.clone()),
                    other => eyre::bail!("wrong string type: {other}"),
                },
                Expr::Native {
                    name,
                    compiler_scope,
                    data: _,
                } => {
                    let mut code: &str = name;
                    let mut result = String::new();
                    while !code.is_empty() {
                        if let Some(index) = code.find('$') {
                            write!(result, "{}", &code[..index])?;
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
                            let value = self.transpile(&value).await?;
                            write!(result, "{}", value.show(ShowOptions::Minified))?;
                        } else {
                            write!(result, "{}", code)?;
                            break;
                        }
                    }
                    ir::Expr::Raw(result)
                }
                Expr::Let {
                    is_const_let: _,
                    pattern,
                    value,
                    data: _,
                } => ir::Expr::scope({
                    let temp_var = ir::Name::unique("let");
                    let mut stmts = Vec::new();
                    let value = value.as_ref().expect("no value in let?");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::Expr::Binding(temp_var.clone()),
                        value: self.read_place(value).await?,
                    });
                    self.pattern_match(
                        ir::Expr::Binding(temp_var),
                        pattern,
                        &mut stmts,
                        MatchMode::Assign,
                    )?;
                    stmts
                }),
                Expr::Call { f, arg, data: _ } => ir::Expr::Call {
                    f: Box::new(self.eval_expr(f).await?),
                    args: vec![
                        ir::Expr::Binding(ir::Name::context()),
                        self.eval_expr(arg).await?,
                    ],
                },
                Expr::CallMacro { r#macro, arg, data } => todo!(),
                Expr::Scope { expr, data: _ } => self.eval_expr(expr).await?,
                Expr::Function {
                    ty: _,
                    compiled,
                    data: _,
                } => self.transpile_compiled_fn(compiled).await?,
                Expr::Template { compiled, data } => todo!(),
                Expr::Instantiate {
                    template,
                    arg,
                    data: _,
                } => ir::Expr::Call {
                    f: Box::new(self.eval_expr(template).await?),
                    args: vec![self.eval_expr(arg).await?],
                },
                Expr::Tuple { tuple, data: _ } => ir::Expr::Object {
                    fields: {
                        let mut fields = LinkedHashMap::new();
                        for (member, field) in tuple.iter() {
                            fields.insert(member.to_string(), self.eval_expr(field).await?);
                        }
                        fields
                    },
                },
                Expr::Ast {
                    expr_root,
                    definition,
                    values,
                    hygiene,
                    def_site,
                    data,
                } => todo!(),
                Expr::ReadPlace { place, data: _ } => self.read_place(place).await?,
                Expr::TargetDependent { branches, data: _ } => {
                    let body = self
                        .kast
                        .select_target_dependent_branch(branches, Target::JavaScript)
                        .await?;
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
        stmts: &'a mut Vec<ir::Stmt>,
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
                    self.pattern_match(value, pattern, stmts, MatchMode::Assign)?
                }
            }
            Ok(())
        }
        .boxed()
    }

    fn pattern_check(&mut self, value: ir::Expr, pattern: &Pattern) -> eyre::Result<ir::Expr> {
        Ok(ir::Expr::scope({
            let mut stmts = Vec::new();
            self.pattern_match(value, pattern, &mut stmts, MatchMode::Check)?;
            stmts.push(ir::Stmt::Return(ir::Expr::Bool(true)));
            stmts
        }))
    }
}

#[derive(Debug, Copy, Clone)]
enum MatchMode {
    Check,
    Assign,
}

impl Transpiler {
    fn pattern_match(
        &mut self,
        value: ir::Expr,
        pattern: &Pattern,
        stmts: &mut Vec<ir::Stmt>,
        mode: MatchMode,
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
                    assignee: ir::Expr::Binding(ir::Name::for_binding(binding)),
                    value,
                });
            }
            Pattern::Tuple { tuple, data: _ } => {
                for (member, field_pattern) in tuple.iter() {
                    self.pattern_match(
                        ir::Expr::Index {
                            obj: Box::new(value.clone()),
                            index: Box::new(ir::Expr::String(member.to_string())),
                        },
                        field_pattern,
                        stmts,
                        mode,
                    )?;
                }
            }
            Pattern::Variant {
                name,
                value: value_pattern,
                data: _,
            } => {
                stmts.push(ir::Stmt::If {
                    condition: ir::Expr::NotEq(
                        Box::new(ir::Expr::variant_name(value.clone())),
                        Box::new(ir::Expr::String(name.clone())),
                    ),
                    then_case: match mode {
                        MatchMode::Assign => {
                            vec![ir::Stmt::Throw(ir::Expr::String("assertion failed".into()))]
                        }
                        MatchMode::Check => vec![ir::Stmt::Return(ir::Expr::Bool(false))],
                    },
                    else_case: vec![],
                });
                if let Some(value_pattern) = value_pattern {
                    self.pattern_match(ir::Expr::variant_value(value), value_pattern, stmts, mode)?;
                }
            }
        }
        Ok(())
    }

    fn ensure_captured(&mut self, place: &PlaceRef) {
        if self.captured.insert(place.clone()) {
            self.captured_unprocessed.push(place.clone());
        }
    }
}

fn captured_name(place_ref: &PlaceRef) -> ir::Name {
    ir::Name(format!("captured{}", place_ref.place.id))
}

impl Transpiler {
    async fn process_captured(&mut self) -> eyre::Result<()> {
        while let Some(unprocessed) = self.captured_unprocessed.pop() {
            let value = self.transpile(&*unprocessed.read_value()?).await?;
            self.captured_code.push(ir::Stmt::Assign {
                assignee: ir::Expr::Binding(captured_name(&unprocessed)),
                value,
            });
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
        self.pattern_match(
            ir::Expr::Binding(arg),
            &compiled.arg,
            &mut body,
            MatchMode::Assign,
        )?;
        body.push(ir::Stmt::Return(self.eval_expr(&compiled.body).await?));
        Ok(ir::Expr::Fn { args, body })
    }
}

impl Kast {
    pub async fn transpile_to_javascript(
        &mut self,
        engine_type: JavaScriptEngineType,
        value: &Value,
        options: ShowOptions,
    ) -> eyre::Result<String> {
        let mut transpiler = Transpiler::new(self, engine_type);
        let result = transpiler.transpile(value).await?;
        transpiler.process_captured().await?;
        let expr = ir::Expr::scope({
            let mut stmts = transpiler.captured_code;
            stmts.push(ir::Stmt::Return(result));
            stmts
        });
        let expr = expr.optimize();
        Ok(expr.show(options).to_string())
    }
}
