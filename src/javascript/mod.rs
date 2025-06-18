use super::*;
use eyre::OptionExt as _;
use linked_hash_map::LinkedHashMap;
use std::fmt::Write;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum JavaScriptEngineType {
    Node,
    Browser,
}

#[derive(Copy, Clone, Debug)]
pub enum ShowOptions {
    Minified,
    Pretty, // TODO { max_line_length: usize },
}

macro_rules! todo {
    () => {
        eyre::bail!("TODO at {}:{}:{}", file!(), line!(), column!())
    };
    ($($t:tt)*) => {
        return Err(eyre!($($t)*).wrap_err(format!("{}:{}:{}", file!(), line!(), column!())))
    };
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
        Index { obj: Box<Expr>, index: Box<Expr> },
        NotEq(Box<Expr>, Box<Expr>),
        AsyncFn { args: Vec<Name>, body: Vec<Stmt> },
        AwaitCall { f: Box<Expr>, args: Vec<Expr> },
        SyncFn { args: Vec<Name>, body: Vec<Stmt> },
        SyncCall { f: Box<Expr>, args: Vec<Expr> },
        Raw(String),
    }

    impl Expr {
        fn has_bindings(&self) -> bool {
            match self {
                Self::Binding(_) => true,
                Self::Undefined => false,
                Self::Bool(_) => false,
                Self::Number(_) => false,
                Self::String(_) => false,
                Self::List(exprs) => exprs.iter().all(Self::has_bindings),
                Self::Object { fields } => fields.values().all(Self::has_bindings),
                Self::Index { obj, index } => obj.has_bindings() || index.has_bindings(),
                Self::NotEq(a, b) => a.has_bindings() || b.has_bindings(),
                Self::AsyncFn { args: _, body: _ } | Self::SyncFn { args: _, body: _ } => true, // too lazy too look deeper
                Self::AwaitCall { f, args } | Self::SyncCall { f, args } => {
                    f.has_bindings() || args.iter().any(Self::has_bindings)
                }
                Self::Raw(_) => true, // can be anything
            }
        }
    }

    #[must_use]
    #[derive(Clone)]
    pub enum AssigneeExpr {
        NewVar(Name),
        /// Make sure its working as an assignee
        Expr(Expr),
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
        DefVar(Name),
        Assign {
            assignee: AssigneeExpr,
            value: Expr,
        },
        Expr(Expr),
        Try {
            body: Vec<Stmt>,
            catch_var: Name,
            catch_body: Vec<Stmt>,
        },
        Scope {
            body: Vec<Stmt>,
        },
        Raw(String),
    }

    impl Expr {
        pub fn binding(binding: &Binding) -> Self {
            Self::Binding(Name::for_binding(binding))
        }
        pub fn new_variant(name: impl Into<String>, value: Option<Self>) -> Self {
            let mut fields = LinkedHashMap::new();
            fields.insert("variant".into(), ir::Expr::String(name.into()));
            if let Some(value) = value {
                fields.insert("value".into(), value);
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
            Self::AwaitCall {
                f: Box::new(Self::AsyncFn {
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
                        Expr::SyncFn { args, body } => {
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
                        Expr::AsyncFn { args, body } => {
                            write!(f, "(async function(")?;
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
                                ShowOptions::Minified => write!(f, "){{")?,
                                ShowOptions::Pretty => write!(f, ") {{")?,
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
                        Expr::SyncCall { f: fun, args } => {
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
                        Expr::AwaitCall { f: fun, args } => {
                            write!(f, "(await ")?;
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

    impl AssigneeExpr {
        pub fn show(&self, options: ShowOptions) -> impl std::fmt::Display + '_ {
            struct Show<'a> {
                expr: &'a AssigneeExpr,
                options: ShowOptions,
            }
            impl std::fmt::Display for Show<'_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self.expr {
                        AssigneeExpr::NewVar(name) => write!(f, "var {}", name.0)?,
                        AssigneeExpr::Expr(expr) => f.write(expr.show(self.options))?,
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
                        Stmt::Raw(s) => {
                            write!(f, "{s}")?;
                        }
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
                        Stmt::DefVar(name) => {
                            write!(f, "var {}", name.0)?;
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
                        Stmt::Scope { body } => {
                            write!(f, "{{")?;
                            write_stmts(f, body, self.options)?;
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

        pub fn throw_error(s: &str) -> Stmt {
            Stmt::Throw(Expr::AwaitCall {
                f: Box::new(Expr::Raw("Error".into())),
                args: vec![Expr::String(s.into())],
            })
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
                Self::List(values) => Self::List(values.iter().map(Self::optimize).collect()),
                Self::Object { fields } => Self::Object {
                    fields: fields
                        .iter()
                        .map(|(key, value)| (key.clone(), value.optimize()))
                        .collect(),
                },
                Self::SyncFn { args, body } => Self::SyncFn {
                    args: args.clone(),
                    body: optimize_stmts(body),
                },
                Self::AsyncFn { args, body } => Self::AsyncFn {
                    args: args.clone(),
                    body: optimize_stmts(body),
                },
                Self::Index { obj, index } => {
                    let obj = obj.optimize();
                    let index = index.optimize();

                    /// from: { field: value, ... }["field"]
                    /// into: value
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

                    /// from: ((args) => { ...; return obj; })(args)["field"]
                    /// into: ((args) => { ...; return obj["field"]; })(args)
                    fn inline_lambda_obj_field(obj: &Expr, index: &Expr) -> Option<Expr> {
                        let Expr::AwaitCall { f, args } = obj else {
                            return None;
                        };
                        let Expr::AsyncFn { args: f_args, body } = &**f else {
                            return None;
                        };
                        let (Stmt::Return(ret), before_return) = body.split_last()? else {
                            return None;
                        };
                        if has_return(before_return) {
                            return None;
                        }
                        Some(
                            Expr::AwaitCall {
                                f: Box::new(Expr::AsyncFn {
                                    args: f_args.clone(),
                                    body: {
                                        let mut body = before_return.to_vec();
                                        body.push(Stmt::Return(Expr::Index {
                                            obj: Box::new(ret.clone()),
                                            index: Box::new(index.clone()),
                                        }));
                                        body
                                    },
                                }),
                                args: args.clone(),
                            }
                            .optimize(), // TODO optimizes body again?
                        )
                    }
                    if let Some(result) = inline_lambda_obj_field(&obj, &index) {
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
                Self::SyncCall { f, args } => Self::SyncCall {
                    f: Box::new(f.optimize()),
                    args: args.iter().map(Self::optimize).collect(),
                },
                Self::AwaitCall { f, args } => {
                    let f = f.optimize();
                    let args: Vec<Self> = args.iter().map(Self::optimize).collect();

                    /// from: (() => { return x })()
                    /// into: x
                    fn lambda_return_call(f: &Expr, args: &[Expr]) -> Option<Expr> {
                        if !args.is_empty() {
                            return None;
                        }
                        let Expr::AsyncFn { args: f_args, body } = f else {
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

                    /// from: ((args) => { ...; return f; })(args)(outer_args)
                    /// into: ((args) => { ...; return f(outer_args); })(args)
                    fn inline_lambda_f_call(outer_f: &Expr, outer_args: &[Expr]) -> Option<Expr> {
                        if outer_args.iter().any(Expr::has_bindings) {
                            return None;
                        }
                        let Expr::AwaitCall { f, args } = outer_f else {
                            return None;
                        };
                        let Expr::AsyncFn { args: f_args, body } = &**f else {
                            return None;
                        };
                        let (Stmt::Return(ret), before_return) = body.split_last()? else {
                            return None;
                        };
                        if has_return(before_return) {
                            return None;
                        }
                        Some(
                            Expr::AwaitCall {
                                f: Box::new(Expr::AsyncFn {
                                    args: f_args.clone(),
                                    body: {
                                        let mut body = before_return.to_vec();
                                        body.push(Stmt::Return(Expr::AwaitCall {
                                            f: Box::new(ret.clone()),
                                            args: outer_args.to_vec(),
                                        }));
                                        body
                                    },
                                }),
                                args: args.clone(),
                            }
                            .optimize(), // TODO optimizes body again?
                        )
                    }
                    if let Some(result) = inline_lambda_f_call(&f, &args) {
                        return result;
                    }

                    Self::AwaitCall {
                        f: Box::new(f),
                        args,
                    }
                }
                Self::Raw(_) => self.clone(),
            }
        }
    }

    impl AssigneeExpr {
        pub fn optimize(&self) -> Self {
            match self {
                Self::NewVar(_) => self.clone(),
                Self::Expr(expr) => Self::Expr(expr.optimize()),
            }
        }
    }

    impl Stmt {
        pub fn optimize(&self) -> Self {
            match self {
                Self::Raw(_) => self.clone(),
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
                Self::DefVar(_) => self.clone(),
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
                Self::Scope { body } => Self::Scope {
                    body: optimize_stmts(body),
                },
            }
        }
    }

    fn optimize_stmts(stmts: &[Stmt]) -> Vec<Stmt> {
        let mut result = Vec::new();
        for stmt in stmts {
            let stmt = stmt.optimize();

            /// from: return (() => { body })();
            /// into: body
            fn return_lambda_call(stmt: &Stmt) -> Option<Vec<Stmt>> {
                let Stmt::Return(expr) = stmt else {
                    return None;
                };
                let Expr::AwaitCall { f, args } = expr else {
                    return None;
                };
                let Expr::AsyncFn { args: f_args, body } = &**f else {
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

            /// from: x = expr; return x;
            /// into: return expr;
            fn return_temp(prev: Option<&Stmt>, stmt: &Stmt) -> Option<Stmt> {
                let Stmt::Assign { assignee, value } = prev? else {
                    return None;
                };
                // TODO make sure its a new var
                let AssigneeExpr::NewVar(name) = assignee else {
                    return None;
                };
                let Stmt::Return(Expr::Binding(ret_name)) = stmt else {
                    return None;
                };
                if name != ret_name {
                    return None;
                };
                Some(Stmt::Return(value.clone()))
            }
            if let Some(stmt) = return_temp(result.last(), &stmt) {
                _ = result.pop().unwrap();
                result.push(stmt);
                continue;
            }

            result.push(stmt);
        }
        result
    }

    fn has_return(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            let stmt_has_return = match stmt {
                Stmt::Raw(_) => false, // TODO ????
                Stmt::Return(_) => true,
                Stmt::If {
                    condition: _,
                    then_case,
                    else_case,
                } => has_return(then_case) || has_return(else_case),
                Stmt::Throw(_) => false,
                Stmt::DefVar(_) => false,
                Stmt::Assign { .. } => false,
                Stmt::Expr(..) => false,
                Stmt::Try {
                    body,
                    catch_var: _,
                    catch_body: _,
                } => has_return(body),
                Stmt::Scope { body } => has_return(body),
            };
            if stmt_has_return {
                return true;
            }
        }
        false
    }
}

struct Transpiler {
    kast: Kast,
    compiled_fns: std::collections::HashMap<Parc<CompiledFn>, ir::Name>,
    scopes: Parc<Scopes>,
    engine_type: JavaScriptEngineType,
    target: Target,
    captured: std::collections::HashSet<PlaceRef>,
    init_code: Vec<ir::Stmt>,
    cleanup_code: Vec<ir::Stmt>,
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

impl Transpiler {
    fn new(kast: &mut Kast, engine_type: JavaScriptEngineType) -> Self {
        Self {
            compiled_fns: Default::default(),
            kast: kast.clone(),
            scopes: Parc::new(kast.scopes.clone()),
            engine_type,
            target: Target::JavaScript {
                engine: engine_type,
            },
            captured: Default::default(),
            init_code: vec![],
            cleanup_code: vec![],
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
            ir::Expr::SyncFn {
                args: vec![],
                body: vec![ir::Stmt::Return(value.clone())],
            },
        );
        fields.insert(
            "set".into(),
            ir::Expr::SyncFn {
                args: vec![new_value_var.clone()],
                body: vec![ir::Stmt::Assign {
                    assignee: ir::AssigneeExpr::Expr(value),
                    value: ir::Expr::Binding(new_value_var),
                }],
            },
        );
        Ok(ir::Expr::Object { fields })
    }

    fn context_name(&mut self, context_ty: &Type) -> String {
        let type_name = format!("{context_ty}");
        let type_name = escape_ident(&type_name);
        format!(
            "context_{type_name}__{}",
            self.type_ids
                .entry(Hashable(context_ty.clone()))
                .or_insert_with(Id::new)
        )
    }

    async fn read_place(&mut self, place: &PlaceExpr) -> eyre::Result<ir::Expr> {
        let place = self.eval_place(place).await?;
        Ok(ir::Expr::AwaitCall {
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
                        Some(place)
                            if place.read()?.get()?.as_inferred().is_ok_and(|inferred| {
                                !matches!(*inferred, ValueShape::Binding(..))
                            }) =>
                        {
                            // println!("capturing {}", binding.symbol);
                            self.ensure_captured(&place);
                            self.make_ref(ir::Expr::Binding(captured_name(&place)))?
                        }
                        Some(_ /* binding */) | None => {
                            self.make_ref(ir::Expr::binding(binding))?
                        }
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
                            assignee: ir::AssigneeExpr::NewVar(temp_var.clone()),
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
                            assignee: ir::AssigneeExpr::NewVar(var.clone()),
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

    async fn eval_expr_into_stmts(&mut self, expr: &Expr) -> eyre::Result<Vec<ir::Stmt>> {
        let mut stmts = Vec::new();
        self.eval_expr_as_stmts(expr, &mut stmts).await?;
        Ok(stmts)
    }
    fn eval_expr_as_stmts<'a>(
        &'a mut self,
        expr: &'a Expr,
        stmts: &'a mut Vec<ir::Stmt>,
    ) -> BoxFuture<'a, eyre::Result<()>> {
        let r#impl = async move {
            match expr {
                Expr::Ref { .. } => todo!(),
                Expr::And { .. } => todo!(),
                Expr::Or { .. } => todo!(),
                Expr::Assign {
                    assignee,
                    value,
                    data: _,
                } => {
                    let temp_var = ir::Name::unique("var");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::AssigneeExpr::NewVar(temp_var.clone()),
                        value: self.read_place(value).await?,
                    });
                    self.assign(assignee, ir::Expr::Binding(temp_var), stmts)
                        .await?;
                }
                Expr::List { .. } => todo!(),
                Expr::Unwindable { .. } => stmts.push(ir::Stmt::Expr(self.eval_expr(expr).await?)),
                Expr::Unwind {
                    name,
                    value,
                    data: _,
                } => {
                    stmts.push(ir::Stmt::Throw(ir::Expr::Object {
                        fields: {
                            let mut fields = LinkedHashMap::new();
                            fields.insert("handle".into(), self.eval_expr(name).await?);
                            fields.insert("value".into(), self.eval_expr(value).await?);
                            fields
                        },
                    }));
                }
                Expr::InjectContext { context, data: _ } => {
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::AssigneeExpr::Expr(ir::Expr::Index {
                            obj: Box::new(ir::Expr::Binding(ir::Name::context())),
                            index: Box::new(ir::Expr::String(
                                self.context_name(&context.data().ty),
                            )),
                        }),
                        value: self.eval_expr(context).await?,
                    });
                }
                Expr::CurrentContext { data: _ } => todo!(),
                Expr::Unit { data: _ } => {}
                Expr::FunctionType { .. } => todo!(),
                Expr::Cast { .. } => todo!(),
                Expr::Is { .. } => todo!(),
                Expr::Match { .. } => todo!(),
                Expr::Newtype { .. } => todo!(),
                Expr::Variant { .. } => todo!(),
                Expr::MakeMultiset { .. } => todo!(),
                Expr::Use { .. } => {}
                Expr::Recursive { .. } => todo!(),
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data: _,
                } => {
                    self.register_cond_vars(condition, stmts)?;
                    stmts.push(ir::Stmt::If {
                        condition: self.eval_expr(condition).await?,
                        then_case: self.eval_expr_into_stmts(then_case).await?,
                        else_case: match else_case {
                            None => vec![],
                            Some(else_case) => self.eval_expr_into_stmts(else_case).await?,
                        },
                    });
                }
                Expr::Then { list, data: _ } => {
                    for expr in list {
                        self.eval_expr_as_stmts(expr, stmts).await?;
                    }
                }
                Expr::Constant { .. } => todo!(),
                Expr::Number { .. } => todo!(),
                Expr::String { .. } => todo!(),
                Expr::Native { .. } => {
                    stmts.push(ir::Stmt::Expr(self.eval_expr(expr).await?));
                }
                Expr::Let {
                    is_const_let,
                    pattern,
                    value,
                    data: _,
                } => {
                    // TODO huh?
                    if *is_const_let {
                        return Ok(());
                    }
                    let temp_var = ir::Name::unique("let");
                    let value = value.as_ref().expect("no value in let?");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::AssigneeExpr::NewVar(temp_var.clone()),
                        value: self.read_place(value).await?,
                    });
                    self.pattern_match(
                        ir::Expr::Binding(temp_var),
                        pattern,
                        stmts,
                        MatchMode::Assign,
                    )?;
                }
                Expr::Call { .. } => {
                    stmts.push(ir::Stmt::Expr(self.eval_expr(expr).await?));
                }
                Expr::CallMacro { .. } => todo!(),
                Expr::Scope { expr, data: _ } => {
                    let mut body = Vec::new();
                    self.eval_expr_as_stmts(expr, &mut body).await?;
                    stmts.push(ir::Stmt::Scope { body });
                }
                Expr::Function { .. } => todo!(),
                Expr::Template { .. } => todo!(),
                Expr::Instantiate { .. } => todo!(),
                Expr::Tuple { .. } => todo!(),
                Expr::Ast { .. } => todo!(),
                Expr::ReadPlace { .. } => todo!(),
                Expr::TargetDependent { branches, data: _ } => {
                    let body = self
                        .kast
                        .select_target_dependent_branch(branches, self.target.clone())
                        .await?;
                    self.eval_expr_as_stmts(body, stmts).await?;
                }
                Expr::Type { .. } => todo!(),
            }
            Ok::<_, eyre::Report>(())
        };
        async move {
            r#impl
                .await
                .wrap_err_with(|| format!("while transpiling {} to stmt", expr.show_short()))
        }
        .boxed()
    }

    fn eval_expr<'a>(&'a mut self, expr: &'a Expr) -> BoxFuture<'a, eyre::Result<ir::Expr>> {
        let r#impl = async move {
            Ok(match expr {
                Expr::Type { .. } => ir::Expr::Undefined,
                Expr::Ref { place, data: _ } => self.eval_place(place).await?,
                Expr::And { .. } => todo!(),
                Expr::Or { .. } => todo!(),
                Expr::Assign { .. } => ir::Expr::scope(self.eval_expr_into_stmts(expr).await?),
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
                        assignee: ir::AssigneeExpr::NewVar(handle_var.clone()),
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
                Expr::Unwind { .. } => ir::Expr::scope(self.eval_expr_into_stmts(expr).await?),
                Expr::InjectContext { .. } => {
                    ir::Expr::scope(self.eval_expr_into_stmts(expr).await?)
                }
                Expr::CurrentContext { data } => {
                    // println!("{}", &data.ty);
                    ir::Expr::Index {
                        obj: Box::new(ir::Expr::Binding(ir::Name::context())),
                        index: Box::new(ir::Expr::String(self.context_name(&data.ty))),
                    }
                }
                Expr::Unit { data: _ } => ir::Expr::Undefined,
                Expr::FunctionType { .. } => todo!(),
                Expr::Cast { .. } => {
                    let value = self
                        .kast
                        .with_scopes((*expr.data().captured).clone())
                        .eval(expr)
                        .await?;
                    self.transpile(&value).await?
                }
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
                        assignee: ir::AssigneeExpr::NewVar(var.clone()),
                        value: self.read_place(value).await?,
                    });
                    for branch in branches {
                        self.register_pattern_vars(&branch.pattern, &mut stmts)?;
                        stmts.push(ir::Stmt::If {
                            condition: self
                                .pattern_check(ir::Expr::Binding(var.clone()), &branch.pattern)?,
                            then_case: vec![ir::Stmt::Return(self.eval_expr(&branch.body).await?)],
                            else_case: vec![],
                        });
                    }
                    stmts.push(ir::Stmt::throw_error("non exhaustive match"));
                    stmts
                }),
                Expr::Newtype { def: _, data: _ } => ir::Expr::Undefined,
                Expr::Variant {
                    name,
                    value,
                    data: _,
                } => ir::Expr::new_variant(
                    name,
                    match value {
                        None => None,
                        Some(value) => Some(self.eval_expr(value).await?),
                    },
                ),
                Expr::MakeMultiset { .. } => todo!(),
                Expr::Use { .. } => ir::Expr::Undefined,
                Expr::Recursive {
                    body,
                    compiler_scope: _,
                    data: _,
                } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    self.eval_expr_as_stmts(body, &mut stmts).await?;
                    let mut fields = LinkedHashMap::new();
                    expr.collect_bindings(
                        &mut |binding| {
                            fields.insert(
                                binding.symbol.name().to_owned(),
                                ir::Expr::Binding(ir::Name::for_binding(&binding)),
                            );
                        },
                        None,
                    );
                    stmts.push(ir::Stmt::Return(ir::Expr::Object { fields }));
                    stmts
                }),
                Expr::If {
                    condition,
                    then_case,
                    else_case,
                    data: _,
                } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    self.register_cond_vars(condition, &mut stmts)?;
                    stmts.push(ir::Stmt::If {
                        condition: self.eval_expr(condition).await?,
                        then_case: vec![ir::Stmt::Return(self.eval_expr(then_case).await?)],
                        else_case: match else_case {
                            None => vec![],
                            Some(else_case) => {
                                vec![ir::Stmt::Return(self.eval_expr(else_case).await?)]
                            }
                        },
                    });
                    stmts
                }),
                Expr::Then { list, data: _ } => ir::Expr::scope({
                    let mut stmts = Vec::new();
                    for (index, expr) in list.iter().enumerate() {
                        if index == list.len() - 1 {
                            stmts.push(ir::Stmt::Return(self.eval_expr(expr).await?));
                        } else {
                            self.eval_expr_as_stmts(expr, &mut stmts).await?;
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
                    TypeShape::Ref(inner) => {
                        match inner
                            .inferred()
                            .map_err(|_| eyre!("string type not inferred"))?
                        {
                            TypeShape::String => ir::Expr::scope({
                                let mut stmts = Vec::new();
                                let temp = ir::Name::unique("str");
                                stmts.push(ir::Stmt::Assign {
                                    assignee: ir::AssigneeExpr::NewVar(temp.clone()),
                                    value: ir::Expr::String(token.contents.clone()),
                                });
                                stmts.push(ir::Stmt::Return(
                                    self.make_ref(ir::Expr::Binding(temp))?,
                                ));
                                stmts
                            }),
                            other => eyre::bail!("wrong string type: &{other}"),
                        }
                    }
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
                            code = match code[index..].strip_prefix("$(") {
                                Some(code) => code,
                                None => {
                                    write!(result, "$")?;
                                    code = code[index..]
                                        .strip_prefix("$$")
                                        .ok_or_eyre("expected $( or $$")?;
                                    continue;
                                }
                            };
                            let Some(index) = code.find(')') else {
                                eyre::bail!("unclosed $(");
                            };
                            let var = &code[..index];
                            code = code[index..].strip_prefix(')').unwrap();
                            let value = match var {
                                "$ctx" => ir::Expr::Binding(ir::Name::context()),
                                _ => {
                                    let value = compiler_scope
                                        .lookup(var, Hygiene::DefSite, self.kast.spawn_id)
                                        .await
                                        .ok_or_eyre(format!("{var} not found???"))?;
                                    self.transpile(&value).await?
                                }
                            };
                            write!(result, "{}", value.show(ShowOptions::Minified))?;
                        } else {
                            write!(result, "{}", code)?;
                            break;
                        }
                    }
                    ir::Expr::Raw(result)
                }
                Expr::Let { .. } => ir::Expr::scope(self.eval_expr_into_stmts(expr).await?),
                Expr::Call { f, arg, data: _ } => ir::Expr::AwaitCall {
                    f: Box::new(self.eval_expr(f).await?),
                    args: vec![
                        ir::Expr::Binding(ir::Name::context()),
                        self.eval_expr(arg).await?,
                    ],
                },
                Expr::CallMacro { .. } => todo!(),
                Expr::Scope { expr, data: _ } => self.eval_expr(expr).await?,
                Expr::Function {
                    ty: _,
                    compiled,
                    data: _,
                } => self.transpile_compiled_fn(compiled).await?,
                Expr::Template { .. } => todo!(),
                Expr::Instantiate {
                    template,
                    arg,
                    data,
                } => {
                    let captured = &data.captured;
                    let mut kast = self.kast.with_scopes((**captured).clone());
                    let template_fn = kast
                        .eval(template)
                        .await?
                        .into_inferred()?
                        .into_template()?;
                    let arg = kast.eval(arg).await?;
                    let template = kast.await_compiled(&template_fn.compiled).await?;
                    let instantiated_template = kast.monomorphise(&template, arg).await?;
                    // println!("instantiated = {instantiated_template}");
                    let f = self
                        .transpile_fn(&Function {
                            id: Id::new(),
                            span: template_fn.span.clone(),
                            name: template_fn.name.clone(), // TODO ?
                            captured: template_fn.captured.clone(),
                            compiled: crate::executor::Spawned::ready(instantiated_template),
                        })
                        .await?;
                    ir::Expr::AwaitCall {
                        f: Box::new(f),
                        args: vec![ir::Expr::Binding(ir::Name::context())],
                    }
                }
                Expr::Tuple { tuple, data: _ } => ir::Expr::Object {
                    fields: {
                        let mut fields = LinkedHashMap::new();
                        for (member, field) in tuple.iter() {
                            fields.insert(member.to_string(), self.eval_expr(field).await?);
                        }
                        fields
                    },
                },
                Expr::Ast { .. } => todo!(),
                Expr::ReadPlace { place, data: _ } => self.read_place(place).await?,
                Expr::TargetDependent { branches, data: _ } => {
                    let body = self
                        .kast
                        .select_target_dependent_branch(branches, self.target.clone())
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
                ValueShape::Expr(_) => todo!(),
                ValueShape::Type(_) => {
                    ir::Expr::Undefined // TODO
                }
                ValueShape::SyntaxModule(_) => todo!(),
                ValueShape::SyntaxDefinition(_) => todo!(),
                ValueShape::UnwindHandle(_) => todo!(),
                ValueShape::Symbol(_) => todo!(),
                ValueShape::HashMap(_) => todo!(),
                ValueShape::Ref(_) => todo!(),
                ValueShape::Target(_) => todo!(),
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
                    stmts.push(ir::Stmt::Expr(ir::Expr::AwaitCall {
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

    fn register_pattern_vars(
        &self,
        pattern: &Pattern,
        stmts: &mut Vec<ir::Stmt>,
    ) -> eyre::Result<()> {
        match pattern {
            Pattern::Placeholder { data: _ } => {}
            Pattern::Unit { data: _ } => {}
            Pattern::Binding {
                binding,
                bind_mode: _,
                data: _,
            } => stmts.push(ir::Stmt::DefVar(ir::Name::for_binding(binding))),
            Pattern::Tuple { tuple, data: _ } => {
                for field in tuple.values() {
                    self.register_pattern_vars(field, stmts)?;
                }
            }
            Pattern::Variant {
                name: _,
                value,
                data: _,
            } => {
                if let Some(value) = value {
                    self.register_pattern_vars(value, stmts)?;
                }
            }
        }
        Ok(())
    }
    fn register_cond_vars(&self, expr: &Expr, stmts: &mut Vec<ir::Stmt>) -> eyre::Result<()> {
        match expr {
            Expr::Ref { .. } => {}
            Expr::And { lhs, rhs, data: _ } => {
                self.register_cond_vars(lhs, stmts)?;
                self.register_cond_vars(rhs, stmts)?;
            }
            Expr::Or { lhs, rhs, data: _ } => {
                self.register_cond_vars(lhs, stmts)?;
                _ = rhs; // supposed to have all same vars
            }
            Expr::Assign { .. } => {}
            Expr::List { .. } => {}
            Expr::Unwindable { .. } => {}
            Expr::Unwind { .. } => {}
            Expr::InjectContext { .. } => {}
            Expr::CurrentContext { .. } => {}
            Expr::Unit { .. } => {}
            Expr::FunctionType { .. } => {}
            Expr::Cast { .. } => {}
            Expr::Is {
                value: _,
                pattern,
                data: _,
            } => {
                self.register_pattern_vars(pattern, stmts)?;
            }
            Expr::Match { .. } => {}
            Expr::Newtype { .. } => {}
            Expr::Variant { .. } => {}
            Expr::MakeMultiset { .. } => {}
            Expr::Use { .. } => {}
            Expr::Recursive { .. } => {}
            Expr::If { .. } => {}
            Expr::Then { .. } => {}
            Expr::Constant { .. } => {}
            Expr::Number { .. } => {}
            Expr::String { .. } => {}
            Expr::Native { .. } => {}
            Expr::Let { .. } => {}
            Expr::Call { .. } => {}
            Expr::CallMacro { .. } => {}
            Expr::Scope { expr, data: _ } => {
                self.register_cond_vars(expr, stmts)?;
            }
            Expr::Function { .. } => {}
            Expr::Template { .. } => {}
            Expr::Instantiate { .. } => {}
            Expr::Tuple { .. } => {}
            Expr::Ast { .. } => {}
            Expr::ReadPlace { .. } => {}
            Expr::TargetDependent { .. } => todo!(),
            Expr::Type { .. } => {}
        }
        Ok(())
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
                let name = ir::Name::for_binding(binding);
                stmts.push(ir::Stmt::Assign {
                    assignee: match mode {
                        MatchMode::Check => ir::AssigneeExpr::Expr(ir::Expr::Binding(name)),
                        MatchMode::Assign => ir::AssigneeExpr::NewVar(name),
                    },
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
                            vec![ir::Stmt::throw_error("assertion failed")]
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
            self.init_code.push(ir::Stmt::Assign {
                assignee: ir::AssigneeExpr::NewVar(captured_name(&unprocessed)),
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
        // if let Some(name) = self.compiled_fns.get(&compiled) {
        //     return Ok(ir::Expr::Binding(name.clone()));
        // }
        let name = ir::Name::unique("compiled_fn");
        self.compiled_fns.insert(compiled.clone(), name.clone());

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

        // self.captured_code.push(ir::Stmt::Assign {
        //     assignee: ir::AssigneeExpr::NewVar(name.clone()),
        //     value: ir::Expr::Fn { args, body },
        // });
        // Ok(ir::Expr::Binding(name))
        Ok(ir::Expr::AsyncFn { args, body })
    }

    pub fn create_contexts(&mut self) -> ir::Expr {
        let source = match self.engine_type {
            JavaScriptEngineType::Node => include_str!("contexts/node.js"),
            JavaScriptEngineType::Browser => include_str!("contexts/browser.js"),
        };

        ir::Expr::scope({
            let mut stmts = Vec::new();
            let mut contexts = LinkedHashMap::new();
            // TODO more contexts
            stmts.push(ir::Stmt::Raw(format!(
                "const {{ contexts: {{ input, output }}, cleanup }} = await {source}"
            )));
            stmts.push(ir::Stmt::Raw(format!("globalThis.kast_cleanup = cleanup",)));
            contexts.insert(
                self.context_name(&contexts::default_output().ty()),
                ir::Expr::Raw("output".into()),
            );
            contexts.insert(
                self.context_name(&contexts::default_input().ty()),
                ir::Expr::Raw("input".into()),
            );
            self.cleanup_code
                .push(ir::Stmt::Raw(format!("globalThis.kast_cleanup()")));

            stmts.push(ir::Stmt::Return(ir::Expr::Object { fields: contexts }));
            stmts
        })
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

        let result = match &*value.as_inferred()? {
            ValueShape::Expr(expr) => {
                let code = transpiler.eval_expr(expr).await?;
                let code = ir::Expr::AsyncFn {
                    args: vec![ir::Name::context()],
                    body: vec![ir::Stmt::Return(code)],
                };
                ir::Expr::AwaitCall {
                    f: Box::new(code),
                    args: vec![transpiler.create_contexts()],
                }
            }
            _ => transpiler.transpile(value).await?,
        };
        transpiler.process_captured().await?;

        let expr = ir::Expr::SyncCall {
            f: Box::new(ir::Expr::AsyncFn {
                args: vec![],
                body: {
                    let mut stmts = transpiler.init_code;
                    let result_name = ir::Name::unique("result");
                    stmts.push(ir::Stmt::Assign {
                        assignee: ir::AssigneeExpr::NewVar(result_name.clone()),
                        value: result,
                    });
                    stmts.extend(transpiler.cleanup_code);
                    stmts.push(ir::Stmt::Return(ir::Expr::Binding(result_name)));
                    stmts
                },
            }),
            args: vec![],
        };
        let expr = expr.optimize();
        Ok(expr.show(options).to_string())
    }
}
