#![allow(unused)]
use super::*;
use eyre::OptionExt as _;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum JavaScriptEngineType {
    Node,
    Browser,
}

struct Transpiler {
    kast: Kast,
    scopes: Parc<Scopes>,
    engine_type: JavaScriptEngineType,
    code: String,
    captured: std::collections::HashSet<PlaceRef>,
    captured_unprocessed: Vec<PlaceRef>,
}

impl std::fmt::Write for Transpiler {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.code.write_str(s)
    }
}

fn binding_name(binding: &Binding) -> String {
    format!(
        "var{}",
        // TODO escape_ident(&binding.symbol.name),
        binding.symbol.id,
    )
}

impl Transpiler {
    fn new(kast: &mut Kast, engine_type: JavaScriptEngineType) -> Self {
        Self {
            kast: kast.clone(),
            scopes: Parc::new(kast.scopes.clone()),
            engine_type,
            code: String::new(),
            captured: Default::default(),
            captured_unprocessed: Default::default(),
        }
    }

    fn make_ref(&mut self, value: &str) -> eyre::Result<()> {
        let new_value_var = format!("var{}", Id::new());
        write!(
            self,
            "({{\"get\":()=>{value},\"set\":({new_value_var})=>{value}={new_value_var}}})"
        )?;
        Ok(())
    }

    async fn read_place(&mut self, place: &PlaceExpr) -> eyre::Result<()> {
        self.eval_place(place).await?;
        write!(self, ".get()")?;
        Ok(())
    }

    fn eval_place<'a>(&'a mut self, expr: &'a PlaceExpr) -> BoxFuture<'a, eyre::Result<()>> {
        async move {
            match expr {
                PlaceExpr::Binding { binding, data: _ } => {
                    match self.scopes.interpreter.get(&binding.symbol) {
                        Some(place) => {
                            self.ensure_captured(&place);
                            self.make_ref(&format!("captured{}", place.place.id))?;
                        }
                        None => self.make_ref(&binding_name(binding))?,
                    }
                }
                PlaceExpr::FieldAccess {
                    obj,
                    field,
                    data: _,
                } => {
                    self.begin_scope()?;
                    let temp_var = format!("var{}", Id::new());
                    write!(self, "{temp_var}=")?;
                    self.read_place(obj).await?; // The value is a reference type since it has fields
                    write!(self, ";return")?;
                    self.make_ref(&format!("{temp_var}[{:?}]", field.to_string()))?;
                    self.end_scope()?;
                }
                PlaceExpr::Temporary { value, data: _ } => {
                    let var = format!("var{}", Id::new());
                    self.begin_scope()?;
                    write!(self, "{var}=")?;
                    self.eval_expr(value).await?;
                    write!(self, ";return")?;
                    self.make_ref(&var)?;
                    self.end_scope()?;
                }
                PlaceExpr::Deref { r#ref, data: _ } => {
                    self.eval_expr(r#ref).await?;
                }
            }
            Ok(())
        }
        .boxed()
    }

    fn eval_expr<'a>(&'a mut self, expr: &'a Expr) -> BoxFuture<'a, eyre::Result<()>> {
        async move {
            match expr {
                Expr::Ref { place, data } => todo!(),
                Expr::And { lhs, rhs, data } => todo!(),
                Expr::Or { lhs, rhs, data } => todo!(),
                Expr::Assign {
                    assignee,
                    value,
                    data: _,
                } => {
                    let temp_var = format!("var{}", Id::new());
                    write!(self, "{temp_var}=")?;
                    self.read_place(value).await?;
                    write!(self, ";")?;
                    self.assign(assignee, &temp_var).await?;
                }
                Expr::List { values, data } => todo!(),
                Expr::Unwindable { name, body, data } => todo!(),
                Expr::Unwind { name, value, data } => todo!(),
                Expr::InjectContext { context, data } => todo!(),
                Expr::CurrentContext { data } => todo!(),
                Expr::Unit { data: _ } => write!(self, "undefined")?,
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
                    data,
                } => todo!(),
                Expr::Match {
                    value,
                    branches,
                    data,
                } => todo!(),
                Expr::Newtype { def, data } => todo!(),
                Expr::Variant { name, value, data } => todo!(),
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
                } => todo!(),
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
                    is_const_let,
                    pattern,
                    value,
                    data,
                } => todo!(),
                Expr::Call { f, arg, data: _ } => {
                    write!(self, "(")?;
                    self.eval_expr(f).await?;
                    write!(self, ")(")?;
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
                    let body = self
                        .kast
                        .select_target_dependent_branch(branches, Target::JavaScript)
                        .await?;
                    self.eval_expr(body).await?
                }
            }
            Ok(())
        }
        .boxed()
    }

    fn transpile<'a>(&'a mut self, value: &'a Value) -> BoxFuture<'a, eyre::Result<()>> {
        async move {
            let value = value.as_inferred()?;
            let value: &ValueShape = &value;
            match value {
                ValueShape::Unit => write!(self, "undefined")?,
                ValueShape::Bool(value) => write!(self, "{value:?}")?,
                ValueShape::Int32(value) => write!(self, "{value}")?,
                ValueShape::Int64(value) => write!(self, "{value}")?, // TODO two int32s
                ValueShape::Float64(value) => write!(self, "{value}")?, // TODO this might lose precision
                ValueShape::Char(c) => write!(self, "{c:?}")?,
                ValueShape::String(s) => write!(self, "{s:?}")?,
                ValueShape::List(list) => {
                    write!(self, "[")?;
                    for (index, place) in list.values.iter().enumerate() {
                        if index != 0 {
                            write!(self, ",")?;
                        }
                        let value = place.read_value()?;
                        self.transpile(&value).await?;
                    }
                    write!(self, "]")?;
                }
                ValueShape::Tuple(tuple) => {
                    write!(self, "{{")?;
                    for (index, (member, value)) in tuple.values()?.into_iter().enumerate() {
                        if index != 0 {
                            write!(self, ",")?;
                        }
                        write!(self, "{:?}:", member.to_string())?;
                        self.transpile(&value).await?;
                    }
                    write!(self, "}}")?;
                }
                ValueShape::Function(f) => {
                    self.transpile_fn(f).await?;
                }
                ValueShape::Template(t) => {
                    self.transpile_fn(t).await; // TODO it should have memoization
                }
                ValueShape::Macro(_) => todo!(),
                ValueShape::NativeFunction(native_function) => todo!(),
                ValueShape::Binding(binding) => {
                    write!(self, "{}", binding_name(binding))?;
                }
                ValueShape::Variant(variant) => {
                    write!(self, "{{\"variant\":{:?},\"value\":", variant.name)?;
                    match &variant.value {
                        Some(place) => self.transpile(&*place.read_value()?).await?,
                        None => write!(self, "undefined")?,
                    }
                    write!(self, "}}")?;
                }
                ValueShape::Multiset(_) => todo!(),
                ValueShape::Contexts(_) => todo!(),
                ValueShape::Ast(_) => todo!(),
                ValueShape::Expr(expr) => todo!(),
                ValueShape::Type(_) => {
                    write!(self, "undefined")?; // TODO
                }
                ValueShape::SyntaxModule(_) => todo!(),
                ValueShape::SyntaxDefinition(_) => todo!(),
                ValueShape::UnwindHandle(unwind_handle) => todo!(),
                ValueShape::Symbol(symbol) => todo!(),
                ValueShape::HashMap(hash_map) => {
                    let var = "m";
                    self.begin_scope()?;
                    write!(self, "const {var}=new Map();")?;
                    for (key, value) in hash_map.values.iter() {
                        write!(self, "{var}.set(")?;
                        self.transpile(&key.0).await?;
                        write!(self, ",")?;
                        self.transpile(&*value.read_value()?).await?;
                        write!(self, ");")?;
                    }
                    write!(self, "return {var}")?;
                    self.end_scope()?;
                }
                ValueShape::Ref(place_ref) => todo!(),
                ValueShape::Target(target) => todo!(),
            }
            Ok(())
        }
        .boxed()
    }

    fn assign<'a>(
        &'a mut self,
        assignee: &'a AssigneeExpr,
        value: &'a str,
    ) -> BoxFuture<'a, eyre::Result<()>> {
        async move {
            match assignee {
                AssigneeExpr::Placeholder { data: _ } => {}
                AssigneeExpr::Unit { data: _ } => {}
                AssigneeExpr::Tuple { tuple, data: _ } => {
                    for (index, (member, field_assignee)) in tuple.as_ref().into_iter().enumerate()
                    {
                        if index != 0 {
                            write!(self, ";")?;
                        }
                        self.assign(
                            field_assignee,
                            &format!("{value}[{:?}]", member.to_string()),
                        )
                        .await?;
                    }
                }
                AssigneeExpr::Place { place, data: _ } => {
                    self.eval_place(place).await?;
                    write!(self, ".set({value})")?;
                }
                AssigneeExpr::Let { pattern, data: _ } => self.pattern_match(value, pattern)?,
            }
            Ok(())
        }
        .boxed()
    }

    fn pattern_match(&mut self, value: &str, pattern: &Pattern) -> eyre::Result<()> {
        match pattern {
            Pattern::Placeholder { data: _ } => {}
            Pattern::Unit { data: _ } => {}
            Pattern::Binding {
                binding,
                bind_mode: _, // TODO maybe we care?
                data: _,
            } => {
                let binding_name = binding_name(binding);
                write!(self, "{binding_name}={value}")?
            }
            Pattern::Tuple { tuple, data: _ } => {
                for (index, (member, field_pattern)) in tuple.as_ref().into_iter().enumerate() {
                    if index != 0 {
                        write!(self, ";")?;
                    }
                    self.pattern_match(
                        &format!("{value}[{:?}]", member.to_string()),
                        field_pattern,
                    )?;
                }
            }
            Pattern::Variant {
                name,
                value: value_pattern,
                data: _,
            } => {
                write!(
                    self,
                    "if({value}[\"variant\"] != {name:?})throw \"assertion failed\";",
                )?;
                if let Some(value_pattern) = value_pattern {
                    self.pattern_match(&format!("{value}[\"value\"]"), value_pattern)?;
                }
            }
        }
        Ok(())
    }

    fn begin_scope(&mut self) -> eyre::Result<()> {
        write!(self, "(()=>{{")?;
        Ok(())
    }

    fn end_scope(&mut self) -> eyre::Result<()> {
        write!(self, "}})()")?;
        Ok(())
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

    async fn transpile_fn(&mut self, f: &Function) -> eyre::Result<()> {
        let old_scopes = std::mem::replace(&mut self.scopes, f.captured.clone());
        self.transpile_compiled_fn(&f.compiled).await?;
        self.scopes = old_scopes;
        Ok(())
    }

    async fn transpile_compiled_fn(&mut self, f: &MaybeCompiledFn) -> eyre::Result<()> {
        let arg = format!("arg{}", Id::new());
        write!(self, "({arg})=>{{")?;
        let compiled = self.kast.await_compiled(f).await?;
        self.pattern_match(&arg, &compiled.arg)?;
        write!(self, ";return ")?;
        self.eval_expr(&compiled.body).await?;
        write!(self, "}}")?;
        Ok(())
    }
}

impl Kast {
    pub async fn transpile_to_javascript(
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
