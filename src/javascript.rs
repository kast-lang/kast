use super::*;
use eyre::OptionExt as _;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum JavaScriptEngineType {
    Node,
    Browser,
}

struct Transpiler {
    engine_type: JavaScriptEngineType,
    code: String,
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
    fn new(engine_type: JavaScriptEngineType) -> Self {
        Self {
            engine_type,
            code: String::new(),
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

    fn read_place(&mut self, place: &PlaceExpr) -> eyre::Result<()> {
        self.eval_place(place)?;
        write!(self, ".get()")?;
        Ok(())
    }

    fn eval_place(&mut self, expr: &PlaceExpr) -> eyre::Result<()> {
        match expr {
            PlaceExpr::Binding { binding, data: _ } => self.make_ref(&binding_name(binding))?,
            PlaceExpr::FieldAccess {
                obj,
                field,
                data: _,
            } => {
                self.begin_scope()?;
                let temp_var = format!("var{}", Id::new());
                write!(self, "{temp_var}=")?;
                self.read_place(obj)?; // The value is a reference type since it has fields
                write!(self, ";return")?;
                self.make_ref(&format!("{temp_var}[{:?}]", field.to_string()))?;
                self.end_scope()?;
            }
            PlaceExpr::Temporary { value, data: _ } => {
                let var = format!("var{}", Id::new());
                self.begin_scope()?;
                write!(self, "{var}=")?;
                self.eval_expr(value)?;
                write!(self, ";return")?;
                self.make_ref(&var)?;
                self.end_scope()?;
            }
            PlaceExpr::Deref { r#ref, data: _ } => {
                self.eval_expr(r#ref)?;
            }
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> eyre::Result<()> {
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
                self.eval_place(value)?;
                write!(self, ";")?;
                self.assign(assignee, &temp_var)?;
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
                        self.eval_expr(expr)?;
                    } else {
                        self.eval_expr(expr)?;
                        write!(self, ";")?;
                    }
                }
                self.end_scope()?;
            }
            Expr::Constant { value, data: _ } => self.transpile(value)?,
            Expr::Number { raw, data: _ } => {
                write!(self, "{raw}")?; // TODO
            }
            Expr::String { token, data: _ } => write!(self, "{:?}", token.contents)?,
            Expr::Native { name, data: _ } => {
                write!(self, "{name}")?;
            }
            Expr::Let {
                is_const_let,
                pattern,
                value,
                data,
            } => todo!(),
            Expr::Call { f, arg, data: _ } => {
                write!(self, "(")?;
                self.eval_expr(f)?;
                write!(self, ")(")?;
                self.eval_expr(arg)?;
                write!(self, ")")?;
            }
            Expr::CallMacro { r#macro, arg, data } => todo!(),
            Expr::Scope { expr, data: _ } => {
                self.eval_expr(expr)?; // TODO
            }
            Expr::Function { ty, compiled, data } => todo!(),
            Expr::Template { compiled, data } => todo!(),
            Expr::Instantiate {
                template,
                arg,
                data,
            } => todo!(),
            Expr::Tuple { tuple, data: _ } => {
                write!(self, "({{")?;
                for (index, (member, field)) in tuple.as_ref().into_iter().enumerate() {
                    if index != 0 {
                        write!(self, ",")?;
                    }
                    write!(self, "{:?}:", member.to_string())?;
                    self.eval_expr(field)?;
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
                self.read_place(place)?;
            }
        }
        Ok(())
    }

    fn transpile(&mut self, value: &Value) -> eyre::Result<()> {
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
                    self.transpile(&value)?;
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
                    self.transpile(&value)?;
                }
                write!(self, "}}")?;
            }
            ValueShape::Function(f) => {
                let arg = format!("arg{}", Id::new());
                write!(self, "({arg})=>{{")?;
                let compiled =
                    f.f.compiled
                        .get()
                        .now_or_never()
                        .ok_or_eyre("function not compiled yet")??;
                self.pattern_match(&arg, &compiled.arg)?;
                write!(self, ";return ")?;
                self.eval_expr(&compiled.body)?;
                write!(self, "}}")?;
            }
            ValueShape::Template(_) => todo!(),
            ValueShape::Macro(_) => todo!(),
            ValueShape::NativeFunction(native_function) => todo!(),
            ValueShape::Binding(_) => todo!(),
            ValueShape::Variant(variant) => {
                write!(self, "{{\"variant\":{:?},\"value\":", variant.name)?;
                match &variant.value {
                    Some(place) => self.transpile(&*place.read_value()?)?,
                    None => write!(self, "undefined")?,
                }
                write!(self, "}}")?;
            }
            ValueShape::Multiset(_) => todo!(),
            ValueShape::Contexts(_) => todo!(),
            ValueShape::Ast(_) => todo!(),
            ValueShape::Expr(expr) => todo!(),
            ValueShape::Type(_) => todo!(),
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
                    self.transpile(&key.0)?;
                    write!(self, ",")?;
                    self.transpile(&*value.read_value()?)?;
                    write!(self, ");")?;
                }
                write!(self, "return {var}")?;
                self.end_scope()?;
            }
            ValueShape::Ref(place_ref) => todo!(),
        }
        Ok(())
    }

    fn assign(&mut self, assignee: &AssigneeExpr, value: &str) -> eyre::Result<()> {
        match assignee {
            AssigneeExpr::Placeholder { data: _ } => {}
            AssigneeExpr::Unit { data: _ } => {}
            AssigneeExpr::Tuple { tuple, data: _ } => {
                for (index, (member, field_assignee)) in tuple.as_ref().into_iter().enumerate() {
                    if index != 0 {
                        write!(self, ";")?;
                    }
                    self.assign(
                        field_assignee,
                        &format!("{value}[{:?}]", member.to_string()),
                    )?;
                }
            }
            AssigneeExpr::Place { place, data: _ } => {
                self.eval_place(place)?;
                write!(self, ".set({value})")?;
            }
            AssigneeExpr::Let { pattern, data: _ } => self.pattern_match(value, pattern)?,
        }
        Ok(())
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
}

impl Kast {
    pub fn transpile_to_javascript(
        &mut self,
        engine_type: JavaScriptEngineType,
        value: &Value,
    ) -> eyre::Result<String> {
        let mut transpiler = Transpiler::new(engine_type);
        transpiler.transpile(value)?;
        Ok(transpiler.code)
    }
}
