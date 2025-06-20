use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    Interpreter,
    JavaScript {
        engine: javascript::JavaScriptEngineType,
    },
}

impl Target {
    pub fn get_field(&self, field: &tuple::Member<'static>) -> eyre::Result<Value> {
        let Some(field) = field.as_ref().into_name() else {
            eyre::bail!("target has only named fields");
        };
        let field: &str = &field;
        Ok(match field {
            "name" => {
                let name = match self {
                    Target::Interpreter => "interpreter",
                    Target::JavaScript { .. } => "javascript",
                };
                // println!("name={name:?}");
                ValueShape::String(name.to_owned()).into()
            }
            "is_web" => {
                let is_web = match self {
                    Target::JavaScript {
                        engine: javascript::JavaScriptEngineType::Browser,
                    } => true,
                    _ => false,
                };
                ValueShape::Bool(is_web).into()
            }
            _ => eyre::bail!("target doesnt have field {field:?}"),
        })
    }
    pub fn field_ty(field: &tuple::Member<'static>) -> eyre::Result<Type> {
        let Some(field) = field.as_ref().into_name() else {
            eyre::bail!("target has only named fields");
        };
        let field: &str = &field;
        Ok(match field {
            "name" => TypeShape::String.into(),
            "is_web" => TypeShape::Bool.into(),
            _ => eyre::bail!("target doesnt have field {field:?}"),
        })
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, derive_macros::ExprDisplay)]
pub struct TargetDependentBranch<T: std::fmt::Display> {
    pub condition: Expr,
    pub body: T,
}

impl<T: std::fmt::Display + SubstituteBindings<Target = T>> SubstituteBindings
    for TargetDependentBranch<T>
{
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self::Target {
        Self {
            condition: self.condition.substitute_bindings(kast, cache),
            body: self.body.substitute_bindings(kast, cache),
        }
    }
}

impl Kast {
    pub async fn select_target_dependent_branch<'a, T: std::fmt::Display>(
        &self,
        branches: &'a [TargetDependentBranch<T>],
        target: Target,
    ) -> eyre::Result<&'a T> {
        // println!("selecting {target:?}");
        let scopes = Scopes::new(
            self.spawn_id,
            ScopeType::NonRecursive,
            Some(self.cache.target_dependent_scopes.clone()),
        );
        scopes.interpreter.insert(
            &self.cache.target_symbol,
            ValueShape::Target(target).into(),
            Mutability::ReadOnly,
        );
        for branch in branches {
            let condition: Value = self
                .with_scopes(scopes.clone())
                .enter_scope()
                .eval(&branch.condition)
                .await?;
            let condition = condition.into_inferred()?.into_bool()?;
            if condition {
                return Ok(&branch.body);
            }
        }
        eyre::bail!("no matching target branch");
    }
}
