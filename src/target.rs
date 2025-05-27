use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    Interpreter,
    JavaScript,
}

impl Target {
    pub fn get_field(&self, field: &tuple::Member<'static>) -> eyre::Result<Value> {
        let Some(field) = field.as_ref().into_name() else {
            eyre::bail!("target has only named fields");
        };
        let field: &str = &field;
        Ok(match field {
            "name" => {
                let name = format!("{self:?}").to_lowercase();
                ValueShape::String(name).into()
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

impl Kast {
    pub async fn select_target_dependent_branch<'a, T: std::fmt::Display>(
        &self,
        branches: &'a [TargetDependentBranch<T>],
        target: Target,
    ) -> eyre::Result<&'a T> {
        let scopes = &self.cache.target_dependent_scopes.clone();
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
