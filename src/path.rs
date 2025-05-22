use super::*;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Name(Parc<Mutex<Option<Symbol>>>);

impl Name {
    pub fn unknown() -> Self {
        Self(Parc::new(Mutex::new(None)))
    }
    pub fn name_if_needed(&self, name: &Symbol) {
        let mut this = self.0.lock().unwrap();
        if this.is_none() {
            *this = Some(name.clone());
        }
    }
}

fn show_path(f: &mut std::fmt::Formatter<'_>, scope: &CompilerScope) -> std::fmt::Result {
    let scope_name = scope.name().0.lock().unwrap().clone();
    if let Some(scope_name) = scope_name {
        show_path(f, &scope_name.scope)?;
        write!(f, "{scope_name}.")
    } else if let Some(parent) = scope.parent() {
        show_path(f, &parent)?;
        // write!(f, "{}.", scope.id())?;
        Ok(())
    } else {
        Ok(())
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self.0.lock().unwrap() {
            Some(ref name) => {
                show_path(f, &name.scope)?;
                write!(f, "{name}")
            }
            _ => write!(f, "_"),
        }
    }
}
