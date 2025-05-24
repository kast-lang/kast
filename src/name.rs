use super::*;

#[derive(Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub enum NamePart {
    File(PathBuf),
    Str(String),
    Symbol(Symbol),
    Instantiate(#[try_hash] Value),
    ImplCast {
        #[try_hash]
        value: Value,
        #[try_hash]
        target: Value,
    },
}

impl SubstituteBindings for NamePart {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        match self {
            NamePart::File(_) | NamePart::Str(_) | NamePart::Symbol(_) => self,
            NamePart::Instantiate(value) => {
                NamePart::Instantiate(value.substitute_bindings(kast, cache))
            }
            NamePart::ImplCast { value, target } => NamePart::ImplCast {
                value: value.substitute_bindings(kast, cache),
                target: target.substitute_bindings(kast, cache),
            },
        }
    }
}

#[derive(Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(#[try_hash] std::sync::Arc<Vec<NamePart>>);

impl Name {
    pub fn short(&self) -> impl std::fmt::Display + '_ {
        self.0
            .iter()
            .rfind(|part| match part {
                NamePart::Instantiate(_) => false,
                NamePart::ImplCast { .. } => true,
                NamePart::File(_) => true,
                NamePart::Str(_) => true,
                NamePart::Symbol(_) => true,
            })
            .unwrap()
    }
}

impl Inferrable for Name {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        if a == b {
            Ok(a)
        } else {
            eyre::bail!("{a} != {b}")
        }
    }
}

impl SubstituteBindings for Name {
    type Target = Self;
    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
        let substituted = self
            .0
            .iter()
            .cloned()
            .map(|part| part.substitute_bindings(kast, cache))
            .collect();
        Self(std::sync::Arc::new(substituted))
    }
}

impl Name {
    pub fn new(only_part: NamePart) -> Self {
        Self(std::sync::Arc::new(vec![only_part]))
    }
    pub fn append(&self, part: NamePart) -> Self {
        let mut parts: Vec<NamePart> = (*self.0).clone();
        parts.push(part);
        Self(parts.into())
    }
}

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl std::fmt::Display for NamePart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamePart::File(path_buf) => {
                if let Some(name) = path_buf.file_name().and_then(std::ffi::OsStr::to_str) {
                    write!(f, "{name}")?;
                } else {
                    write!(f, "_")?;
                }
            }
            NamePart::Str(s) => write!(f, "{s}")?,
            NamePart::Symbol(symbol) => write!(f, "{symbol}")?,
            NamePart::Instantiate(value) => write!(f, "[{value}]")?,
            NamePart::ImplCast { value, target } => {
                write!(f, "(impl {value} as {target})")?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for part in self.0.iter() {
            if !first && !matches!(part, NamePart::Instantiate { .. }) {
                write!(f, ".")?;
            }
            first = false;
            write!(f, "{part}")?;
        }
        if first {
            write!(f, "<unonymous>")?;
        }
        Ok(())
    }
}
