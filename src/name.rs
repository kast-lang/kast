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
    Tbd(Parc<Mutex<Option<NamePart>>>),
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
            NamePart::Tbd(tbd) => todo!(),
        }
    }
}

impl NamePart {
    pub fn tbd() -> Self {
        Self::Tbd(Parc::new(Mutex::new(None)))
    }
    pub fn is_known(&self) -> bool {
        match self {
            NamePart::Tbd(tbd) => tbd
                .lock()
                .unwrap()
                .as_ref()
                .is_some_and(|part| part.is_known()),
            _ => true,
        }
    }
}

#[derive(Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(#[try_hash] std::sync::Arc<Vec<NamePart>>);

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
    pub fn unknown() -> Self {
        Self::new(NamePart::tbd())
    }
    pub fn new(only_part: NamePart) -> Self {
        Self(std::sync::Arc::new(vec![only_part]))
    }
    pub fn append(&self, part: NamePart) -> Self {
        let mut parts: Vec<NamePart> = (*self.0).clone();
        parts.push(part);
        Self(parts.into())
    }
    pub fn is_known(&self) -> bool {
        self.0.iter().all(|part| part.is_known())
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
            NamePart::Tbd(tbd) => {
                if let Some(part) = &*tbd.lock().unwrap() {
                    write!(f, "{part}")?;
                } else {
                    write!(f, "_")?;
                }
            }
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
            if let NamePart::Tbd(tbd) = part {
                if tbd.lock().unwrap().is_none() {
                    continue;
                }
            }
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
