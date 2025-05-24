use super::*;

#[derive(Clone)]
pub enum NamePart {
    File(PathBuf),
    Str(String),
    Symbol(Symbol),
    Instantiate(Value),
    ImplCast { value: Value, target: Value },
    Tbd(Parc<Mutex<Option<NamePart>>>),
}

impl NamePart {
    pub fn tbd() -> Self {
        Self::Tbd(Parc::new(Mutex::new(None)))
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Name(Parc<Vec<NamePart>>);

impl Name {
    pub fn unknown() -> Self {
        Self::new(NamePart::tbd())
    }
    pub fn new(only_part: NamePart) -> Self {
        Self(Parc::new(vec![only_part]))
    }
    pub fn append(&self, part: NamePart) -> Self {
        let mut parts: Vec<NamePart> = (*self.0).clone();
        parts.push(part);
        Self(Parc::new(parts))
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
