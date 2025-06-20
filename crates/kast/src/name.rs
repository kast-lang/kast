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

#[async_trait]
impl Inferrable for NamePart {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        use NamePart::*;
        macro_rules! fail {
            () => {
                eyre::bail!("expected name part {a}, got {b}")
            };
        }
        Ok(match (a.clone(), b.clone()) {
            (File(a), File(b)) if a == b => File(a),
            (File(_), _) => fail!(),
            (Str(a), Str(b)) if a == b => Str(a),
            (Str(_), _) => fail!(),
            (Symbol(a), Symbol(b)) if a == b => Symbol(a),
            (Symbol(_), _) => fail!(),
            (Instantiate(a), Instantiate(b)) => Instantiate(Inferrable::make_same(a, b)?),
            (Instantiate { .. }, _) => fail!(),
            (
                ImplCast {
                    value: a_value,
                    target: a_target,
                },
                ImplCast {
                    value: b_value,
                    target: b_target,
                },
            ) => ImplCast {
                value: Inferrable::make_same(a_value, b_value)?,
                target: Inferrable::make_same(a_target, b_target)?,
            },
            (ImplCast { .. }, _) => fail!(),
        })
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        match self {
            NamePart::File(_) => {}
            NamePart::Str(_) => {}
            NamePart::Symbol(_) => {}
            NamePart::Instantiate(value) => {
                value.await_fully_inferred(cache).await?;
            }
            NamePart::ImplCast { value, target } => {
                value.await_fully_inferred(cache).await?;
                target.await_fully_inferred(cache).await?;
            }
        }
        Ok(())
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

#[async_trait]
impl Inferrable for Name {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let error_context = format!("name {a} != {b}");
        let parts = |this: Self| -> Vec<NamePart> { (*this.0).clone() };
        Ok(Self(std::sync::Arc::new(
            Inferrable::make_same(parts(a), parts(b)).wrap_err(error_context)?,
        )))
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        self.0.await_fully_inferred(cache).await
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
