use std::collections::BTreeMap;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple<T> {
    unnamed: Vec<T>,
    named: BTreeMap<String, T>,
    named_order: Vec<String>,
}

impl<T> Tuple<T> {
    pub fn empty() -> Self {
        Self {
            unnamed: vec![],
            named: BTreeMap::new(),
            named_order: vec![],
        }
    }
    pub fn is_empty(&self) -> bool {
        self.unnamed.is_empty() && self.named.is_empty()
    }
    pub fn add_unnamed(&mut self, value: T) {
        self.unnamed.push(value);
    }
    pub fn add_named(&mut self, name: String, value: T) {
        self.named_order.push(name.clone());
        self.named.insert(name, value);
    }
    pub fn fmt_with_name<'a>(&'a self, name: &'a str) -> NamedTupleFmt<'a, T> {
        NamedTupleFmt { name, tuple: self }
    }
    pub fn unnamed(&self) -> impl Iterator<Item = &T> + '_ {
        self.unnamed.iter()
    }
    pub fn named(&self) -> impl Iterator<Item = (&str, &T)> + '_ {
        self.named_order
            .iter()
            .map(|key| (key.as_str(), &self.named[key]))
    }
    pub fn into_field_values(mut self) -> impl Iterator<Item = T> {
        self.unnamed.into_iter().chain(
            self.named_order
                .into_iter()
                .map(move |name| self.named.remove(&name).unwrap()),
        )
    }
    pub fn show_fields(&self) -> impl std::fmt::Display + '_ {
        struct ShowFields<'a, T>(&'a Tuple<T>);
        impl<T> std::fmt::Display for ShowFields<'_, T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut first = true;
                if !self.0.unnamed.is_empty() {
                    write!(f, "{} unnamed fields", self.0.unnamed.len())?;
                    first = false;
                }
                for name in &self.0.named_order {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name:?}")?;
                }
                Ok(())
            }
        }
        ShowFields(self)
    }
    pub fn as_ref(&self) -> Tuple<&T> {
        Tuple {
            unnamed: self.unnamed.iter().collect(),
            named: self
                .named
                .iter()
                .map(|(key, value)| (key.clone(), value))
                .collect(),
            named_order: self.named_order.clone(),
        }
    }
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Tuple<U> {
        Tuple {
            unnamed: self.unnamed.into_iter().map(&f).collect(),
            named: self
                .named
                .into_iter()
                .map(|(key, value)| (key, f(value)))
                .collect(),
            named_order: self.named_order,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum NamedErrorReason {
    UnnamedFieldsPresent,
    FieldNotPresent(String),
    FieldUnexpected(String),
}

impl std::fmt::Display for NamedErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedErrorReason::UnnamedFieldsPresent => write!(f, "no unnamed fields were expected"),
            NamedErrorReason::FieldNotPresent(name) => write!(f, "field {name:?} not present"),
            NamedErrorReason::FieldUnexpected(name) => write!(f, "field {name:?} was not expected"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub struct NamedError {
    erased_value: Tuple<()>,
    expected_fields: Vec<String>,
    #[source]
    reason: NamedErrorReason,
}

impl std::fmt::Display for NamedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected only named fields {:?}, got {}",
            self.expected_fields,
            self.erased_value.show_fields(),
        )
    }
}

impl<T> Tuple<T> {
    pub fn into_named<const N: usize>(mut self, names: [&str; N]) -> Result<[T; N], NamedError> {
        macro_rules! error {
            ($reason:expr) => {
                Err(NamedError {
                    expected_fields: names.into_iter().map(String::from).collect(),
                    reason: $reason,
                    erased_value: self.map(|_| ()),
                })
            };
        }
        if !self.unnamed.is_empty() {
            return error!(NamedErrorReason::UnnamedFieldsPresent);
        }
        if let Some(name) = names.iter().find(|&&name| !self.named.contains_key(name)) {
            return error!(NamedErrorReason::FieldNotPresent(name.to_string()));
        }
        if self.named.iter().len() != N {
            let name = self
                .named_order
                .iter()
                .find(|&name| !names.iter().any(|s| s == name))
                .unwrap();
            return error!(NamedErrorReason::FieldUnexpected(name.to_string()));
        }
        Ok(names.map(|name| self.named.remove(name).unwrap()))
    }
    pub fn into_single_named(self, name: &str) -> Result<T, NamedError> {
        self.into_named([name]).map(|[value]| value)
    }
}

pub enum TupleZipError {
    DifferentUnnamedAmount(usize, usize),
    NamedNotPresentInOther(String),
    NamedOnlyPresentInOther(String),
}

impl<T> Tuple<T> {
    pub fn zip<U>(mut self, mut other: Tuple<U>) -> Result<Tuple<(T, U)>, TupleZipError> {
        if self.unnamed.len() != other.unnamed.len() {
            return Err(TupleZipError::DifferentUnnamedAmount(
                self.unnamed.len(),
                other.unnamed.len(),
            ));
        }
        for name in self.named.keys() {
            if !other.named.contains_key(name) {
                return Err(TupleZipError::NamedNotPresentInOther(name.to_owned()));
            }
        }
        for name in other.named.keys() {
            if !self.named.contains_key(name) {
                return Err(TupleZipError::NamedOnlyPresentInOther(name.to_owned()));
            }
        }
        Ok(Tuple {
            unnamed: self.unnamed.into_iter().zip(other.unnamed).collect(),
            named: self
                .named_order
                .iter()
                .map(|name| {
                    (
                        name.to_owned(),
                        (
                            self.named.remove(name).unwrap(),
                            other.named.remove(name).unwrap(),
                        ),
                    )
                })
                .collect(),
            named_order: self.named_order,
        })
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Tuple<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        {
            if !self.is_empty() {
                writeln!(f)?;
            }
            let mut f = pad_adapter::PadAdapter::new(f);
            for field in &self.unnamed {
                writeln!(f, "{field}")?;
            }
            for name in &self.named_order {
                let field = &self.named[name];
                writeln!(f, "{name}: {field}")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

pub struct NamedTupleFmt<'a, T> {
    name: &'a str,
    tuple: &'a Tuple<T>,
}

impl<T: std::fmt::Display> std::fmt::Display for NamedTupleFmt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.tuple)
    }
}
