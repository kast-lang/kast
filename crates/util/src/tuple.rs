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
