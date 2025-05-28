use std::collections::BTreeMap;
use std::fmt::Write;

use crate::pad_adapter;
use try_hash::TryHash;

#[derive(Debug, Clone)]
pub struct Tuple<T> {
    unnamed: Vec<T>,
    named: BTreeMap<String, T>,
    named_order: Vec<String>,
}

impl<T: PartialEq> PartialEq for Tuple<T> {
    fn eq(&self, other: &Self) -> bool {
        self.unnamed == other.unnamed && self.named == other.named
    }
}

impl<T: Eq> Eq for Tuple<T> {}

impl<T: PartialOrd> PartialOrd for Tuple<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.unnamed.partial_cmp(&other.unnamed) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.named.partial_cmp(&other.named)
    }
}

impl<T: Ord> Ord for Tuple<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.unnamed.cmp(&other.unnamed) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.named.cmp(&other.named)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Tuple<T> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        for unnamed in &self.unnamed {
            unnamed.hash(hasher);
        }
        for (name, value) in &self.named {
            std::hash::Hash::hash(name, hasher);
            value.hash(hasher);
        }
    }
}

impl<T: TryHash> TryHash for Tuple<T> {
    type Error = T::Error;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        for unnamed in &self.unnamed {
            unnamed.try_hash(hasher)?;
        }
        for (name, value) in &self.named {
            std::hash::Hash::hash(name, hasher);
            value.try_hash(hasher)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Member<'a> {
    Unnamed(usize),
    Named(std::borrow::Cow<'a, str>),
}

impl<'a> Member<'a> {
    pub fn into_name(self) -> Option<std::borrow::Cow<'a, str>> {
        match self {
            Member::Unnamed(_) => None,
            Member::Named(name) => Some(name),
        }
    }
}

impl std::fmt::Display for Member<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Member::Unnamed(index) => write!(f, "{index}"),
            Member::Named(ident) => write!(f, "{ident}"),
        }
    }
}

impl Member<'_> {
    pub fn into_owned(self) -> Member<'static> {
        match self {
            Member::Unnamed(index) => Member::Unnamed(index),
            Member::Named(ident) => Member::Named(std::borrow::Cow::Owned(ident.into_owned())),
        }
    }
    pub fn as_ref(&self) -> Member<'_> {
        match *self {
            Member::Unnamed(index) => Member::Unnamed(index),
            Member::Named(ref ident) => Member::Named(std::borrow::Cow::Borrowed(ident)),
        }
    }
}

impl<T> Tuple<T> {
    pub fn empty() -> Self {
        Self {
            unnamed: vec![],
            named: BTreeMap::new(),
            named_order: vec![],
        }
    }
    pub fn single_named(name: impl Into<String>, value: T) -> Self {
        let mut result = Self::empty();
        result.add_named(name, value);
        result
    }
    pub fn is_empty(&self) -> bool {
        self.unnamed.is_empty() && self.named.is_empty()
    }
    pub fn get<'a>(&'a self, member: Member<'_>) -> Option<&'a T> {
        match member {
            Member::Unnamed(index) => self.get_unnamed(index),
            Member::Named(ident) => self.get_named(ident.as_ref()),
        }
    }
    pub fn get_unnamed(&self, index: usize) -> Option<&T> {
        self.unnamed.get(index)
    }
    pub fn get_named(&self, name: &str) -> Option<&T> {
        self.named.get(name)
    }
    pub fn take_named(&mut self, name: &str) -> Option<T> {
        let value = self.named.remove(name);
        if value.is_some() {
            self.named_order.retain(|s| s != name);
        }
        value
    }
    pub fn add_unnamed(&mut self, value: T) {
        assert_eq!(self.named.len(), 0);
        self.unnamed.push(value);
    }
    pub fn add_named(&mut self, name: impl Into<String>, value: T) {
        let name: String = name.into();
        if self.named.insert(name.clone(), value).is_none() {
            self.named_order.push(name.clone());
        }
    }
    pub fn add(&mut self, name: Option<String>, value: T) {
        match name {
            Some(name) => self.add_named(name, value),
            None => self.add_unnamed(value),
        }
    }
    pub fn add_member(&mut self, member: Member<'_>, value: T) {
        match member {
            Member::Unnamed(index) => {
                assert_eq!(self.unnamed.len(), index);
                self.add_unnamed(value);
            }
            Member::Named(name) => self.add_named(name, value),
        }
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
    pub fn try_map<U, E>(self, mut f: impl FnMut(T) -> Result<U, E>) -> Result<Tuple<U>, E> {
        Ok(Tuple {
            unnamed: self
                .unnamed
                .into_iter()
                .map(&mut f)
                .collect::<Result<Vec<_>, _>>()?,
            named: self
                .named
                .into_iter()
                .map(|(key, value)| Ok((key, f(value)?)))
                .collect::<Result<BTreeMap<_, _>, _>>()?,
            named_order: self.named_order,
        })
    }
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Tuple<U> {
        Tuple {
            unnamed: self.unnamed.into_iter().map(&mut f).collect(),
            named: self
                .named
                .into_iter()
                .map(|(key, value)| (key, f(value)))
                .collect(),
            named_order: self.named_order,
        }
    }
    pub fn values(&self) -> impl Iterator<Item = &T> + '_ {
        self.unnamed
            .iter()
            .chain(self.named_order.iter().map(|name| &self.named[name]))
    }
}

pub struct TupleIntoIter<T> {
    unnamed: std::iter::Enumerate<<Vec<T> as IntoIterator>::IntoIter>,
    names: <Vec<String> as IntoIterator>::IntoIter,
    named: BTreeMap<String, T>,
}

impl<T> Iterator for TupleIntoIter<T> {
    type Item = (Member<'static>, T);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some((index, value)) = self.unnamed.next() {
            return Some((Member::Unnamed(index), value));
        }
        let name = self.names.next()?;
        let value = self.named.remove(&name).unwrap();
        Some((Member::Named(name.into()), value))
    }
}

impl<T> IntoIterator for Tuple<T> {
    type Item = (Member<'static>, T);
    type IntoIter = TupleIntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        TupleIntoIter {
            unnamed: self.unnamed.into_iter().enumerate(),
            names: self.named_order.into_iter(),
            named: self.named,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum IntoFixedErrorReason {
    IncorrectAmountOfUnnamedFields { expected: usize, actual: usize },
    NamedFieldNotPresent(String),
    NamedFieldUnexpected(String),
}

impl std::fmt::Display for IntoFixedErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NamedFieldNotPresent(name) => {
                write!(f, "field {name:?} is not present")
            }
            Self::NamedFieldUnexpected(name) => {
                write!(f, "field {name:?} was not expected")
            }
            Self::IncorrectAmountOfUnnamedFields { expected, actual } => {
                write!(f, "expected {expected} unnamed fields, got {actual}")
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub struct IntoFixedError {
    erased_value: Tuple<()>,
    expected_unnamed_fields: usize,
    expected_named_fields: Vec<String>,
    optional_fields: Vec<String>,
    #[source]
    reason: IntoFixedErrorReason,
}

impl std::fmt::Display for IntoFixedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected ")?;
        let mut started = false;
        let mut start = |f: &mut std::fmt::Formatter| {
            if started {
                write!(f, ", ")
            } else {
                started = true;
                Ok(())
            }
        };
        if self.expected_unnamed_fields != 0 {
            start(f)?;
            write!(f, "{} unnamed fields", self.expected_unnamed_fields)?;
        }
        for name in &self.expected_named_fields {
            start(f)?;
            write!(f, "{name:?}")?;
        }
        if !self.optional_fields.is_empty() {
            start(f)?;
            write!(f, "optional ")?;
            for (i, name) in self.optional_fields.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{name:?}")?;
            }
        }
        write!(f, " but got {}", self.erased_value.show_fields())
    }
}

pub struct FixedTuple<const UNNAMED: usize, const NAMED: usize, const OPTIONAL: usize, T> {
    unnamed: [T; UNNAMED],
    named: [T; NAMED],
    optional: [Option<T>; OPTIONAL],
}

impl<T> Tuple<T> {
    pub fn into_fixed<const UNNAMED: usize, const NAMED: usize, const OPTIONAL: usize>(
        mut self,
        named: [&str; NAMED],
        optional: [&str; OPTIONAL],
    ) -> Result<FixedTuple<UNNAMED, NAMED, OPTIONAL, T>, IntoFixedError> {
        let erased_value = self.as_ref().map(|_| ());
        macro_rules! error {
            ($reason:expr) => {
                Err(IntoFixedError {
                    expected_unnamed_fields: UNNAMED,
                    expected_named_fields: named.iter().map(|s| s.to_string()).collect(),
                    optional_fields: optional.iter().map(|s| s.to_string()).collect(),
                    reason: $reason,
                    erased_value,
                })
            };
        }

        if self.unnamed.len() != UNNAMED {
            return error!(IntoFixedErrorReason::IncorrectAmountOfUnnamedFields {
                expected: UNNAMED,
                actual: self.unnamed.len()
            });
        }
        let unnamed: [T; UNNAMED] = self.unnamed.try_into().ok().unwrap();

        if let Some(name) = self
            .named_order
            .iter()
            .find(|name| !named.contains(&name.as_str()) && !optional.contains(&name.as_str()))
        {
            return error!(IntoFixedErrorReason::NamedFieldUnexpected(name.to_string()));
        }

        if let Some(name) = named.iter().find(|&&name| !self.named.contains_key(name)) {
            return error!(IntoFixedErrorReason::NamedFieldNotPresent(name.to_string()));
        }

        let named = named.map(|name| self.named.remove(name).unwrap());
        let optional = optional.map(|name| self.named.remove(name));

        Ok(FixedTuple {
            unnamed,
            named,
            optional,
        })
    }
    /// let [a, b, c] = tuple.into_unnamed()
    pub fn into_unnamed<const N: usize>(self) -> Result<[T; N], IntoFixedError> {
        Ok(self.into_fixed([], [])?.unnamed)
    }
    pub fn into_named_opt<const N: usize, const OPT: usize>(
        self,
        names: [&str; N],
        optional: [&str; OPT],
    ) -> Result<([T; N], [Option<T>; OPT]), IntoFixedError> {
        let FixedTuple {
            unnamed: [],
            named,
            optional,
        } = self.into_fixed(names, optional)?;
        Ok((named, optional))
    }
    pub fn into_named<const N: usize>(self, names: [&str; N]) -> Result<[T; N], IntoFixedError> {
        Ok(self.into_fixed::<0, N, 0>(names, [])?.named)
    }
    pub fn into_single_named(self, name: &str) -> Result<T, IntoFixedError> {
        self.into_named([name]).map(|[value]| value)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TupleZipError {
    #[error("different amount of unnamed fields: {0} vs {1}")]
    DifferentUnnamedAmount(usize, usize),
    #[error("field {0} not present in other")]
    NamedNotPresentInOther(String),
    #[error("field {0} is only present in other")]
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

impl<T: std::fmt::Display> Tuple<T> {
    pub fn show<'a>(&'a self, field_value_symbol: &'a str) -> impl std::fmt::Display + 'a {
        struct Show<'a, T> {
            tuple: &'a Tuple<T>,
            field_value_symbol: &'a str,
        }
        impl<T> std::ops::Deref for Show<'_, T> {
            type Target = Tuple<T>;
            fn deref(&self) -> &Self::Target {
                self.tuple
            }
        }
        impl<T: std::fmt::Display> std::fmt::Display for Show<'_, T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "(")?;
                {
                    if !self.is_empty() && f.alternate() {
                        writeln!(f)?;
                    }
                    let mut f = pad_adapter::PadAdapter::with_padding(
                        f,
                        if f.alternate() { "    " } else { "" },
                    );
                    let mut first = true;
                    let mut before_field = |f: &mut pad_adapter::PadAdapter| -> std::fmt::Result {
                        if !first && !f.alternate() {
                            write!(f, ", ")?;
                        }
                        first = false;
                        Ok(())
                    };
                    let after_field = |f: &mut pad_adapter::PadAdapter| -> std::fmt::Result {
                        if f.alternate() {
                            writeln!(f, ",")?;
                        }
                        Ok(())
                    };
                    for field in &self.unnamed {
                        before_field(&mut f)?;
                        f.write(field)?;
                        after_field(&mut f)?;
                    }
                    for name in &self.named_order {
                        before_field(&mut f)?;
                        let field = &self.named[name];
                        write!(f, ".{name} {} ", self.field_value_symbol)?;
                        f.write(field)?;
                        after_field(&mut f)?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
        }
        Show {
            tuple: self,
            field_value_symbol,
        }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Tuple<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.show("=").fmt(f)
    }
}

pub struct NamedTupleFmt<'a, T> {
    name: &'a str,
    tuple: &'a Tuple<T>,
}

impl<T: std::fmt::Display> std::fmt::Display for NamedTupleFmt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)?;
        write!(f, " ")?;
        self.tuple.fmt(f)
    }
}
