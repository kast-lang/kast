use std::{collections::BTreeMap, fmt::Write};

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
