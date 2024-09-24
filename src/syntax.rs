use crate::error::*;
use noisy_float::prelude::*;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Priority(R64);

impl Priority {
    pub fn new(raw: f64) -> Self {
        Self(R64::new(raw))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug)]
pub enum SyntaxDefinitionPart {
    Keyword(String),
    UnnamedBinding,
    NamedBinding(String),
}

#[derive(Debug)]
pub struct SyntaxDefinition {
    pub name: String,
    pub priority: Priority,
    pub associativity: Associativity,
    pub parts: Vec<SyntaxDefinitionPart>,
}

impl SyntaxDefinition {
    pub fn binding_power(&self) -> BindingPower {
        BindingPower {
            priority: self.priority,
            associativity: self.associativity,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BindingPower {
    pub priority: Priority,
    pub associativity: Associativity,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Edge {
    pub values_before_keyword: usize,
    pub keyword: String,
}

#[derive(Clone, Debug)]
pub struct ParseNode {
    /// Map key is amount of values before we finish
    pub finish: BTreeMap<usize, Arc<SyntaxDefinition>>,
    pub next: HashMap<Edge, ParseNode>,
    pub binding_power: Option<BindingPower>,
    pub is_open_paren: bool,
}

impl ParseNode {
    fn new(is_open_paren: bool, binding_power: Option<BindingPower>) -> Self {
        Self {
            finish: BTreeMap::new(),
            next: HashMap::new(),
            binding_power,
            is_open_paren,
        }
    }
    pub fn with_power(is_open_paren: bool, binding_power: BindingPower) -> Self {
        Self::new(is_open_paren, Some(binding_power))
    }
    pub fn format_possible_continuations(&self) -> impl std::fmt::Display + '_ {
        struct Format<'a>(&'a ParseNode);
        impl std::fmt::Display for Format<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let write_values =
                    |f: &mut std::fmt::Formatter<'_>, values: usize| -> std::fmt::Result {
                        for i in 0..values {
                            if i != 0 {
                                write!(f, " ")?;
                            }
                            write!(f, "_")?;
                        }
                        Ok(())
                    };
                let mut option_written = false;
                let mut start_option = |f: &mut std::fmt::Formatter<'_>| match option_written {
                    false => {
                        option_written = true;
                        write!(f, "\"")
                    }
                    true => write!(f, ", \""),
                };
                let finish_option = |f: &mut std::fmt::Formatter<'_>| write!(f, "\"");
                for &values in self.0.finish.keys() {
                    start_option(f)?;
                    write_values(f, values)?;
                    if values == 0 {
                        write!(f, "<no values>")?;
                    }
                    finish_option(f)?;
                }
                for edge in self.0.next.keys() {
                    start_option(f)?;
                    write_values(f, edge.values_before_keyword)?;
                    if edge.values_before_keyword != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", edge.keyword)?;
                    finish_option(f)?;
                }
                Ok(())
            }
        }
        Format(self)
    }
}

#[derive(Clone, Debug)]
pub struct Syntax {
    pub keywords: HashSet<String>,
    pub root: ParseNode,
}

impl Syntax {
    pub fn empty() -> Self {
        Self {
            keywords: HashSet::new(),
            root: ParseNode::new(false, None),
        }
    }

    pub fn insert(&mut self, definition: Arc<SyntaxDefinition>) -> Result<(), ErrorMessage> {
        let binding_power = definition.binding_power();
        let mut current_node = &mut self.root;
        let mut values_before_keyword = 0;
        for part in &definition.parts {
            match part {
                SyntaxDefinitionPart::Keyword(keyword) => {
                    self.keywords.insert(keyword.clone());
                    let edge = Edge {
                        values_before_keyword,
                        keyword: keyword.clone(),
                    };
                    let next_node = current_node.next.entry(edge).or_insert_with(|| {
                        ParseNode::with_power(
                            keyword.chars().any(|c| "([{".contains(c)),
                            binding_power,
                        )
                    });
                    if next_node.binding_power != Some(binding_power) {
                        return error!("different binding power");
                    }
                    values_before_keyword = 0;
                    current_node = next_node;
                }
                SyntaxDefinitionPart::UnnamedBinding | SyntaxDefinitionPart::NamedBinding(_) => {
                    values_before_keyword += 1;
                }
            }
        }
        use std::collections::btree_map::Entry;
        match current_node.finish.entry(values_before_keyword) {
            Entry::Vacant(entry) => {
                entry.insert(definition);
            }
            Entry::Occupied(entry) => {
                return error!(
                    "Conficting syntax definitions: {:?} and {:?}",
                    entry.get().name,
                    definition.name,
                );
            }
        }
        Ok(())
    }
}
