use crate::error::*;
use noisy_float::prelude::*;
use std::{
    collections::{HashMap, HashSet},
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BindingPower {
    pub priority: Priority,
    pub associativity: Associativity,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Edge {
    pub value_before_keyword: bool,
    pub keyword: String,
}

#[derive(Clone, Debug)]
pub enum NodeFinish {
    Complex(Arc<SyntaxDefinition>),
    SimpleValue,
}

#[derive(Clone, Debug)]
pub struct ParseNode {
    pub finish: HashMap<bool, NodeFinish>,
    pub next: HashMap<Edge, ParseNode>,
    pub binding_power: Option<BindingPower>,
    pub is_open_paren: bool,
}

impl ParseNode {
    fn new(is_open_paren: bool, binding_power: Option<BindingPower>) -> Self {
        Self {
            finish: HashMap::new(),
            next: HashMap::new(),
            binding_power,
            is_open_paren,
        }
    }
    pub fn with_power(is_open_paren: bool, binding_power: BindingPower) -> Self {
        Self::new(is_open_paren, Some(binding_power))
    }
}

#[derive(Clone, Debug)]
pub struct Syntax {
    pub keywords: HashSet<String>,
    pub root: ParseNode,
    pub maybe_join: ParseNode,
}

impl Syntax {
    pub fn empty() -> Self {
        Self {
            keywords: HashSet::new(),
            maybe_join: {
                let mut node = ParseNode::new(false, None);
                node.finish.insert(false, NodeFinish::SimpleValue);
                node
            },
            root: ParseNode::new(false, None),
        }
    }

    pub fn insert(&mut self, definition: Arc<SyntaxDefinition>) -> Result<(), ErrorMessage> {
        let binding_power = BindingPower {
            priority: definition.priority,
            associativity: definition.associativity,
        };
        if definition.parts.len() == 2
            && !definition
                .parts
                .iter()
                .any(|part| matches!(part, SyntaxDefinitionPart::Keyword(..)))
        {
            if self.maybe_join.binding_power.is_some() {
                return error!("Multiple join syntaxes");
            }
            self.maybe_join.binding_power = Some(binding_power);
            self.maybe_join
                .finish
                .insert(true, NodeFinish::Complex(definition));
            return Ok(());
        }
        let mut current_node = &mut self.root;
        let mut number_of_values_before_keyword = 0;
        let value_before_keyword =
            |number_of_values_before_keyword: usize| -> Result<bool, ErrorMessage> {
                Ok(match number_of_values_before_keyword {
                    0 => false,
                    1 => true,
                    _ => {
                        return error!(
                            "Multiple values without keyword in between is not supported"
                        );
                    }
                })
            };
        for part in &definition.parts {
            match part {
                SyntaxDefinitionPart::Keyword(keyword) => {
                    self.keywords.insert(keyword.clone());
                    let edge = Edge {
                        value_before_keyword: value_before_keyword(
                            number_of_values_before_keyword,
                        )?,
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
                    number_of_values_before_keyword = 0;
                    current_node = next_node;
                }
                SyntaxDefinitionPart::UnnamedBinding | SyntaxDefinitionPart::NamedBinding(_) => {
                    number_of_values_before_keyword += 1;
                }
            }
        }
        match current_node
            .finish
            .entry(value_before_keyword(number_of_values_before_keyword)?)
        {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(NodeFinish::Complex(definition));
            }
            std::collections::hash_map::Entry::Occupied(entry) => {
                let existing_def = match entry.get() {
                    NodeFinish::Complex(def) => def,
                    NodeFinish::SimpleValue => unreachable!(),
                };
                return error!(
                    "Conficting syntax definitions: {:?} and {:?}",
                    existing_def.name, definition.name,
                );
            }
        }
        Ok(())
    }
}
