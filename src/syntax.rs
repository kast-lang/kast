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
pub struct ParseNode {
    pub finish: HashMap<bool, Arc<SyntaxDefinition>>,
    pub next: HashMap<Edge, ParseNode>,
    pub binding_power: Option<BindingPower>,
}

impl ParseNode {
    fn new(binding_power: Option<BindingPower>) -> Self {
        Self {
            finish: HashMap::new(),
            next: HashMap::new(),
            binding_power,
        }
    }
    pub fn with_power(binding_power: BindingPower) -> Self {
        Self::new(Some(binding_power))
    }
}

#[derive(Clone, Debug)]
pub struct Syntax {
    pub keywords: HashSet<String>,
    pub join: Option<ParseNode>,
    pub root_node: ParseNode,
}

impl Syntax {
    pub fn empty() -> Self {
        Self {
            keywords: HashSet::new(),
            join: None,
            root_node: ParseNode::new(None),
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
            if self.join.is_some() {
                return error!("Multiple join syntaxes");
            }
            self.join = Some(ParseNode {
                finish: HashMap::from_iter([(true, definition)]),
                next: HashMap::new(),
                binding_power: Some(binding_power),
            });
            return Ok(());
        }
        let mut current_node = &mut self.root_node;
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
                    let next_node = current_node
                        .next
                        .entry(edge)
                        .or_insert_with(|| ParseNode::with_power(binding_power));
                    if next_node.binding_power != Some(binding_power) {
                        return error!("different binding power");
                    }
                    number_of_values_before_keyword = 0;
                    current_node = next_node;
                }
                SyntaxDefinitionPart::UnnamedBinding => {
                    number_of_values_before_keyword += 1;
                }
                SyntaxDefinitionPart::NamedBinding(_) => {
                    number_of_values_before_keyword += 1;
                }
            }
        }
        match current_node
            .finish
            .entry(value_before_keyword(number_of_values_before_keyword)?)
        {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(definition);
            }
            std::collections::hash_map::Entry::Occupied(entry) => {
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
