use super::*;
use noisy_float::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    sync::Mutex,
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
    Group(Parc<Mutex<Group>>),
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
pub enum Edge {
    Value,
    Keyword(String),
}
impl Edge {
    pub fn is_open_bracket(&self) -> bool {
        match self {
            Self::Keyword(keyword) => is_open_bracket(keyword),
            Self::Value => false,
        }
    }
}

pub fn is_open_bracket(s: &str) -> bool {
    s.chars().any(|c| "([{".contains(c))
}

#[derive(Clone, Debug)]
pub struct ParseNode {
    pub finish: Option<Parc<SyntaxDefinition>>,
    pub next: HashMap<Edge, ParseNode>,
    pub binding_power: Option<BindingPower>,
    pub is_open_paren: bool,
}

impl ParseNode {
    fn new(is_open_paren: bool, binding_power: Option<BindingPower>) -> Self {
        Self {
            finish: None,
            next: HashMap::new(),
            binding_power,
            is_open_paren,
        }
    }
    pub fn with_power(is_open_paren: bool, binding_power: BindingPower) -> Self {
        Self::new(is_open_paren, Some(binding_power))
    }
    /// write what is expected after this node (a value or on of the keywords)
    pub fn format_possible_continuations(&self) -> impl std::fmt::Display + '_ {
        struct Format<'a>(&'a ParseNode);
        impl std::fmt::Display for Format<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                for (i, edge) in self.0.next.keys().enumerate() {
                    if i != 0 {
                        write!(f, " or ")?;
                    }
                    match edge {
                        Edge::Value => write!(f, "a value")?,
                        Edge::Keyword(keyword) => write!(f, "{keyword:?}")?,
                    }
                }
                Ok(())
            }
        }
        Format(self)
    }
}

#[derive(Clone, Debug)]
pub struct Syntax {
    pub(crate) keywords: HashSet<String>,
    pub(crate) root_without_start_value: ParseNode,
    pub(crate) root_with_start_value: ParseNode,
}

impl Syntax {
    pub fn empty() -> Self {
        Self {
            keywords: HashSet::new(),
            root_without_start_value: ParseNode::new(false, None),
            root_with_start_value: ParseNode::new(false, None),
        }
    }

    pub fn insert(&mut self, definition: Parc<SyntaxDefinition>) -> Result<(), ErrorMessage> {
        let binding_power = definition.binding_power();
        let skip;
        let mut current_node = match definition.parts[0] {
            SyntaxDefinitionPart::Keyword(_) => {
                skip = 0;
                &mut self.root_without_start_value
            }
            _ => {
                skip = 1;
                &mut self.root_with_start_value
            }
        };
        for part in definition.parts.iter().skip(skip) {
            let edge = match part {
                SyntaxDefinitionPart::Keyword(keyword) => {
                    self.keywords.insert(keyword.clone());
                    Edge::Keyword(keyword.clone())
                }
                SyntaxDefinitionPart::UnnamedBinding | SyntaxDefinitionPart::NamedBinding(_) => {
                    Edge::Value
                }
                SyntaxDefinitionPart::Group(_) => todo!(),
            };
            let is_open_bracket = edge.is_open_bracket();
            let next_node = current_node
                .next
                .entry(edge)
                .or_insert_with(|| ParseNode::with_power(is_open_bracket, binding_power));
            if next_node.binding_power != Some(binding_power) {
                return error!("different binding power");
            }
            current_node = next_node;
        }
        if let Some(current) = &current_node.finish {
            return error!(
                "Conficting syntax definitions: {:?} and {:?}",
                current.name, definition.name,
            );
        }
        current_node.finish = Some(definition);
        Ok(())
    }
}
