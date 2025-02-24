use std::sync::Mutex;

use crate::lexer::Result;
use kast_util::*;

use crate::{Ast, SyntaxDefinitionPart};

// #[derive(Debug)]
// pub enum Quantifier<Elem> {
//     One(Elem),
//     /// `rest[";" expr]?`
//     ZeroOrOne(Option<Elem>),
//     /// `rest[";" expr]+`
//     OneOrMore(Elem, Vec<Elem>),
//     /// `rest[";" expr]*`
//     ZeroOrMore(Vec<Elem>),
//     /// `rest[";" expr]{2}`
//     Exact(usize),
// }

#[derive(Debug)]
pub enum Quantifier {
    One,
    /// `rest[";" expr]?`
    ZeroOrOne,
    /// `rest[";" expr]+`
    OneOrMore,
    /// `rest[";" expr]*`
    ZeroOrMore,
    /// `rest[";" expr]{2}`
    Exact(usize),
}

// impl Into<Ast> for &Quantifier<SyntaxDefinitionPart> {
//     fn into(self) -> Ast {
//         match self {
//             Quantifier::ZeroOrOne(first) => Ast::Complex {
//                 definition: (),
//                 values: (),
//                 data: (),
//             },
//             Quantifier::OneOrMore(first, rest) => todo!(),
//             Quantifier::ZeroOrMore(rest) => todo!(),
//             Quantifier::Exact(amount) => todo!(),
//         }
//     }
// }

#[derive(Debug)]
pub struct Group {
    pub name: String,
    pub quantifier: Quantifier,
    sub_parts: Vec<SyntaxDefinitionPart>,
}

impl Group {
    pub fn new(name: impl Into<String>) -> Self {
        Group {
            name: name.into(),
            quantifier: Quantifier::One,
            sub_parts: Vec::new(),
        }
    }

    pub fn into_complex_ast() -> Ast {
        Ast::Complex {
            definition: todo!(),
            values: todo!(),
            data: todo!(),
        }
    }
}

pub struct PartsAccumulator {
    parts: Vec<SyntaxDefinitionPart>,
    current: GroupPtr,
}

trait TryInsertGroup {
    fn try_insert_group(&mut self) -> Result<Parc<Mutex<Group>>>;
    fn try_assign_quantifier_to_last_group(&mut self);
}

impl TryInsertGroup for Vec<SyntaxDefinitionPart> {
    fn try_insert_group(&mut self) -> Result<Parc<Mutex<Group>>> {
        match &self.last() {
            Some(SyntaxDefinitionPart::NamedBinding(name)) => {
                let name = name.clone();
                _ = self.pop();
                let group = Parc::new(Mutex::new(Group::new(name)));
                self.push(SyntaxDefinitionPart::Group(group.clone()));
                Ok(group)
            }
            None | Some(_) => error!("quantifiers should be preceded with their name"),
        }
    }

    fn try_assign_quantifier_to_last_group(&mut self) {
        // TODO over here 1: convert anything to unnamed group
        match self.pop() {
            Some(SyntaxDefinitionPart::Keyword(keyword)) => {
                self.try_insert_group()?;
                todo!()
            }
            Some(SyntaxDefinitionPart::UnnamedBinding) => todo!(),
            Some(SyntaxDefinitionPart::NamedBinding(_)) => todo!(),
            Some(SyntaxDefinitionPart::Group(_)) => todo!(),
            None => todo!(),
        }
    }
}

/// Linked-list data-structure so that syntax-definitions may contain recursive [`Groups`](Group)
struct GroupPtr {
    /// the current group that parts being read will be pushed into
    /// *None* means we are in no group right now
    group: Option<Parc<Mutex<Group>>>,
    /// the previous group that is a parent of the current group
    /// *None* means we are in no group right now, or we are in a group of depth 1
    previous: Option<Parc<GroupPtr>>,
}

impl PartsAccumulator {
    pub fn new() -> Self {
        PartsAccumulator {
            parts: Vec::new(),
            current: GroupPtr {
                group: None,
                previous: None,
            },
        }
    }

    /// Insert a new group, increasing the nesting of groups
    /// Fails if there isn't a named-binding at the tail of the `parts` of the currently pointed-to group, which will become the name of the group
    pub fn insert_group(&mut self) -> Result<()> {
        let current = &mut self.current;
        (current.group, current.previous) = match (&current.group, &current.previous) {
            (None, None) => {
                let group = self.parts.try_insert_group()?;
                (Some(group), None)
            }
            (Some(current_group), None) => {
                let new_group = current_group.lock().unwrap().sub_parts.try_insert_group()?;
                (
                    Some(new_group),
                    Some(Parc::new(GroupPtr {
                        group: Some(current_group.clone()),
                        previous: None,
                    })),
                )
            }
            (Some(current_group), Some(previous)) => {
                let new_group = current_group.lock().unwrap().sub_parts.try_insert_group()?;
                (
                    Some(new_group),
                    Some(Parc::new(GroupPtr {
                        group: Some(current_group.clone()),
                        previous: Some(previous.clone()),
                    })),
                )
            }
            (None, Some(_)) => unreachable!(),
        };
        Ok(())
    }

    // TODO over here 2: remove quantifier from here, every group is implicitly closed as Quantifier::One
    pub fn close_group(&mut self, quantifier: Quantifier) -> Result<(), ()> {
        let current = &mut self.current;
        // error!("unexpected token, quantifie")
        (current.group, current.previous) = match (&current.group, &current.previous) {
            (None, None) => return Err(()),
            (Some(current_group), None) => {
                current_group.lock().unwrap().quantifier = quantifier;
                (None, None)
            }
            (Some(current_group), Some(previous)) => {
                current_group.lock().unwrap().quantifier = quantifier;
                (
                    Some(previous.group.clone().expect("unreachable")),
                    previous.previous.clone(),
                )
            }
            (None, Some(_)) => unreachable!(),
        };
        Ok(())
    }

    fn insert_non_group_part(&mut self, part: SyntaxDefinitionPart) {
        match &self.current.group {
            Some(current_group) => current_group.lock().unwrap().insert_part(part),
            None => self.parts.push(part),
        }
    }

    pub fn insert_keyword(&mut self, keyword: String) {
        self.insert_non_group_part(SyntaxDefinitionPart::Keyword(keyword))
    }
    pub fn insert_unnamed_binding(&mut self) {
        self.insert_non_group_part(SyntaxDefinitionPart::UnnamedBinding);
    }
    pub fn insert_named_binding(&mut self, name: String) {
        self.insert_non_group_part(SyntaxDefinitionPart::NamedBinding(name));
    }
}

impl TryInto<Ast> for PartsAccumulator {
    type Error = ();

    fn try_into(self) -> Result<Ast, Self::Error> {
        todo!()
    }
}
