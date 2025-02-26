use std::sync::Mutex;

use crate::{lexer::Result, SyntaxDefinitionPart};
use kast_util::*;

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
pub enum GroupTag {
    Named,
    Unnamed,
}

#[derive(Debug, PartialEq)]
pub enum Quantifier {
    /// `rest[";" expr]`
    /// Note: only exists for ease of implementation, not a valid quantifier
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

/// A group of syntax-defintion parts that has a quantifier indicating how many occurences should be
/// parsed
#[derive(Debug)]
pub struct Group {
    pub name: Option<String>,
    pub quantifier: Quantifier,
    sub_parts: Vec<SyntaxDefinitionPart>,
}

impl PartialEq for Group {
    fn eq(&self, other: &Self) -> bool {
        use std::ops::Deref;
        use SyntaxDefinitionPart::{Keyword, NamedBinding, UnnamedBinding};

        self.name == other.name
            && self.quantifier == other.quantifier
            && self
                .sub_parts
                .iter()
                .zip(other.sub_parts.iter())
                .try_for_each(|(a, b)| match (a, b) {
                    (Keyword(a), Keyword(b)) if a == b => Some(()),
                    (UnnamedBinding, UnnamedBinding) => Some(()),
                    (NamedBinding(a), NamedBinding(b)) if a == b => Some(()),
                    (SyntaxDefinitionPart::Group(a), SyntaxDefinitionPart::Group(b)) => {
                        let a = a.deref().lock().unwrap();
                        let b = b.deref().lock().unwrap();
                        (*a == *b).then_some(())
                    }
                    _ => None,
                })
                .is_some()
    }
}

impl Group {
    /// Create a new named group `fields[key ":" value]*`
    pub fn named(name: impl Into<String>) -> Self {
        Group {
            name: Some(name.into()),
            quantifier: Quantifier::One,
            sub_parts: Vec::new(),
        }
    }

    /// Create a new unnamed group `("->" returns)?`
    pub fn unnamed() -> Self {
        Group {
            name: None,
            quantifier: Quantifier::One,
            sub_parts: Vec::new(),
        }
    }

    pub fn is_named(&self) -> bool {
        self.name.is_some()
    }

    pub fn is_unnamed(&self) -> bool {
        self.name.is_none()
    }
}

pub struct PartsAccumulator {
    parts: Vec<SyntaxDefinitionPart>,
    current: GroupPtr,
    // If in the future quantifiers are allowed for every type of part, simply remove this field
    unassigned_group: bool,
}

trait TryInsertGroup {
    fn try_insert_named_group(&mut self) -> Result<Parc<Mutex<Group>>>;
    fn insert_unnamed_group(&mut self) -> Parc<Mutex<Group>>;
    // fn assign_quantifier(&mut self, quantifier: Quantifier) -> Result<()>;
}

impl TryInsertGroup for Vec<SyntaxDefinitionPart> {
    fn try_insert_named_group(&mut self) -> Result<Parc<Mutex<Group>>> {
        match &self.last() {
            Some(SyntaxDefinitionPart::NamedBinding(name)) => {
                let name = name.clone();
                _ = self.pop();
                let group = Parc::new(Mutex::new(Group::named(name)));
                self.push(SyntaxDefinitionPart::Group(group.clone()));
                Ok(group)
            }
            None | Some(_) => error!("named groups (`[ ... ]`) should be preceded with their name"),
        }
    }

    fn insert_unnamed_group(&mut self) -> Parc<Mutex<Group>> {
        let group = Parc::new(Mutex::new(Group::unnamed()));
        self.push(SyntaxDefinitionPart::Group(group.clone()));
        group
    }
}

/// Reverse linked-list data-structure so that syntax-definitions may contain nested [`Groups`](Group)
struct GroupPtr {
    /// the current group that parts being read will be pushed into
    /// *None* means we are in no group right now
    group: Option<Parc<Mutex<Group>>>,
    /// the pointer to the previous group-ptr node so we can get back to it when the current group
    /// gets closed
    /// *None* means we are in no group right now, or we are in a group inside the root
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
            unassigned_group: false,
        }
    }

    /// Request an inserter for this `PartsAccumulator`
    ///
    /// This extra step is added because inserting a part requires that any previously inserted
    /// groups have been assigned a quantifier after being closed
    pub fn insert<'a>(&'a mut self) -> Result<PartsAccumulatorInserter<'a>> {
        if self.unassigned_group {
            return error!("unexpected token encountered when expecting quantifier for group");
        } else {
            Ok(PartsAccumulatorInserter { inner: self })
        }
    }

    /// Finish this accumulator, returning the collected `Vec<SyntaxDefinitionPart>`s
    ///
    /// This fails if the any opened groups have not all been closed
    pub fn finish(self) -> Result<Vec<SyntaxDefinitionPart>> {
        if self.unassigned_group {
            return error!("syntax definition ended when expecting quantifier for group");
        }
        let current = self.current;
        match (current.group, current.previous) {
            (None, None) => Ok(self.parts),
            (Some(_), None | Some(_)) => {
                error!("syntax definition ended when expecting group to be closed")
            }
            (None, Some(_)) => unreachable!(),
        }
    }

    /// Close a group (match the `[` or `(` that marked the opening of a group with a `]` or `)`
    /// respectively)
    ///
    /// This fails in only one way, which is when its called while we are not in any groups
    pub fn close_group(&mut self, tag: GroupTag) -> Result<()> {
        let current = &mut self.current;
        if self.unassigned_group {
            return error!("unexpected token encountered when expecting quantifier for group");
        }
        (current.group, current.previous) = match (&current.group, &current.previous) {
            (None, None) => {
                return error!("unexpected symbol to close group when there are no groups open")
            }
            (Some(current_group), None) => {
                let group = current_group.lock().unwrap();
                if group.is_named() && !matches!(tag, GroupTag::Named) {
                    return error!("mismatch of tokens, named group `[` cannot be closed with `)`");
                } else if group.is_unnamed() && !matches!(tag, GroupTag::Unnamed) {
                    return error!(
                        "mismatch of tokens, unnamed group `(` cannot be closed with `]`"
                    );
                } else {
                    (None, None)
                }
            }
            (Some(_), Some(previous)) => (
                Some(previous.group.clone().expect("unreachable")),
                previous.previous.clone(),
            ),
            (None, Some(_)) => unreachable!(),
        };
        // Group that has been closed is now waiting to be assigned a quantifier
        self.unassigned_group = true;
        Ok(())
    }

    /// Try to assign a quantifier to the previously inserted group
    ///
    /// This fails if the previously inserted part was not a group
    // INFO Modify this fn if quantifiers are to be allowed for every kind of
    // [part](SyntaxDefinitionPart), not just [group](SyntaxDefinitionPart::Group)
    pub fn assign_quantifier(&mut self, quantifier: Quantifier) -> Result<()> {
        // Quantifier when group hasn't even been closed
        if !self.unassigned_group {
            return error!(
                "unexpected symbol: quantifiers are only allowed after named/unnamed groups"
            );
        }
        self.unassigned_group = false;
        match &self.current.group {
            Some(group) => {
                let group = group.lock().unwrap(); // temporary value
                Self::update_group(group.sub_parts.last(), quantifier)
            }
            // no nesting, we're at depth 0 and inserting directly into `PartsAccumulator.parts`
            None => Self::update_group(self.parts.last(), quantifier),
        }
    }

    // Utility function to try to update a group with a quantifier
    //
    // This fails if the part is None (vector of parts is empty or group just started) or if its not
    // a group
    fn update_group(part: Option<&SyntaxDefinitionPart>, quantifier: Quantifier) -> Result<()> {
        match part {
            Some(SyntaxDefinitionPart::Group(group)) => {
                group.lock().unwrap().quantifier = quantifier;
                Ok(())
            }
            // Quantifier is the first encountered part, or Quantifier after part that isn't a group
            None | Some(_) => {
                error!("unexpected symbol: quantifiers are only allowed after named/unnamed groups")
            }
        }
    }

    // Utility function to insert a part THAT WE KNOW is not a group
    // Used by `PartsAccumulatorInserter`'s `keyword()`, `unnamed_binding()` and `named_binding()`
    fn insert_non_group_part(&mut self, part: SyntaxDefinitionPart) {
        match &self.current.group {
            Some(current_group) => current_group.lock().unwrap().sub_parts.push(part),
            None => self.parts.push(part),
        }
    }
}

/// Interface through which we can insert parts into the (`PartsAccumulator`)[PartsAccumulator]
pub struct PartsAccumulatorInserter<'a> {
    inner: &'a mut PartsAccumulator,
}

impl<'a> PartsAccumulatorInserter<'a> {
    /// Insert a new unnamed group, thus increasing the nest depth
    pub fn unnamed_group(&mut self) {
        let current = &mut self.inner.current;
        (current.group, current.previous) = match (&current.group, &current.previous) {
            (None, None) => {
                let group = self.inner.parts.insert_unnamed_group();
                (Some(group), None)
            }
            (Some(current_group), None) => {
                let new_group = current_group
                    .lock()
                    .unwrap()
                    .sub_parts
                    .insert_unnamed_group();
                (
                    Some(new_group),
                    Some(Parc::new(GroupPtr {
                        group: Some(current_group.clone()),
                        previous: None,
                    })),
                )
            }
            (Some(current_group), Some(previous)) => {
                let new_group = current_group
                    .lock()
                    .unwrap()
                    .sub_parts
                    .insert_unnamed_group();
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
    }

    /// Insert a new named group, thus increasing the nest depth
    ///
    /// This fails if there isn't a named-binding at the tail of the `parts` of the currently
    /// pointed-to group, which is supposed to be popped out to become the name of this group
    pub fn named_group(&mut self) -> Result<()> {
        let current = &mut self.inner.current;
        (current.group, current.previous) = match (&current.group, &current.previous) {
            (None, None) => {
                let group = self.inner.parts.try_insert_named_group()?;
                (Some(group), None)
            }
            (Some(current_group), None) => {
                let new_group = current_group
                    .lock()
                    .unwrap()
                    .sub_parts
                    .try_insert_named_group()?;
                (
                    Some(new_group),
                    Some(Parc::new(GroupPtr {
                        group: Some(current_group.clone()),
                        previous: None,
                    })),
                )
            }
            (Some(current_group), Some(previous)) => {
                let new_group = current_group
                    .lock()
                    .unwrap()
                    .sub_parts
                    .try_insert_named_group()?;
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

    /// Insert a keyword
    pub fn keyword(&mut self, keyword: String) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::Keyword(keyword))
    }

    /// Insert an unnamed-binding
    pub fn unnamed_binding(&mut self) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::UnnamedBinding);
    }

    /// Insert a named-binding
    pub fn named_binding(&mut self, name: impl Into<String>) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::NamedBinding(name.into()));
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        group::{Group, Quantifier},
        lexer::Result,
        read_syntax, Syntax, SyntaxDefinition, SyntaxDefinitionPart,
    };
    use kast_util::{Error, Parc, SourceFile};
    use std::sync::{Arc, Mutex};

    use Quantifier::*;
    use SyntaxDefinitionPart::{Keyword, NamedBinding};

    fn make_syntax(source: impl Into<String>) -> Result<Syntax, Error> {
        let syntax = read_syntax(SourceFile {
            contents: source.into(),
            filename: "<stdin>".into(),
        })?;
        Ok(syntax)
    }

    fn group_ptr(group: super::Group) -> SyntaxDefinitionPart {
        SyntaxDefinitionPart::Group(Parc::new(Mutex::new(group)))
    }

    fn test<'a>(source: impl Into<&'a str>, expected_parts: Vec<SyntaxDefinitionPart>) {
        let parts;
        {
            let syntax = make_syntax(String::from("syntax foo <- 10 = ") + source.into()).unwrap();
            parts = Arc::from(
                syntax
                    .root_with_start_value
                    .finish
                    .as_ref()
                    .unwrap()
                    .clone(),
            );
            // so that Parc held by `syntax` gets dropped
        }
        let parts: SyntaxDefinition = Arc::into_inner(parts).unwrap();
        let parts = parts.parts;
        assert_eq!(parts.len(), expected_parts.len());

        assert_eq!(
            &Group {
                name: None,
                quantifier: One,
                sub_parts: parts,
            },
            &Group {
                name: None,
                quantifier: One,
                sub_parts: expected_parts,
            }
        );
    }

    #[test]
    fn group_named() {
        test(
            r#"fields [ key ":" value ]?"#,
            vec![group_ptr(Group {
                name: Some("fields".into()),
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("key".into()),
                    Keyword(":".into()),
                    NamedBinding("value".into()),
                ],
            })],
        );
    }

    #[test]
    fn group_unnamed() {
        test(
            r#"( keys ":" values )?"#,
            vec![group_ptr(Group {
                name: None,
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("keys".into()),
                    Keyword(":".into()),
                    NamedBinding("values".into()),
                ],
            })],
        );
    }

    #[test]
    fn group_named_nested() {
        test(
            r#"hashTableFields[ bucket "=>" values[ value "," ]* ]?"#,
            vec![group_ptr(Group {
                name: Some("hashTableFields".into()),
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("bucket".into()),
                    Keyword("=>".into()),
                    group_ptr(Group {
                        name: Some("values".into()),
                        quantifier: ZeroOrMore,
                        sub_parts: vec![NamedBinding("value".into()), Keyword(",".into())],
                    }),
                ],
            })],
        );
    }

    #[test]
    fn group_unnamed_nested() {
        test(
            r#"( buckets "=>" ( values "," )* )?"#,
            vec![group_ptr(Group {
                name: None,
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("buckets".into()),
                    Keyword("=>".into()),
                    group_ptr(Group {
                        name: None,
                        quantifier: ZeroOrMore,
                        sub_parts: vec![NamedBinding("values".into()), Keyword(",".into())],
                    }),
                ],
            })],
        );
    }
}
