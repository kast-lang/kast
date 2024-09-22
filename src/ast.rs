use std::{
    collections::{BTreeMap, VecDeque},
    path::PathBuf,
    sync::Arc,
};

use super::*;
use crate::{lexer::*, syntax::*};
use error::*;

#[derive(Debug)]
pub enum Ast<Data = Span> {
    Simple {
        token: Token,
        data: Data,
    },
    Complex {
        definition: Arc<SyntaxDefinition>,
        values: Tuple<Self>,
    },
    SyntaxDefinition {
        def: Arc<SyntaxDefinition>,
        data: Data,
    },
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Display<T: std::fmt::Display>(T);
        impl<T: std::fmt::Display> std::fmt::Debug for Display<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }
        match self {
            Ast::Simple { token, .. } => write!(f, "{:?}", token.raw()),
            Ast::Complex { definition, values } => {
                let mut f = f.debug_struct(&definition.name);
                for (index, value) in values.unnamed.iter().enumerate() {
                    f.field(&index.to_string(), &Display(value));
                }
                for (name, value) in &values.named {
                    f.field(name, &Display(value));
                }
                f.finish()
            }
            Ast::SyntaxDefinition { def, .. } => write!(f, "syntax {:?}", def.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple<T> {
    pub unnamed: Vec<T>,
    pub named: BTreeMap<String, T>,
}

pub fn parse(syntax: &Syntax, source: SourceFile) -> Result<Ast, Error> {
    let start = Position {
        index: 0,
        line: 1,
        column: 1,
    };
    let filename = source.filename.clone();
    let tokens: VecDeque<_> = lex(source)
        .collect::<Result<_, _>>()
        .expect("todo error message");
    let mut parser = Parser {
        filename: filename.clone(),
        position: start,
        end: tokens.back().map_or(start, |last| last.span.end),
        tokens,
    };
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.read(syntax)))
        .map_err(|e| {
            if let Some(s) = e.downcast_ref::<&str>() {
                return ErrorMessage(s.to_string());
            }
            if let Ok(s) = e.downcast::<String>() {
                return ErrorMessage(*s);
            }
            error_fmt!("unknown")
        })
        .map_err(|ErrorMessage(message)| Error {
            filename: filename.clone(),
            message,
            position: parser
                .tokens
                .front()
                .map_or(parser.end, |front| front.span.start),
        })
}

pub fn read_syntax(source: SourceFile) -> Syntax {
    let mut parser = Parser {
        filename: source.filename.clone(),
        tokens: lex(source).collect::<Result<_, _>>().unwrap(),
        // TODO
        position: Position {
            index: 0,
            line: 1,
            column: 1,
        },
        end: Position {
            index: 0,
            line: 1,
            column: 1,
        },
    };
    let mut syntax = Syntax::empty();
    loop {
        while parser
            .tokens
            .front()
            .map_or(false, |front| match &front.token {
                Token::Punctuation { raw } if raw == ";" => true,
                Token::Comment { .. } => true,
                _ => false,
            })
        {
            parser.tokens.pop_front();
        }
        if parser.tokens.is_empty() {
            break;
        }
        let def = parser.read_syntax_def().0;
        syntax.insert(Arc::new(def)).unwrap();
    }
    syntax
}

struct Parser {
    tokens: VecDeque<SpannedToken>,
    filename: PathBuf,
    position: Position,
    end: Position,
}

impl Parser {
    fn read_syntax_def(&mut self) -> (SyntaxDefinition, Span) {
        let start = match self.tokens.front() {
            Some(token) if token.raw() == "syntax" => self.tokens.pop_front().unwrap().span.start,
            _ => panic!("expected a syntax definition"),
        };
        let name_token = self
            .tokens
            .pop_front()
            .expect("expected a name for the syntax");
        let name = match name_token.token {
            Token::Ident { name, .. } => name,
            _ => panic!("name for the syntax must be an identifier"),
        };
        let associativity = match self
            .tokens
            .pop_front()
            .expect("expected a associativity")
            .token
        {
            Token::Punctuation { raw } if raw == "<-" => Associativity::Left,
            Token::Punctuation { raw } if raw == "->" => Associativity::Right,
            _ => panic!("expected associativity (<- or ->)"),
        };
        let priority = match self.tokens.pop_front().expect("expected a priority").token {
            Token::Number { raw } | Token::String { contents: raw, .. } => {
                Priority::new(raw.parse().expect("failed to parse priority"))
            }
            _ => panic!("syntax priority must be a number"),
        };
        if self
            .tokens
            .pop_front()
            .map(|spanned| spanned.token.raw().to_owned())
            != Some("=".to_owned())
        {
            panic!("expected a =");
        }
        let mut parts = Vec::new();
        let mut end = None;
        while let Some(token) = self.tokens.front() {
            parts.push(match &token.token {
                Token::Ident { name, .. } => {
                    if name == "_" {
                        SyntaxDefinitionPart::UnnamedBinding
                    } else {
                        SyntaxDefinitionPart::NamedBinding(name.clone())
                    }
                }
                Token::String { contents, .. } => SyntaxDefinitionPart::Keyword(contents.clone()),
                _ => break,
            });
            end = Some(self.tokens.pop_front().unwrap().span.end);
        }
        (
            SyntaxDefinition {
                name,
                priority,
                associativity,
                parts,
            },
            Span {
                start,
                end: end.unwrap(),
                filename: self.filename.clone(),
            },
        )
    }
}

struct ReadOne<'a> {
    parser: &'a mut Parser,
    syntax: &'a Syntax,
    made_progress: bool,
    unassigned_value: Option<Ast>,
    until: Option<BindingPower>,
    current_node: &'a syntax::ParseNode,
    assigned_values: Vec<Ast>,
}

#[must_use]
enum ReadOneProgress {
    Ready(Option<Ast>),
    NotReady,
}

impl ReadOne<'_> {
    fn finish(mut self, mut extra_value: Option<Ast>) -> ReadResult {
        if self.unassigned_value.is_none() {
            self.unassigned_value = extra_value.take();
        }
        let result = if self.made_progress {
            let mut result = None;
            tracing::trace!(
                "finishing with {:?} unassigned values",
                self.unassigned_value.iter().count(),
            );
            for using_unassigned_value in 0..=(self.unassigned_value.iter().count()) {
                let using_unassigned_value = using_unassigned_value != 0;
                if let Some(definition) = self.current_node.finish.get(&using_unassigned_value) {
                    if using_unassigned_value {
                        self.assigned_values
                            .push(self.unassigned_value.take().unwrap());
                    }
                    result = Some(Ast::Complex {
                        definition: definition.clone(),
                        values: definition.assign_values(self.assigned_values),
                    });
                    break;
                }
            }
            let result = result.expect("could not finish");
            tracing::trace!("parsed {result}");
            Some(result)
        } else {
            tracing::trace!("finishing without progress");
            None
        };
        ReadResult {
            result,
            extra_value: match (self.unassigned_value, extra_value) {
                (None, None) => None,
                (Some(value), None) | (None, Some(value)) => Some(value),
                (Some(_), Some(_)) => unreachable!(),
            },
        }
    }
    fn progress_with_value(&mut self, mut value: Ast) -> ReadOneProgress {
        while self.unassigned_value.is_some() {
            match self
                .syntax
                .join
                .as_ref()
                .filter(|join| should_resume(self.until, join.binding_power))
            {
                Some(join_state) => {
                    tracing::trace!("joining");
                    assert!(
                        join_state.binding_power.as_ref().unwrap().associativity
                            == Associativity::Left
                    );
                    let result = ReadOne {
                        parser: self.parser,
                        syntax: self.syntax,
                        made_progress: true,
                        unassigned_value: Some(value),
                        until: join_state.binding_power,
                        current_node: join_state,
                        assigned_values: vec![self.unassigned_value.take().unwrap()],
                    }
                    .read_one();
                    tracing::trace!("join is done : {result:?}");
                    let result_value = result.result.expect("join didnt finish?");
                    if let Some(extra_value) = result.extra_value {
                        self.unassigned_value = Some(result_value);
                        value = extra_value;
                        continue;
                    } else {
                        value = result_value;
                    }
                }
                None => return ReadOneProgress::Ready(Some(value)),
            }
        }
        let updated_syntax = match &value {
            Ast::SyntaxDefinition { def, .. } => {
                let mut syntax = self.syntax.clone();
                syntax.insert(def.clone()).unwrap();
                syntax
            }
            _ => self.syntax.clone(),
        };
        let inner = self.parser.read_until(
            &updated_syntax,
            Some(value),
            self.min_binding_power_for_inner_values(),
        );
        self.unassigned_value = match inner.result {
            Some(inner_result) => {
                if let Some(extra) = inner.extra_value {
                    todo!()
                }
                Some(inner_result)
            }
            None => inner.extra_value,
        };
        ReadOneProgress::NotReady
    }
    fn progress(&mut self) -> ReadOneProgress {
        let Some(token) = self.parser.tokens.front() else {
            return ReadOneProgress::Ready(None);
        };
        let edge = Edge {
            value_before_keyword: self.unassigned_value.is_some(),
            keyword: token.raw().to_owned(),
        };
        tracing::trace!("looking up edge {edge:?}");
        match self.current_node.next.get(&edge) {
            Some(next_node) => {
                if should_resume(self.until, next_node.binding_power) {
                    tracing::trace!("continued with {edge:?}");
                    self.parser.tokens.pop_front().unwrap();
                    self.current_node = next_node;
                    self.made_progress = true;
                    self.assigned_values.extend(self.unassigned_value.take());
                } else {
                    return ReadOneProgress::Ready(None);
                }
            }
            None => {
                let raw_token = token.raw();
                let value = if raw_token == "syntax" {
                    let (def, span) = self.parser.read_syntax_def();
                    let def = Arc::new(def);
                    Ast::SyntaxDefinition { def, data: span }
                } else if self.syntax.keywords.contains(raw_token) {
                    if !self.made_progress {
                        return ReadOneProgress::Ready(None);
                    }
                    let inner = self.parser.read_until(
                        self.syntax,
                        self.unassigned_value.take(),
                        self.min_binding_power_for_inner_values(),
                    );
                    match inner.result {
                        Some(value) => {
                            if inner.extra_value.is_some() {
                                todo!()
                            }
                            value
                        }
                        None => {
                            return ReadOneProgress::Ready(inner.extra_value);
                        }
                    }
                } else {
                    let token = self.parser.tokens.pop_front().unwrap();
                    tracing::trace!("simple {:?}", token.raw());
                    Ast::Simple {
                        token: token.token,
                        data: token.span,
                    }
                };
                match self.progress_with_value(value) {
                    ReadOneProgress::Ready(result) => return ReadOneProgress::Ready(result),
                    ReadOneProgress::NotReady => {}
                }
            }
        }
        tracing::trace!("not ready");
        ReadOneProgress::NotReady
    }

    fn read_one(mut self) -> ReadResult {
        loop {
            if let ReadOneProgress::Ready(extra_value) = self.progress() {
                return self.finish(extra_value);
            }
        }
    }

    fn min_binding_power_for_inner_values(&self) -> Option<BindingPower> {
        if !self.made_progress {
            return self.until;
        }

        if !self.current_node.finish.is_empty()
            || self
                .current_node
                .next
                .keys()
                .any(|edge| self.syntax.root_node.next.contains_key(edge))
        {
            self.current_node.binding_power
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct ReadResult {
    result: Option<Ast>,
    extra_value: Option<Ast>,
}

impl Parser {
    /// Read a single ast node (not just single token), without trying to combine it with the rest of the tokens
    /// (stops as soon as a single complex node is completed)
    fn read_one(
        &mut self,
        syntax: &Syntax,
        unassigned_value: Option<Ast>,
        until: Option<BindingPower>,
    ) -> ReadResult {
        tracing::trace!(
            "start with unassigned_value={:?} until={until:?}",
            unassigned_value.is_some(),
        );
        ReadOne {
            parser: self,
            syntax,
            made_progress: false,
            unassigned_value,
            until,
            current_node: &syntax.root_node,
            assigned_values: Vec::new(),
        }
        .read_one()
    }
}

impl Parser {
    /// Read an ast node from the stream of tokens, potentially starting with existing node,
    /// only using binding power stronger than given
    ///
    /// When we have smth like `a + b + c`
    /// read_one will only parse the `a + b`
    /// read_until is trying to combine the first parsed node with the rest
    fn read_until(
        &mut self,
        syntax: &Syntax,
        mut unassigned_value: Option<Ast>,
        until: Option<BindingPower>,
    ) -> ReadResult {
        loop {
            let one = self.read_one(syntax, unassigned_value, until);
            if one.result.is_none() {
                return one;
            }
            if one.extra_value.is_some() {
                todo!();
            }
            unassigned_value = one.result;
        }
    }

    fn read(&mut self, syntax: &Syntax) -> Ast {
        let result = self.read_until(syntax, None, None);
        match result.result {
            Some(result_value) => {
                assert!(result.extra_value.is_none(), "todo");
                result_value
            }
            None => result.extra_value.expect("hmm"),
        }
    }
}

impl SyntaxDefinition {
    fn assign_values(&self, values: impl IntoIterator<Item = Ast>) -> Tuple<Ast> {
        let mut result = Tuple {
            unnamed: Vec::new(),
            named: BTreeMap::new(),
        };
        let mut values = values.into_iter();
        for part in &self.parts {
            match part {
                SyntaxDefinitionPart::Keyword(_) => {}
                SyntaxDefinitionPart::UnnamedBinding => {
                    result
                        .unnamed
                        .push(values.next().expect("not enough values"));
                }
                SyntaxDefinitionPart::NamedBinding(name) => {
                    result
                        .named
                        .insert(name.clone(), values.next().expect("not enough values"));
                }
            }
        }
        if values.next().is_some() {
            panic!("too many values");
        }
        result
    }
}

fn should_resume(until: Option<BindingPower>, with: Option<BindingPower>) -> bool {
    let Some(until) = until else {
        return true;
    };
    match with {
        None => false,
        Some(with) => match until.priority.cmp(&with.priority) {
            std::cmp::Ordering::Equal => {
                if until.associativity != with.associativity {
                    panic!("same priority different associativity");
                }
                match until.associativity {
                    Associativity::Left => false,
                    Associativity::Right => true,
                }
            }
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
        },
    }
}
