use std::{collections::BTreeMap, path::PathBuf, sync::Arc};

use super::*;
use crate::{lexer::*, syntax::*};
use error::*;

#[allow(dead_code)]
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

pub fn parse(syntax: &Syntax, source: SourceFile) -> Result<Option<Ast>, Error> {
    let filename = source.filename.clone();
    let tokens: Vec<_> = lex(source).collect::<Result<_, _>>()?;
    let mut parser = Parser {
        filename: filename.clone(),
        unprocessed: Unprocessed {
            reader: peek2::Reader::new(filename.clone(), tokens),
            value: None,
        },
    };
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.read_all(syntax)))
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
            position: parser.unprocessed.reader.position(),
        })
}

pub fn read_syntax(source: SourceFile) -> Syntax {
    let mut reader = peek2::Reader::new(
        source.filename.clone(),
        lex(source).collect::<Result<Vec<_>, _>>().unwrap(),
    );
    let mut syntax = Syntax::empty();
    loop {
        while reader.peek().map_or(false, |front| match &front.token {
            Token::Punctuation { raw } if raw == ";" => true,
            Token::Comment { .. } => true,
            _ => false,
        }) {
            reader.next().unwrap();
        }
        if reader.peek().is_none() {
            break;
        }
        let def = read_syntax_def(&mut reader).0;
        syntax.insert(Arc::new(def)).unwrap();
    }
    syntax
}

struct Unprocessed {
    value: Option<Ast>,
    reader: peek2::Reader<SpannedToken>,
}

enum TokenOrValue<'a> {
    Token(&'a SpannedToken),
    #[allow(dead_code)]
    Value(&'a Ast),
}

impl Unprocessed {
    fn peek(&mut self) -> Option<TokenOrValue<'_>> {
        if let Some(value) = &self.value {
            return Some(TokenOrValue::Value(value));
        }
        self.reader.peek().map(TokenOrValue::Token)
    }
    fn reader(&mut self) -> &mut peek2::Reader<SpannedToken> {
        assert!(self.value.is_none());
        &mut self.reader
    }
    /// pop the value, panic if no unprocessed value
    fn pop_value(&mut self) -> Ast {
        self.value.take().unwrap()
    }
    // pop the token, panic if EOF or have unprocessed value
    fn pop_token(&mut self) -> SpannedToken {
        if self.value.is_some() {
            panic!("have unprocessed value");
        }
        self.reader.next().unwrap()
    }

    fn progress(&self) -> usize {
        self.reader.progress()
            + match self.value {
                Some(_) => 0,
                None => 1,
            }
    }
}

struct Parser {
    unprocessed: Unprocessed,
    #[allow(dead_code)]
    filename: PathBuf,
}

fn read_syntax_def(reader: &mut peek2::Reader<SpannedToken>) -> (SyntaxDefinition, Span) {
    let Span {
        start,
        end: _,
        filename,
    } = match reader.peek() {
        Some(token) if token.raw() == "syntax" => reader.next().unwrap().span,
        _ => panic!("expected a syntax definition"),
    };
    let name_token = reader.next().expect("expected a name for the syntax");
    let name = match name_token.token {
        Token::Ident { name, .. } => name,
        _ => panic!("name for the syntax must be an identifier"),
    };
    let associativity = match reader.next().expect("expected a associativity").token {
        Token::Punctuation { raw } if raw == "<-" => Associativity::Left,
        Token::Punctuation { raw } if raw == "->" => Associativity::Right,
        _ => panic!("expected associativity (<- or ->)"),
    };
    let priority = match reader.next().expect("expected a priority").token {
        Token::Number { raw } | Token::String { contents: raw, .. } => {
            Priority::new(raw.parse().expect("failed to parse priority"))
        }
        _ => panic!("syntax priority must be a number"),
    };
    if reader.next().map(|spanned| spanned.token.raw().to_owned()) != Some("=".to_owned()) {
        panic!("expected a =");
    }
    let mut parts = Vec::new();
    let mut end = None;
    while let Some(token) = reader.peek() {
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
        end = Some(reader.next().unwrap().span.end);
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
            filename: filename.clone(),
        },
    )
}

// My wife yelled at me for forgetting to lock the front door last night, but later apologized. She
// wanted to be safe, then sorry.

struct ReadOne<'a> {
    parser: &'a mut Parser,
    syntax: &'a Syntax,
    made_progress: bool,
    unassigned_value: Option<Ast>,
    until: Option<BindingPower>,
    current_node: &'a syntax::ParseNode,
    assigned_values: Vec<Ast>,
    should_try_join: bool,
}

#[must_use]
enum ReadOneProgress {
    Ready,
    NotReady,
}

// Bеst viewers on https://www.twitch.tv/kuviman
impl ReadOne<'_> {
    fn finish(mut self) -> Option<Ast> {
        if self.should_try_join {
            assert!(self.assigned_values.is_empty());
            tracing::trace!("finishing without any work");
            return self.unassigned_value;
        }
        let mut result = None;
        tracing::trace!(
            "finishing with {:?} unassigned values",
            self.unassigned_value.iter().count(),
        );
        for using_unassigned_value in [true, false] {
            if using_unassigned_value && self.unassigned_value.is_none() {
                continue;
            }
            if let Some(finish) = self.current_node.finish.get(&using_unassigned_value) {
                if using_unassigned_value {
                    self.assigned_values
                        .push(self.unassigned_value.take().unwrap());
                }
                result = Some(match finish {
                    NodeFinish::Complex(definition) => Ast::Complex {
                        definition: definition.clone(),
                        values: definition.assign_values(self.assigned_values),
                    },
                    NodeFinish::SimpleValue => {
                        assert!(!using_unassigned_value);
                        assert!(self.assigned_values.len() == 1);
                        self.assigned_values.pop().unwrap()
                    }
                });
                break;
            }
        }
        let result = result.expect("could not finish");
        tracing::trace!("parsed {result}");
        if let Some(value) = self.unassigned_value.take() {
            let prev_unprocessed_value = self.parser.unprocessed.value.replace(value);
            assert!(prev_unprocessed_value.is_none());
        }
        Some(result)
    }
    // is this helpful yet?
    fn progress(&mut self) -> ReadOneProgress {
        let Some(peek) = self.parser.unprocessed.peek() else {
            return ReadOneProgress::Ready;
        };
        let keyword = match peek {
            TokenOrValue::Token(token) => {
                let raw = token.raw();
                self.syntax.keywords.contains(raw).then_some(raw)
            }
            TokenOrValue::Value(_) => None,
        };
        let edge = keyword.map(|keyword| Edge {
            value_before_keyword: self.unassigned_value.is_some(),
            keyword: keyword.to_owned(),
        });
        tracing::trace!("continuing with until={:?}", self.until);
        tracing::trace!("looking up edge {edge:?}");
        match edge
            .as_ref()
            .and_then(|edge| self.current_node.next.get(edge))
        {
            Some(next_node) => {
                if should_resume(self.until, next_node.binding_power) {
                    tracing::trace!("continued with {edge:?}");
                    self.parser.unprocessed.pop_token();
                    self.current_node = next_node;
                    self.made_progress = true;
                    self.should_try_join = false;
                    self.assigned_values.extend(self.unassigned_value.take());
                } else {
                    tracing::trace!("not continuing with {edge:?} because of priorities");
                    return ReadOneProgress::Ready;
                }
            }
            None => {
                tracing::trace!("edge does not exist");
                // what was I writing anyway?
                if self.unassigned_value.is_some() {
                    if self.should_try_join
                        && should_resume(self.until, self.syntax.maybe_join.binding_power)
                    {
                        tracing::trace!("trying to join");
                        self.should_try_join = false;
                        self.assigned_values
                            .push(self.unassigned_value.take().unwrap());
                        self.current_node = &self.syntax.maybe_join;
                    } else {
                        tracing::trace!("not much else to do");
                        return ReadOneProgress::Ready;
                    }
                } else if self.should_try_join {
                    // means i did nothing
                    tracing::trace!(
                        "did nothing, no need to go deeper, since cant do anything anyway"
                    );
                    return ReadOneProgress::Ready;
                } else {
                    tracing::trace!("trying to read some more values");
                    let progress_before = self.parser.unprocessed.progress();
                    self.unassigned_value = self.parser.read_until(
                        self.syntax,
                        self.unassigned_value.take(),
                        self.min_binding_power_for_inner_values(),
                    );
                    let made_progress = self.parser.unprocessed.progress() > progress_before;
                    if !made_progress {
                        tracing::trace!("no progress could be made");
                        return ReadOneProgress::Ready;
                    }
                }
            }
        }
        tracing::trace!("not ready");
        ReadOneProgress::NotReady
    }

    fn read_one(mut self) -> Option<Ast> {
        loop {
            // let before = self.parser.unprocessed.amount();
            if let ReadOneProgress::Ready = self.progress() {
                return self.finish();
            }
            // if self.parser.unprocessed.amount() == before {
            //     return self.finish();
            // }
        }
    }

    fn min_binding_power_for_inner_values(&self) -> Option<BindingPower> {
        let result = if self.current_node.is_open_paren {
            None
        } else {
            self.current_node.binding_power
        };
        tracing::trace!("inner bp = {result:?}");
        result
    }
}

impl Parser {
    /// Read a single ast node, without trying to combine it with the rest of the tokens
    fn read_one(
        &mut self,
        syntax: &Syntax,
        unassigned_value: Option<Ast>,
        until: Option<BindingPower>,
    ) -> Option<Ast> {
        tracing::trace!(
            "start with unassigned_value={:?} until={until:?}",
            unassigned_value.is_some(),
        );
        if unassigned_value.is_none() {
            match self.unprocessed.peek() {
                None => {}
                Some(TokenOrValue::Value(_)) => {
                    return Some(self.unprocessed.pop_value());
                }
                Some(TokenOrValue::Token(token)) => {
                    if token.raw() == "syntax" {
                        let (def, span) = read_syntax_def(self.unprocessed.reader());
                        return Some(Ast::SyntaxDefinition {
                            def: Arc::new(def),
                            data: span,
                        });
                    }
                    match token.token {
                        Token::Punctuation { .. } => {}
                        Token::Comment { .. } => unreachable!(),
                        _ => {
                            if !syntax.keywords.contains(token.raw()) {
                                let token = self.unprocessed.pop_token();
                                tracing::trace!("got simple {:?}", token.raw());
                                return Some(Ast::Simple {
                                    token: token.token,
                                    data: token.span,
                                });
                            }
                        }
                    }
                }
            }
        }
        ReadOne {
            parser: self,
            syntax,
            made_progress: false,
            unassigned_value,
            until,
            current_node: &syntax.root,
            assigned_values: Vec::new(),
            should_try_join: true,
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
    ) -> Option<Ast> {
        tracing::trace!("starting read until");
        loop {
            tracing::trace!("continuing read one");
            let progress_before = self.unprocessed.progress();
            let one = self.read_one(syntax, unassigned_value, until);
            tracing::trace!("read one = {one:?}");
            let made_progress = self.unprocessed.progress() > progress_before;
            if !made_progress {
                tracing::trace!("no more progress, stop read until");
                return one;
            }
            unassigned_value = one;
        }
    }

    fn read_all(&mut self, syntax: &Syntax) -> Option<Ast> {
        let result = self.read_until(syntax, None, None);
        assert!(self.unprocessed.value.is_none());
        if let Some(token) = self.unprocessed.reader.peek() {
            panic!("unexpected token {:?}", token.raw());
        }
        result
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
                    panic!("same priority different associativity: {until:?} & {with:?}");
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
