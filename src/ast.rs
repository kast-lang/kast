use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

use super::*;
use crate::lexer::*;
use crate::syntax::*;
use crate::tuple::*;
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
                write!(f, "{}", values.fmt_with_name(&definition.name))
            }
            Ast::SyntaxDefinition { def, .. } => write!(f, "syntax {:?}", def.name),
        }
    }
}

pub fn parse(syntax: &Syntax, source: SourceFile) -> Result<VecDeque<Ast>, Error> {
    let mut parser = Parser {
        reader: lex(source)?,
        unassigned_values: VecDeque::new(),
    };
    let result = parser.read_all(syntax);
    result.map_err(|msg| msg.at(parser.reader.peek().unwrap().span.clone()))
}

pub fn read_syntax(source: SourceFile) -> Result<Syntax, Error> {
    let mut reader = lex(source)?;
    let result = (|| {
        let mut syntax = Syntax::empty();
        loop {
            let should_skip = |token: &Token| match token {
                Token::Punctuation { raw } if raw == ";" => true,
                Token::Comment { .. } => true,
                _ => false,
            };
            while should_skip(&reader.peek().unwrap().token) {
                reader.next().unwrap();
            }

            if reader.peek().unwrap().is_eof() {
                break;
            }
            let def = read_syntax_def(&mut reader)?.0;
            syntax.insert(Arc::new(def)).unwrap();
        }
        Ok(syntax)
    })();
    result.map_err(|msg: ErrorMessage| msg.at(reader.peek().unwrap().span.clone()))
}

struct Parser {
    unassigned_values: VecDeque<Ast>,
    reader: peek2::Reader<SpannedToken>,
}

fn read_syntax_def(reader: &mut peek2::Reader<SpannedToken>) -> Result<(SyntaxDefinition, Span)> {
    let Span {
        start,
        end: _,
        filename,
    } = match reader.peek().unwrap() {
        token if token.raw() == "syntax" => reader.next().unwrap().span,
        token => return error!("expected a syntax definition, got {}", token.token),
    };
    let name_token = reader.next().expect("expected a name for the syntax");
    let name = match name_token.token {
        Token::Ident { name, .. } => name,
        _ => return error!("name for the syntax must be an identifier"),
    };
    let associativity = match reader.next().expect("expected a associativity").token {
        Token::Punctuation { raw } if raw == "<-" => Associativity::Left,
        Token::Punctuation { raw } if raw == "->" => Associativity::Right,
        _ => return error!("expected associativity (<- or ->)"),
    };
    let priority = match reader.next().expect("expected a priority").token {
        Token::Number { raw } | Token::String { contents: raw, .. } => {
            Priority::new(match raw.parse() {
                Ok(number) => number,
                Err(e) => return error!("failed to parse priority: {e}"),
            })
        }
        _ => return error!("syntax priority must be a number"),
    };
    if reader.next().map(|spanned| spanned.token.raw().to_owned()) != Some("=".to_owned()) {
        return error!("expected a =");
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
    Ok((
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
    ))
}

// My wife yelled at me for forgetting to lock the front door last night, but later apologized. She
// wanted to be safe, then sorry.

enum ProgressPart {
    Keyword(String),
    Value(Ast),
}

impl ProgressPart {
    pub fn into_keyword(self) -> Option<String> {
        match self {
            ProgressPart::Keyword(keyword) => Some(keyword),
            ProgressPart::Value(_) => None,
        }
    }
    pub fn into_value(self) -> Option<Ast> {
        match self {
            ProgressPart::Keyword(_) => None,
            ProgressPart::Value(value) => Some(value),
        }
    }
}

impl std::fmt::Display for ProgressPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Keyword(keyword) => write!(f, "{keyword}"),
            Self::Value(_) => write!(f, "_"),
        }
    }
}

struct ReadOne<'a> {
    parser: &'a mut Parser,
    syntax: &'a Syntax,
    until: Option<BindingPower>,
    current_node: &'a syntax::ParseNode,
    made_progress: Vec<ProgressPart>,
    already_read_values_before_keyword: bool,
}

#[must_use]
enum ReadOneProgress {
    Ready,
    NotReady,
}

// Bеst viewers on https://www.twitch.tv/kuviman
impl ReadOne<'_> {
    fn format_current_progress(&self) -> impl std::fmt::Display + '_ {
        struct Format<'a>(&'a ReadOne<'a>);
        impl std::fmt::Display for Format<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.0.made_progress.is_empty() {
                    write!(f, "<no progress>")?;
                } else {
                    write!(f, "\"")?;
                    for (index, part) in self.0.made_progress.iter().enumerate() {
                        if index != 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{part}")?;
                    }
                    write!(f, "\"")?;
                }
                Ok(())
            }
        }
        Format(self)
    }
    fn finish(self) -> Result<Option<Ast>> {
        tracing::trace!(
            "finishing with {:?} unassigned values",
            self.parser.unassigned_values.len(),
        );
        for (amount, finish_definition) in self
            .current_node
            .finish
            .range(..=self.parser.unassigned_values.len())
            .rev()
        {
            if should_resume(self.until, Some(finish_definition.binding_power())) {
                let mut made_progress = self.made_progress;
                made_progress.extend(
                    self.parser
                        .unassigned_values
                        .drain(..amount)
                        .map(ProgressPart::Value),
                );
                let result = Ast::Complex {
                    definition: finish_definition.clone(),
                    values: assign_progress(finish_definition, made_progress)
                        .expect("Failed to assign made progress"),
                };
                tracing::trace!("parsed {result}");
                return Ok(Some(result));
            }
        }
        if self.made_progress.is_empty() {
            tracing::trace!("parsed nothing");
            return Ok(None);
        }
        error!(
            "could not finish parsing {}, expected one of {}",
            self.format_current_progress(),
            self.current_node.format_possible_continuations(),
        )
    }
    // is this helpful yet?
    fn progress(&mut self) -> Result<ReadOneProgress> {
        tracing::trace!("continuing with until={:?}", self.until);
        let token = self.parser.reader.peek().unwrap();
        let edge = Edge {
            values_before_keyword: self.parser.unassigned_values.len(),
            keyword: token.raw().to_owned(),
        };
        tracing::trace!("looking up edge {edge:?}");
        match self.current_node.next.get(&edge) {
            Some(next_node) => {
                if should_resume(self.until, next_node.binding_power) {
                    tracing::trace!("continued with {edge:?}");
                    self.made_progress.extend(
                        self.parser
                            .unassigned_values
                            .drain(..)
                            .map(ProgressPart::Value),
                    );
                    self.made_progress.push(ProgressPart::Keyword(
                        self.parser.reader.next().unwrap().token.into_raw(),
                    ));
                    self.current_node = next_node;
                    self.already_read_values_before_keyword = false;
                } else {
                    tracing::trace!("not continuing with {edge:?} because of priorities");
                    return Ok(ReadOneProgress::Ready);
                }
            }
            None => {
                tracing::trace!("edge does not exist");
                // what was I writing anyway?
                if self.already_read_values_before_keyword || self.made_progress.is_empty() {
                    return Ok(ReadOneProgress::Ready);
                }
                self.parser
                    .read_until(self.syntax, self.min_binding_power_for_inner_values())?;
                self.already_read_values_before_keyword = true;
            }
        }
        tracing::trace!("not ready");
        Ok(ReadOneProgress::NotReady)
    }

    fn read(mut self) -> Result<Option<Ast>> {
        loop {
            if let ReadOneProgress::Ready = self.progress()? {
                return self.finish();
            }
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
    fn read_one(&mut self, syntax: &Syntax, until: Option<BindingPower>) -> Result<Option<Ast>> {
        tracing::trace!("start reading one until {until:?}");
        ReadOne {
            already_read_values_before_keyword: false,
            parser: self,
            syntax,
            until,
            current_node: &syntax.root,
            made_progress: Vec::new(),
        }
        .read()
    }
}

impl Parser {
    fn read_simple_values(&mut self, syntax: &Syntax) -> Result<()> {
        tracing::trace!("reading simple values");
        loop {
            match &self.reader.peek().unwrap().token {
                Token::Eof | Token::Punctuation { .. } => {
                    break;
                }
                Token::Ident { raw, .. } if raw == "syntax" => {
                    let (def, span) = read_syntax_def(&mut self.reader)?;
                    self.unassigned_values.push_back(Ast::SyntaxDefinition {
                        def: Arc::new(def),
                        data: span,
                    });
                }
                Token::Comment { .. } => {
                    // ignore
                    self.reader.next().unwrap();
                }
                token => {
                    if syntax.keywords.contains(token.raw()) {
                        break;
                    }
                    let token = self.reader.next().unwrap();
                    tracing::trace!("got simple {:?}", token.raw());
                    self.unassigned_values.push_back(Ast::Simple {
                        token: token.token,
                        data: token.span,
                    });
                }
            }
        }
        tracing::trace!("reading simple values - done!");
        Ok(())
    }

    /// Read an ast node from the stream of tokens, potentially starting with existing node,
    /// only using binding power stronger than given
    ///
    /// When we have smth like `a + b + c`
    /// read_one will only parse the `a + b`
    /// read_until is trying to combine the first parsed node with the rest
    fn read_until(&mut self, syntax: &Syntax, until: Option<BindingPower>) -> Result<()> {
        if self.reader.peek().unwrap().is_eof() {
            return Ok(());
        }
        tracing::trace!("read until - start");
        loop {
            tracing::trace!("continuing read one");
            self.read_simple_values(syntax)?;
            let value = self.read_one(syntax, until)?;
            match value {
                Some(value) => {
                    tracing::trace!("read one = {value}");
                    self.unassigned_values.push_front(value)
                }
                None => break,
            }
        }
        tracing::trace!("read until - done!");
        Ok(())
    }

    fn read_all(&mut self, syntax: &Syntax) -> Result<VecDeque<Ast>> {
        self.read_until(syntax, None)?;
        let peek = self.reader.peek().unwrap();
        if !peek.is_eof() {
            return error!("unexpected token {:?}", peek.raw());
        }
        Ok(std::mem::take(&mut self.unassigned_values))
    }
}

fn assign_progress(
    definition: &SyntaxDefinition,
    values: impl IntoIterator<Item = ProgressPart>,
) -> Result<Tuple<Ast>> {
    let mut result = Tuple::empty();
    let mut progress = values.into_iter();
    for part in &definition.parts {
        let progress = progress
            .next()
            .ok_or_else(|| error_fmt!("not enough progress was made"))?;
        match part {
            SyntaxDefinitionPart::Keyword(expected) => {
                assert_eq!(
                    expected.as_str(),
                    progress
                        .into_keyword()
                        .ok_or_else(|| error_fmt!("expected a keyword"))?
                        .as_str(),
                );
            }
            SyntaxDefinitionPart::UnnamedBinding => {
                result.add_unnamed(
                    progress
                        .into_value()
                        .ok_or_else(|| error_fmt!("expected a value"))?,
                );
            }
            SyntaxDefinitionPart::NamedBinding(name) => {
                result.add_named(
                    name.clone(),
                    progress
                        .into_value()
                        .ok_or_else(|| error_fmt!("expected a value"))?,
                );
            }
        }
    }
    if progress.next().is_some() {
        return error!("too many values");
    }
    Ok(result)
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
