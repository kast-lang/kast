use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

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

pub fn parse(syntax: &Syntax, source: SourceFile) -> Result<VecDeque<Ast>, Error> {
    let filename = source.filename.clone();
    let mut parser = Parser {
        reader: lex(source)?,
        unassigned_values: VecDeque::new(),
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
            position: parser.reader.position(),
        })
}

pub fn read_syntax(source: SourceFile) -> Syntax {
    let mut reader = lex(source).expect("lexing failed");
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
        let def = read_syntax_def(&mut reader).0;
        syntax.insert(Arc::new(def)).unwrap();
    }
    syntax
}

struct Parser {
    unassigned_values: VecDeque<Ast>,
    reader: peek2::Reader<SpannedToken>,
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
    until: Option<BindingPower>,
    current_node: &'a syntax::ParseNode,
    assigned_values: Vec<Ast>,
    already_read_values_before_keyword: bool,
}

#[must_use]
enum ReadOneProgress {
    Ready,
    NotReady,
}

// Bеst viewers on https://www.twitch.tv/kuviman
impl ReadOne<'_> {
    fn finish(self) -> Option<Ast> {
        tracing::trace!(
            "finishing with {:?} unassigned values",
            self.parser.unassigned_values.len(),
        );
        let mut result = None;
        for (amount, finish_definition) in self
            .current_node
            .finish
            .range(..=self.parser.unassigned_values.len())
            .rev()
        {
            if should_resume(self.until, Some(finish_definition.binding_power())) {
                let mut values = self.assigned_values;
                values.extend(self.parser.unassigned_values.drain(..amount));
                result = Some(Some(Ast::Complex {
                    definition: finish_definition.clone(),
                    values: finish_definition.assign_values(values),
                }));
                break;
            }
        }
        let result = match result {
            Some(result) => result,
            None => {
                if !self.made_progress {
                    return None;
                }
                panic!("could not finish");
            }
        };
        match &result {
            Some(result) => tracing::trace!("parsed {result}"),
            None => tracing::trace!("parsed nothing :)"),
        }
        result
    }
    // is this helpful yet?
    fn progress(&mut self) -> ReadOneProgress {
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
                    self.parser.reader.next().unwrap();
                    self.current_node = next_node;
                    self.made_progress = true;
                    self.assigned_values
                        .extend(self.parser.unassigned_values.drain(..));
                    self.already_read_values_before_keyword = false;
                } else {
                    tracing::trace!("not continuing with {edge:?} because of priorities");
                    return ReadOneProgress::Ready;
                }
            }
            None => {
                tracing::trace!("edge does not exist");
                // what was I writing anyway?
                if self.already_read_values_before_keyword || !self.made_progress {
                    return ReadOneProgress::Ready;
                }
                self.parser
                    .read_until(self.syntax, self.min_binding_power_for_inner_values());
                self.already_read_values_before_keyword = true;
            }
        }
        tracing::trace!("not ready");
        ReadOneProgress::NotReady
    }

    fn read(mut self) -> Option<Ast> {
        loop {
            if let ReadOneProgress::Ready = self.progress() {
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
    fn read_one(&mut self, syntax: &Syntax, until: Option<BindingPower>) -> Option<Ast> {
        tracing::trace!("start reading one until {until:?}");
        ReadOne {
            already_read_values_before_keyword: false,
            parser: self,
            syntax,
            made_progress: false,
            until,
            current_node: &syntax.root,
            assigned_values: Vec::new(),
        }
        .read()
    }
}

impl Parser {
    fn read_simple_values(&mut self, syntax: &Syntax) {
        tracing::trace!("reading simple values");
        loop {
            match &self.reader.peek().unwrap().token {
                Token::Eof | Token::Punctuation { .. } => {
                    break;
                }
                Token::Ident { raw, .. } if raw == "syntax" => {
                    let (def, span) = read_syntax_def(&mut self.reader);
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
    }

    /// Read an ast node from the stream of tokens, potentially starting with existing node,
    /// only using binding power stronger than given
    ///
    /// When we have smth like `a + b + c`
    /// read_one will only parse the `a + b`
    /// read_until is trying to combine the first parsed node with the rest
    fn read_until(&mut self, syntax: &Syntax, until: Option<BindingPower>) {
        if self.reader.peek().unwrap().is_eof() {
            return;
        }
        tracing::trace!("read until - start");
        loop {
            tracing::trace!("continuing read one");
            self.read_simple_values(syntax);
            let value = self.read_one(syntax, until);
            match value {
                Some(value) => self.unassigned_values.push_front(value),
                None => break,
            }
        }
        tracing::trace!("read until - done!");
    }

    fn read_all(&mut self, syntax: &Syntax) -> VecDeque<Ast> {
        // TODO take self by &mut
        self.read_until(syntax, None);
        let peek = self.reader.peek().unwrap();
        if !peek.is_eof() {
            panic!("unexpected token {:?}", peek.raw());
        }
        std::mem::take(&mut self.unassigned_values)
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
