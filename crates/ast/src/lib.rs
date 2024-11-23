use std::{borrow::Cow, collections::HashSet};

use decursion::FutureExt;
use kast_util::*;

mod lexer;
mod peek2;
mod syntax;

pub use lexer::{is_punctuation, lex, StringType, Token};
pub use syntax::{Associativity, Priority, Syntax, SyntaxDefinition, SyntaxDefinitionPart};

use lexer::*;
use syntax::{BindingPower, Edge};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ast<Data = Span> {
    Simple {
        token: Token,
        data: Data,
    },
    Complex {
        definition: Parc<SyntaxDefinition>,
        values: Tuple<Self>,
        data: Data,
    },
    SyntaxDefinition {
        def: Parc<SyntaxDefinition>,
        data: Data,
    },
    FromScratch {
        next: Option<Box<Self>>,
        data: Data,
    },
}

impl<Data> Ast<Data> {
    pub fn data(&self) -> &Data {
        match self {
            Ast::Simple { data, .. }
            | Ast::Complex { data, .. }
            | Ast::SyntaxDefinition { data, .. }
            | Ast::FromScratch { data, .. } => data,
        }
    }
    pub fn map_data<NewData>(self, f: impl Fn(Data) -> NewData + Copy) -> Ast<NewData> {
        match self {
            Ast::Simple { token, data } => Ast::Simple {
                token,
                data: f(data),
            },
            Ast::Complex {
                definition,
                values,
                data,
            } => Ast::Complex {
                definition,
                values: values.map(|ast| ast.map_data(f)),
                data: f(data),
            },
            Ast::SyntaxDefinition { def, data } => Ast::SyntaxDefinition { def, data: f(data) },
            Ast::FromScratch { next, data } => Ast::FromScratch {
                next: next.map(|ast| Box::new(ast.map_data(f))),
                data: f(data),
            },
        }
    }
    pub fn as_ident(&self) -> Option<&str> {
        match self {
            Ast::Simple {
                token: Token::Ident { name, .. },
                ..
            } => Some(name),
            _ => None,
        }
    }
}

pub trait HasSpan {
    fn span(&self) -> &Span;
}

impl HasSpan for Span {
    fn span(&self) -> &Span {
        self
    }
}

impl<Data: HasSpan> Ast<Data> {
    pub fn show_short(&self) -> impl std::fmt::Display + '_ {
        struct Show<'a, Data>(&'a Ast<Data>);
        impl<Data: HasSpan> std::fmt::Display for Show<'_, Data> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match &self.0 {
                    Ast::Simple { token, data: _ } => write!(f, "token {token}")?,
                    Ast::Complex {
                        definition,
                        values: _,
                        data: _,
                    } => write!(f, "{:?}", definition.name)?,
                    Ast::SyntaxDefinition { def: _, data: _ } => write!(f, "syntax definition")?,
                    Ast::FromScratch { next: _, data: _ } => write!(f, "syntax from scratch")?,
                }
                write!(f, " at {}", self.0.data().span())
            }
        }
        Show(self)
    }
}

impl<Data> std::fmt::Display for Ast<Data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Simple { token, data: _ } => write!(f, "{:?}", token.raw()),
            Ast::Complex {
                definition,
                values,
                data: _,
            } => {
                write!(f, "{}", values.fmt_with_name(&definition.name))
            }
            Ast::SyntaxDefinition { def, data: _ } => write!(f, "syntax {:?}", def.name),
            Ast::FromScratch { next, data: _ } => {
                write!(f, "syntax from scratch")?;
                if let Some(next) = next {
                    write!(f, "{next}")?;
                }
                Ok(())
            }
        }
    }
}

pub fn parse(syntax: &Syntax, source: SourceFile) -> Result<Option<Ast>, Error> {
    let mut parser = Parser {
        reader: lex(source)?,
    };
    let result = parser.read_all(syntax);
    result.map_err(|msg| {
        msg.at(match parser.reader.peek() {
            Some(peek) => peek.span.clone(),
            None => Span {
                start: parser.reader.position(),
                end: parser.reader.position(),
                filename: parser.reader.filename().to_owned(),
            },
        })
    })
}

pub fn read_syntax(source: SourceFile) -> Result<Syntax, Error> {
    let ast = parse(&Syntax::empty(), source)?;
    let mut syntax = Syntax::empty();
    fn collect_syntax_definitions(syntax: &mut Syntax, ast: Ast) -> Result<(), Error> {
        match ast {
            Ast::Simple { .. } => {}
            Ast::Complex {
                definition: _,
                values,
                data: _,
            } => {
                for (_name, value) in values {
                    collect_syntax_definitions(syntax, value)?;
                }
            }
            Ast::SyntaxDefinition { def, data: span } => {
                syntax
                    .insert(def)
                    .map_err(|ErrorMessage(message)| Error { message, span })?;
            }
            Ast::FromScratch { next, data: _ } => {
                if let Some(next) = next {
                    collect_syntax_definitions(syntax, *next)?;
                }
            }
        }
        Ok(())
    }
    if let Some(ast) = ast {
        collect_syntax_definitions(&mut syntax, ast)?;
    }
    Ok(syntax)
}

struct Parser {
    reader: peek2::Reader<SpannedToken>,
}

enum ParsedSyntax {
    Definition(SyntaxDefinition),
    FromScratch,
}

fn read_syntax_def(reader: &mut peek2::Reader<SpannedToken>) -> Result<(ParsedSyntax, Span)> {
    let Span {
        start,
        end: _,
        filename,
    } = match reader.peek().unwrap() {
        token if token.raw() == "syntax" => reader.next().unwrap().span,
        token => return error!("expected a syntax definition, got {}", token.token),
    };
    let name_token = reader.next().expect("expected a name for the syntax");
    if name_token.raw() == "from" {
        let scratch_token = reader.next().expect("expected 'scratch'");
        if scratch_token.raw() != "scratch" {
            return error!("expected 'scratch'");
        }
        return Ok((
            ParsedSyntax::FromScratch,
            Span {
                start,
                end: scratch_token.span.end,
                filename: filename.clone(),
            },
        ));
    }
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
        ParsedSyntax::Definition(SyntaxDefinition {
            name,
            priority,
            associativity,
            parts,
        }),
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
    Keyword(String, Span),
    Value(Ast),
}

impl ProgressPart {
    pub fn span(&self) -> &Span {
        match self {
            ProgressPart::Keyword(_, span) => span,
            ProgressPart::Value(ast) => ast.data(),
        }
    }
    pub fn into_keyword(self) -> Option<String> {
        match self {
            ProgressPart::Keyword(keyword, _) => Some(keyword),
            ProgressPart::Value(_) => None,
        }
    }
    pub fn into_value(self) -> Option<Ast> {
        match self {
            ProgressPart::Keyword(_, _) => None,
            ProgressPart::Value(value) => Some(value),
        }
    }
    fn format(progress: &[Self]) -> impl std::fmt::Display + '_ {
        struct Format<'a>(&'a [ProgressPart]);
        impl std::fmt::Display for Format<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.0.is_empty() {
                    write!(f, "<no progress>")?;
                } else {
                    write!(f, "\"")?;
                    for (index, part) in self.0.iter().enumerate() {
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
        Format(progress)
    }
}

impl std::fmt::Display for ProgressPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Keyword(keyword, _) => write!(f, "{keyword}"),
            Self::Value(_) => write!(f, "_"),
        }
    }
}

/// Bеst viewers on https://www.twitch.tv/kuviman
enum ReadOneResult {
    /// Progress was made, contains the parsed expr
    Progress(Ast),
    /// No progress could be made, contains original start value
    NoProgress(Option<Ast>),
}

impl Parser {
    fn skip_comments(&mut self) {
        while self.reader.peek().unwrap().is_comment() {
            self.reader.next().unwrap();
        }
    }

    /// Read a single ast node (combine start value with smth if given),
    /// without trying to combine it with the rest of the tokens
    ///
    /// If currently parsing an inner value inside another expr,
    /// binding power of the outer expr must be taken into consideration
    async fn read_one(
        &mut self,
        syntax: &Syntax,
        continuation_keywords: &HashSet<&str>,
        start_value: Option<Ast>,
        outer_bp: Option<BindingPower>,
    ) -> Result<ReadOneResult> {
        tracing::trace!(
            "start reading one with outer_bp={outer_bp:?}, start_value={}",
            display_option(&start_value),
        );
        // first lets see if we can just read a simple value (or a syntax def)
        if start_value.is_none() {
            self.skip_comments();
            let peek = self.reader.peek().unwrap();
            let raw = peek.raw();
            if raw == "syntax" {
                tracing::trace!("see syntax keyword, parsing syntax definition...");
                let (parsed, span) = read_syntax_def(&mut self.reader)?;
                match parsed {
                    ParsedSyntax::Definition(def) => {
                        return Ok(ReadOneResult::Progress(Ast::SyntaxDefinition {
                            def: Parc::new(def),
                            data: span,
                        }));
                    }
                    ParsedSyntax::FromScratch => {
                        let next = self
                            .read_expr(&Syntax::empty(), &Default::default(), None)
                            .decurse()
                            .await?;
                        return Ok(ReadOneResult::Progress(Ast::FromScratch {
                            data: Span {
                                start: span.start,
                                end: next
                                    .as_ref()
                                    .map(|next| next.data().end)
                                    .unwrap_or(span.end),
                                filename: span.filename,
                            },
                            next: next.map(Box::new),
                        }));
                    }
                }
            }
            if !syntax.keywords.contains(raw)
                && matches!(
                    peek.token,
                    Token::String { .. } | Token::Number { .. } | Token::Ident { .. }
                )
            {
                let SpannedToken { token, span } = self.reader.next().unwrap();
                tracing::trace!("seeing a simple value {token}");
                return Ok(ReadOneResult::Progress(Ast::Simple { token, data: span }));
            }
        }
        let mut current_node;
        let mut parsed_parts = Vec::new();
        if let Some(start_value) = start_value {
            parsed_parts.push(ProgressPart::Value(start_value));
            current_node = &syntax.root_with_start_value;
        } else {
            current_node = &syntax.root_without_start_value;
        }
        let mut made_progress = false;
        loop {
            self.skip_comments();
            let peek = self.reader.peek().unwrap();
            let raw = peek.raw();
            tracing::trace!("peek = {raw}");
            if let Some(next_node) = current_node
                .next
                .get(&Edge::Keyword(raw.to_owned()))
                .filter(|_| !continuation_keywords.contains(raw))
            {
                if !should_resume(outer_bp, next_node.binding_power) {
                    tracing::trace!("not continuing with keyword {raw:?} because of outer_bp");
                    break;
                }
                tracing::trace!("continuing with keyword {raw:?}");
                parsed_parts.push({
                    let keyword = self.reader.next().unwrap();
                    ProgressPart::Keyword(keyword.token.into_raw(), keyword.span)
                });
                current_node = next_node;
                made_progress = true;
            } else if let Some(next_node) = current_node.next.get(&Edge::Value) {
                if !should_resume(outer_bp, next_node.binding_power) {
                    tracing::trace!("not trying to read a value because of outer_bp");
                    break;
                }
                let (current_bp, inner_continuation_keywords) = if current_node.is_open_paren {
                    tracing::trace!(
                        "since we just opened a paren, we reset bp and continuation keywords",
                    );
                    (None, HashSet::new())
                } else {
                    let mut keywords = continuation_keywords.clone();
                    for edge in next_node.next.keys() {
                        if edge.is_open_bracket() {
                            // TODO maybe is hack?
                            continue;
                        }
                        if let Edge::Keyword(keyword) = edge {
                            keywords.insert(keyword);
                        }
                    }
                    let bp = if !made_progress {
                        next_node.binding_power
                    } else {
                        current_node.binding_power
                    };
                    (bp, keywords)
                };
                tracing::trace!("trying to read a value to continue with");
                tracing::trace!("current_bp={current_bp:?}");
                tracing::trace!("inner_continuation_keywords={inner_continuation_keywords:?})");
                match self
                    .read_expr(syntax, &inner_continuation_keywords, current_bp)
                    .decurse()
                    .await?
                {
                    Some(value) => {
                        tracing::trace!("continuing with a value");
                        parsed_parts.push(ProgressPart::Value(value));
                        current_node = next_node;
                        made_progress = true;
                    }
                    None => {
                        tracing::trace!("did not read a value, stopping");
                        break;
                    }
                }
            } else {
                break;
            }
        }
        if !made_progress {
            tracing::trace!("no progress was made, returning start_value back");
            assert!(parsed_parts.len() <= 1);
            let start_value = parsed_parts.pop().map(|part| match part {
                ProgressPart::Keyword(_, _) => unreachable!(),
                ProgressPart::Value(value) => value,
            });
            return Ok(ReadOneResult::NoProgress(start_value));
        }
        let Some(definition) = &current_node.finish else {
            return error!(
                "Can not finish parsing {}, expected {}",
                ProgressPart::format(&parsed_parts),
                current_node.format_possible_continuations(),
            );
        };
        tracing::trace!("read one finished, collecting progress");
        let span = Span {
            filename: self.reader.filename().to_owned(),
            start: parsed_parts[0].span().start,
            end: parsed_parts.last().unwrap().span().end,
        };
        Ok(ReadOneResult::Progress(Ast::Complex {
            definition: definition.clone(),
            values: assign_progress(definition, parsed_parts).expect("Failed to assign values"),
            data: span,
        }))
    }
}

impl Parser {
    /// Try to read an expr, maybe inside another expr with given binding power
    /// (can only use stronger binding power then)
    async fn read_expr(
        &mut self,
        syntax: &Syntax,
        continuation_keywords: &HashSet<&str>,
        outer_bp: Option<BindingPower>,
    ) -> Result<Option<Ast>> {
        if self.reader.peek().unwrap().is_eof() {
            return Ok(None);
        }
        tracing::trace!("starting to read expr with outer_bp={outer_bp:?}");
        let mut syntax = Cow::Borrowed(syntax);
        let mut already_parsed = None;
        loop {
            tracing::trace!(
                "trying to read one more node with already_parsed={}",
                display_option(&already_parsed),
            );
            match self
                .read_one(&syntax, continuation_keywords, already_parsed, outer_bp)
                .await?
            {
                ReadOneResult::Progress(value) => {
                    if let Ast::SyntaxDefinition { def, data: _ } = &value {
                        let mut new_syntax = syntax.into_owned();
                        new_syntax.insert(def.clone())?;
                        syntax = Cow::Owned(new_syntax);
                    }
                    already_parsed = Some(value);
                }
                ReadOneResult::NoProgress(value) => {
                    match &value {
                        Some(value) => tracing::trace!("read expr - done! parsed {value}"),
                        None => tracing::trace!("read expr - done! nothing was parsed"),
                    }
                    return Ok(value);
                }
            }
        }
    }

    fn read_all(&mut self, syntax: &Syntax) -> Result<Option<Ast>> {
        let result = decursion::run_decursing(self.read_expr(syntax, &HashSet::new(), None))?;
        let peek = self.reader.peek().unwrap();
        if !peek.is_eof() {
            return error!("unexpected token {:?}", peek.raw());
        }
        Ok(result)
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

fn should_resume(outer_bp: Option<BindingPower>, with: Option<BindingPower>) -> bool {
    let Some(outer_bp) = outer_bp else {
        return true;
    };
    match with {
        None => false,
        Some(with) => match outer_bp.priority.cmp(&with.priority) {
            std::cmp::Ordering::Equal => {
                if outer_bp.associativity != with.associativity {
                    panic!("same priority different associativity: {outer_bp:?} & {with:?}");
                }
                match outer_bp.associativity {
                    Associativity::Left => false,
                    Associativity::Right => true,
                }
            }
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
        },
    }
}
