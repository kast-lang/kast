use super::*;
use colored::Colorize;

pub struct Helper(Arc<Mutex<Kast>>);

pub trait AnyHelper: rustyline::Helper {}

impl AnyHelper for () {}
impl AnyHelper for Helper {}

pub struct CompletionCandidate {
    display: String,
    replacement: String,
}

impl rustyline::completion::Candidate for CompletionCandidate {
    fn display(&self) -> &str {
        &self.display
    }
    fn replacement(&self) -> &str {
        &self.replacement
    }
}

impl rustyline::completion::Completer for Helper {
    type Candidate = CompletionCandidate;
    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let start = line[..pos]
            .rfind(|c| kast::ast::is_punctuation(c) || c.is_whitespace())
            .map(|i| i + 1)
            .unwrap_or(0);
        let part = &line[start..pos];
        let kast = self.0.lock().unwrap();
        let completions = kast
            .autocomplete(part)
            .map(|candidate| CompletionCandidate {
                display: format!("{} :: {}", candidate.name, candidate.ty),
                replacement: candidate.name,
            })
            .collect();
        Ok((start, completions))
    }
}

impl rustyline::hint::Hinter for Helper {
    type Hint = String;
}

impl rustyline::highlight::Highlighter for Helper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> std::borrow::Cow<'l, str> {
        match kast::ast::lex(SourceFile {
            contents: line.to_owned(),
            filename: "<stdin>".into(),
        }) {
            Ok(mut reader) => {
                let mut result = String::new();
                let mut line = line.chars();
                let mut current_position = 0;
                while let Some(token) = reader.next() {
                    while current_position < token.span.start.index {
                        current_position += 1;
                        result.push(line.next().unwrap());
                    }
                    let colored_token = match token.token {
                        Token::Ident { raw, .. } => raw.underline(),
                        Token::Punctuation { raw, .. } => raw.normal(),
                        Token::String(kast::ast::StringToken { raw, .. }) => raw.green(),
                        Token::Number { raw, .. } => raw.italic(),
                        Token::Comment { raw, .. } => raw.dimmed(),
                        Token::Eof => break,
                    };
                    result += &colored_token.to_string();
                    while current_position < token.span.end.index {
                        current_position += 1;
                        line.next().unwrap();
                    }
                }
                result.into()
            }
            Err(_e) => line.red().to_string().into(),
        }
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> std::borrow::Cow<'b, str> {
        prompt.bold().dimmed().to_string().into()
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
        hint.dimmed().to_string().into()
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: rustyline::CompletionType,
    ) -> std::borrow::Cow<'c, str> {
        let _ = completion;
        std::borrow::Cow::Borrowed(candidate)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        let _ = (line, pos, forced);
        true
    }
}

impl rustyline::validate::Validator for Helper {}

impl rustyline::Helper for Helper {}

impl Helper {
    pub fn new(kast: Arc<Mutex<Kast>>) -> Self {
        Self(kast)
    }
}
