// We are finally making some progress

use crate::source::*;
use crate::{error::*, peek2};
use std::{collections::HashMap, path::PathBuf};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("at {filename}:{position} - {message}")]
pub struct Error {
    pub message: String,
    pub filename: PathBuf,
    pub position: Position,
}

pub type Result<T, E = ErrorMessage> = std::result::Result<T, E>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringType {
    SingleQuoted,
    DoubleQuoted,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Token {
    Ident {
        raw: String,
        name: String,
        is_raw: bool,
    },
    Punctuation {
        raw: String,
    },
    String {
        raw: String,
        contents: String,
        typ: StringType,
    },
    Number {
        raw: String,
    },
    Comment {
        raw: String,
        contents: String,
    },
}

impl Token {
    pub fn raw(&self) -> &str {
        match self {
            Token::Ident { raw, .. } => raw,
            Token::Punctuation { raw } => raw,
            Token::String { raw, .. } => raw,
            Token::Number { raw } => raw,
            Token::Comment { raw, .. } => raw,
        }
    }
}

struct Lexer {
    // source: SourceFile,
    reader: peek2::Reader<char>,
    next_recording_id: u64,
    recordings: HashMap<u64, String>,
}

impl Lexer {
    fn next_token(&mut self) -> Result<Option<SpannedToken>, Error> {
        match self.next_token_impl() {
            Ok(result) => Ok(result),
            Err(ErrorMessage(message)) => Err(Error {
                message,
                filename: self.reader.filename().to_owned(),
                position: self.reader.position(),
            }),
        }
    }
    fn next_token_impl(&mut self) -> Result<Option<SpannedToken>> {
        self.skip_whitespace();
        let start = self.reader.position();
        let token = [
            Self::read_comment,
            Self::read_string,
            Self::read_ident,
            Self::read_number,
            Self::read_punctuation,
        ]
        .into_iter()
        .find_map(|f| f(self).transpose())
        .transpose()?;
        if token.is_none() {
            if let Some(c) = self.reader.peek() {
                return error!("Unexpected char {c:?}");
            }
        }
        let end = self.reader.position();
        Ok(token.map(|token| SpannedToken {
            token,
            span: Span {
                start,
                end,
                filename: self.reader.filename().to_owned(),
            },
        }))
    }
    fn skip_whitespace(&mut self) {
        while self.reader.peek().map_or(false, |c| c.is_whitespace()) {
            self.reader.next().unwrap();
        }
    }
    fn skip_char(&mut self, expected: char) -> Result<()> {
        match self.reader.peek() {
            None => error!("expected {expected:?}, got EOF"),
            Some(&actual) if actual == expected => {
                self.reader.next().unwrap();
                Ok(())
            }
            Some(&actual) => error!("expected {expected:?}, got {actual:?}"),
        }
    }

    fn read_while(&mut self, mut f: impl FnMut(char) -> bool) -> Result<String> {
        let mut result = String::new();
        while let Some(&c) = self.reader.peek() {
            if f(c) {
                result.push(c);
                self.reader.next().unwrap();
            } else {
                break;
            }
        }
        Ok(result)
    }
}

struct RecordingToken(u64);

impl Lexer {
    fn start_recording(&mut self) -> RecordingToken {
        let id = self.next_recording_id;
        self.next_recording_id += 1;
        self.recordings.insert(id, String::new());
        RecordingToken(id)
    }
    fn stop_recording(&mut self, token: RecordingToken) -> String {
        self.recordings.remove(&token.0).unwrap()
    }
}

impl Lexer {
    fn read_comment(&mut self) -> Result<Option<Token>> {
        if self.reader.peek() != Some(&'#') {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char('#')?;
        Ok(Some(Token::Comment {
            contents: self.read_while(|c| c != '\n')?,
            raw: self.stop_recording(raw),
        }))
    }
    fn read_string(&mut self) -> Result<Option<Token>> {
        [StringType::SingleQuoted, StringType::DoubleQuoted]
            .into_iter()
            .find_map(|typ| self.read_string_of(typ).transpose())
            .transpose()
    }
    fn read_string_of(&mut self, typ: StringType) -> Result<Option<Token>> {
        let quote_char = match typ {
            StringType::SingleQuoted => '\'',
            StringType::DoubleQuoted => '"',
        };
        if self.reader.peek() != Some(&quote_char) {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char(quote_char)?;
        let mut contents = String::new();
        while let Some(&c) = self.reader.peek() {
            if c == quote_char {
                break;
            }
            self.reader.next().unwrap();
            if c == '\\' {
                contents.push(match self.reader.next() {
                    None => return error!("Expected escaped character, got EOF"),
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('x') => {
                        let mut read_digit = || match self.reader.next() {
                            Some(c) => match c.to_digit(16) {
                                Some(digit) => Ok(digit),
                                None => error!("Expected a hex digit, got {c:?}"),
                            },
                            None => error!("Expected a hex digit, got EOF"),
                        };
                        let digit1 = read_digit()?;
                        let digit2 = read_digit()?;
                        let char_code = digit1 * 16 + digit2;
                        char::from_u32(char_code)
                            .ok_or(error_fmt!("{char_code:?} is not a valid char code"))?
                    }
                    Some(c) => c,
                });
            } else {
                contents.push(c);
            }
        }
        self.skip_char(quote_char)?;
        Ok(Some(Token::String {
            raw: self.stop_recording(raw),
            contents,
            typ,
        }))
    }
    fn read_ident(&mut self) -> Result<Option<Token>> {
        let peeked = match self.reader.peek() {
            Some(&c) => c,
            None => return Ok(None),
        };
        match peeked {
            '@' => {
                let raw = self.start_recording();
                self.reader.next().unwrap();
                let Some(Token::String { contents: name, .. }) = self.read_string()? else {
                    return error!("Expected a string token after '@' for raw identifier");
                };
                Ok(Some(Token::Ident {
                    name,
                    raw: self.stop_recording(raw),
                    is_raw: true,
                }))
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut name = String::new();
                while let Some(&c) = self.reader.peek() {
                    let is_good = |c: char| c.is_alphanumeric() || c == '_';
                    if is_good(c) || c == '-' && self.reader.peek2().map_or(false, |&c| is_good(c))
                    {
                        name.push(c);
                        self.reader.next().unwrap();
                    } else {
                        break;
                    }
                }
                Ok(Some(Token::Ident {
                    raw: name.clone(),
                    name,
                    is_raw: false,
                }))
            }
            _ => Ok(None),
        }
    }
    fn read_number(&mut self) -> Result<Option<Token>> {
        let peeked = match self.reader.peek() {
            Some(&c) => c,
            None => return Ok(None),
        };
        if !peeked.is_ascii_digit() {
            return Ok(None);
        }
        let mut seen_dot = false;
        let raw = self.read_while(|c| {
            c.is_ascii_digit() || c == '.' && !std::mem::replace(&mut seen_dot, true) || c == '_'
        })?;
        Ok(Some(Token::Number { raw }))
    }
    fn read_punctuation(&mut self) -> Result<Option<Token>> {
        let is_single_punctuation = |c: char| "(){}[]".contains(c);
        let is_punctuation =
            |c: char| !(c.is_alphanumeric() || "_'\"".contains(c) || c.is_whitespace());
        match self.reader.peek() {
            Some(&c) if is_punctuation(c) => {
                if is_single_punctuation(c) {
                    self.reader.next().unwrap();
                    Ok(Some(Token::Punctuation { raw: c.to_string() }))
                } else {
                    let raw =
                        self.read_while(|c| is_punctuation(c) && !is_single_punctuation(c))?;
                    Ok(Some(Token::Punctuation { raw }))
                }
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl std::ops::Deref for SpannedToken {
    type Target = Token;
    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

pub fn lex(source: SourceFile) -> impl Iterator<Item = Result<SpannedToken, Error>> {
    let mut parser = Lexer {
        next_recording_id: 0,
        recordings: HashMap::new(),
        reader: peek2::Reader::read(source),
    };
    std::iter::from_fn(move || parser.next_token().transpose())
}
