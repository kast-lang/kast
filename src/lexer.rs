// We are finally making some progress

use std::{collections::HashMap, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
#[error("{0}")]
pub struct ErrorMessage(String);

#[derive(Debug, Error)]
#[error("at {filename}:{position} - {message}")]
pub struct Error {
    pub message: String,
    pub filename: PathBuf,
    pub position: Position,
}

pub type Result<T, E = ErrorMessage> = std::result::Result<T, E>;

macro_rules! error_fmt {
    ($f:tt) => {
        ErrorMessage(format!($f))
    };
}

macro_rules! error {
    ($f:tt) => {
        Err(error_fmt!($f))
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringType {
    SingleQuoted,
    DoubleQuoted,
}

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

pub struct SourceFile {
    pub contents: Vec<char>,
    pub filename: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

struct Lexer {
    source: SourceFile,
    current_position: Position,
    next_recording_id: u64,
    recordings: HashMap<u64, String>,
}

impl Lexer {
    fn peek(&self) -> Option<char> {
        self.source
            .contents
            .get(self.current_position.index)
            .copied()
    }
    fn peek2(&self) -> Option<char> {
        self.source
            .contents
            .get(self.current_position.index + 1)
            .copied()
    }
    fn next_char(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.advance().unwrap();
        }
        c
    }
    fn next_token(&mut self) -> Result<Option<SpannedToken>, Error> {
        match self.next_token_impl() {
            Ok(result) => Ok(result),
            Err(ErrorMessage(message)) => Err(Error {
                message,
                filename: self.source.filename.clone(),
                position: self.current_position,
            }),
        }
    }
    fn next_token_impl(&mut self) -> Result<Option<SpannedToken>> {
        self.skip_whitespace();
        let start = self.current_position;
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
            if let Some(c) = self.peek() {
                return error!("Unexpected char {c:?}");
            }
        }
        let end = self.current_position;
        Ok(token.map(|token| SpannedToken {
            token,
            span: Span {
                start,
                end,
                filename: self.source.filename.clone(),
            },
        }))
    }
    fn skip_whitespace(&mut self) {
        while self.peek().map_or(false, char::is_whitespace) {
            self.advance().unwrap();
        }
    }
    fn advance(&mut self) -> Result<()> {
        match self.peek() {
            None => return error!("advanced after EOF"),
            Some(c) => {
                for recording in self.recordings.values_mut() {
                    recording.push(c);
                }
                if c == '\n' {
                    self.current_position.line += 1;
                    self.current_position.column = 1;
                } else {
                    self.current_position.column += 1;
                }
            }
        }
        self.current_position.index += 1;
        Ok(())
    }

    fn skip_char(&mut self, expected: char) -> Result<()> {
        match self.peek() {
            None => error!("expected {expected:?}, got EOF"),
            Some(actual) if actual == expected => {
                self.advance()?;
                Ok(())
            }
            Some(actual) => error!("expected {expected:?}, got {actual:?}"),
        }
    }

    fn read_while(&mut self, mut f: impl FnMut(char) -> bool) -> Result<String> {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if f(c) {
                result.push(c);
                self.advance()?;
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
        if self.peek() != Some('#') {
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
        if self.peek() != Some(quote_char) {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char(quote_char)?;
        let mut contents = String::new();
        while let Some(c) = self.peek() {
            if c == quote_char {
                break;
            }
            self.advance().unwrap();
            if c == '\\' {
                contents.push(match self.next_char() {
                    None => return error!("Expected escaped character, got EOF"),
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('x') => {
                        let mut read_digit = || match self.next_char() {
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
        let peeked = match self.peek() {
            Some(c) => c,
            None => return Ok(None),
        };
        match peeked {
            '@' => {
                let raw = self.start_recording();
                self.advance().unwrap();
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
                while let Some(c) = self.peek() {
                    let is_good = |c: char| c.is_alphanumeric() || c == '_';
                    if is_good(c) || c == '-' && self.peek2().map_or(false, is_good) {
                        name.push(c);
                        self.advance().unwrap()
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
        let peeked = match self.peek() {
            Some(c) => c,
            None => return Ok(None),
        };
        if !peeked.is_digit(10) {
            return Ok(None);
        }
        let mut seen_dot = false;
        let raw = self.read_while(|c| {
            c.is_digit(10) || c == '.' && !std::mem::replace(&mut seen_dot, true) || c == '_'
        })?;
        Ok(Some(Token::Number { raw }))
    }
    fn read_punctuation(&mut self) -> Result<Option<Token>> {
        let is_single_punctuation = |c: char| "(){}[]".contains(c);
        let is_punctuation =
            |c: char| !(c.is_alphanumeric() || "_'\"".contains(c) || c.is_whitespace());
        match self.peek() {
            Some(c) if is_punctuation(c) => {
                if is_single_punctuation(c) {
                    self.advance().unwrap();
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub filename: PathBuf,
}

#[derive(Debug)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub fn lex(source: SourceFile) -> impl Iterator<Item = Result<SpannedToken, Error>> {
    let mut parser = Lexer {
        next_recording_id: 0,
        recordings: HashMap::new(),
        source,
        current_position: Position {
            index: 0,
            line: 1,
            column: 1,
        },
    };
    std::iter::from_fn(move || parser.next_token().transpose())
}
