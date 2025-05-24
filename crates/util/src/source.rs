use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub const ZERO: Self = Self {
        index: 0,
        line: 0,
        column: 0,
    };
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub filename: PathBuf,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.filename.to_str() {
            Some(filename) => write!(f, "{filename}")?,
            None => write!(f, "<non-utf8 filename>")?,
        }
        write!(f, ":{}", self.start)?;
        if self.start != self.end {
            write!(f, "~{}", self.end)?;
        }
        Ok(())
    }
}

pub struct SourceFile {
    pub contents: String,
    pub filename: PathBuf,
}
