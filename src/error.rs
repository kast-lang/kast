use crate::source::Span;

#[derive(Debug, thiserror::Error)]
#[error("at {span} - {message}")]
pub struct Error {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct ErrorMessage(pub String);

impl ErrorMessage {
    pub fn at(self, span: Span) -> Error {
        Error {
            message: self.0,
            span,
        }
    }
}

macro_rules! error_fmt {
    ($($f:tt)*) => {
        ErrorMessage(format!($($f)*))
    };
}

macro_rules! error {
    ($($f:tt)*) => {
        Err(error_fmt!($($f)*))
    };
}
