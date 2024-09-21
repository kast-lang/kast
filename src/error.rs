#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct ErrorMessage(pub String);

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
