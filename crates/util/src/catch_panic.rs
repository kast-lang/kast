#[derive(Debug)]
pub struct Catchable(Box<dyn std::error::Error + Send + Sync>);

impl std::fmt::Display for Catchable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Catchable {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}

pub fn catch_panic<R>(f: impl FnOnce() -> R) -> Result<R, Catchable> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => Ok(result),
        Err(e) => match e.downcast() {
            Ok(catched) => Err(*catched),
            Err(e) => std::panic::resume_unwind(e),
        },
    }
}

pub fn throw_catchable(e: impl Into<Box<dyn std::error::Error + Send + Sync + 'static>>) -> ! {
    std::panic::resume_unwind(Box::new(Catchable(e.into())))
}
