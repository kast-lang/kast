use std::sync::atomic::{AtomicU64, Ordering::SeqCst};

#[derive(Debug, Ord, PartialOrd, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(u64);

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

impl Id {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(NEXT_ID.fetch_add(1, SeqCst))
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
