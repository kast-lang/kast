use std::sync::atomic::{AtomicU64, Ordering::SeqCst};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(u64);

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

impl Id {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(NEXT_ID.fetch_add(1, SeqCst))
    }
}
