use super::*;

struct ExecutorImpl {
    inner: async_executor::Executor<'static>,
}

impl Drop for ExecutorImpl {
    fn drop(&mut self) {
        while self.inner.try_tick() {}
        if !self.inner.is_empty() {
            panic!("executor still has unfinished tasks???");
        }
    }
}

#[derive(Clone)]
pub struct Executor {
    inner: Parc<ExecutorImpl>,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            inner: Parc::new(ExecutorImpl {
                inner: async_executor::Executor::new(),
            }),
        }
    }
    pub fn advance(&self) -> eyre::Result<()> {
        while self.inner.inner.try_tick() {}
        Ok(())
    }
    pub fn spawn(&self, f: impl Future<Output = eyre::Result<()>> + Send + 'static) {
        self.inner
            .inner
            .spawn(f.map_err(|e| tracing::error!("{e:?}")))
            .detach();
    }
}
