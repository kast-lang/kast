use super::*;

struct ExecutorImpl {
    inner: async_executor::Executor<'static>,
}

impl Drop for ExecutorImpl {
    fn drop(&mut self) {
        while self.inner.try_tick() {}
        if !self.inner.is_empty() {
            //panic!("executor still has unfinished tasks???");
        }
    }
}

#[derive(Clone)]
pub struct Executor {
    inner: Parc<ExecutorImpl>,
}

pub struct Spawned<T>(
    Parc<async_once_cell::Lazy<eyre::Result<T>, BoxFuture<'static, eyre::Result<T>>>>,
);

impl<T: std::fmt::Display> std::fmt::Display for Spawned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.try_get() {
            None => write!(f, "<not ready>"),
            Some(Ok(value)) => f.write(value),
            Some(Err(_e)) => write!(f, "<errored>"),
        }
    }
}

impl<T> Spawned<T> {
    pub fn ready(value: T) -> Self {
        Self(Parc::new(async_once_cell::Lazy::with_value(Ok(value))))
    }
}

impl<T> Clone for Spawned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> PartialEq for Spawned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Spawned<T> {}

impl<T> std::hash::Hash for Spawned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> PartialOrd for Spawned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Spawned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Spawned<T> {
    pub async fn get(&self) -> eyre::Result<&T> {
        self.0
            .get_unpin()
            .await
            .as_ref()
            .map_err(|e| eyre!("Background task resulted in error: {e}"))
    }
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
    pub fn spawn<T: Send + Sync + 'static>(
        &self,
        f: impl Future<Output = eyre::Result<T>> + Send + 'static,
    ) -> Spawned<T> {
        let (mut sender, receiver) = async_oneshot::oneshot();
        self.inner
            .inner
            .spawn(async move {
                let result = f.await;
                if let Err(e) = &result {
                    tracing::error!("Spawned task failed: {e:?}");
                }
                _ = sender.send(result);
            })
            .detach();
        Spawned(Parc::new(async_once_cell::Lazy::new(
            async move { receiver.await.expect("dropped sender") }.boxed(),
        )))
    }
}
