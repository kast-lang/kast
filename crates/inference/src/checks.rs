use super::*;

pub type Check<T> = Arc<dyn Fn(&T) -> eyre::Result<CheckResult> + Send + Sync>;

#[must_use]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CheckResult {
    RunAgain,
    Completed,
}

impl<T> Data<T> {
    pub fn run_checks(&mut self) -> eyre::Result<()> {
        // println!("running checks for {:?}", self.description);
        if let Some(value) = &self.value {
            let mut completed = Vec::new();
            for (index, check) in self.checks.iter().enumerate() {
                match check(value)? {
                    CheckResult::RunAgain => {}
                    CheckResult::Completed => {
                        completed.push(index);
                    }
                }
            }
            for idx in completed.into_iter().rev() {
                self.checks.remove(idx);
            }
        }
        Ok(())
    }
}
