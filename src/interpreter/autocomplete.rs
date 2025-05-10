use super::*;

pub struct CompletionCandidate {
    pub name: String,
    pub ty: Type,
}

impl Kast {
    pub fn autocomplete<'a>(&'a self, s: &'a str) -> impl Iterator<Item = CompletionCandidate> {
        self.scopes.interpreter.inspect(|locals| {
            locals
                .iter()
                .filter_map(move |(name, place)| {
                    if name.name().contains(s) {
                        Some(CompletionCandidate {
                            name: name.name().to_owned(),
                            ty: place.ty.clone(),
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .into_iter()
        })
    }
}
