pub fn display_option<T>(option: &Option<T>) -> DisplayOption<'_, T> {
    DisplayOption(option)
}

pub struct DisplayOption<'a, T>(&'a Option<T>);

impl<T: std::fmt::Display> std::fmt::Display for DisplayOption<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(value) => value.fmt(f),
            None => write!(f, "<None>"),
        }
    }
}
