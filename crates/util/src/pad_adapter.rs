use core::fmt::{self, Formatter, Write};

/// Pad adapter inserts the padding after each newline except the last.
///
/// The default padding is `    `.
pub struct PadAdapter<'a, 'b: 'a, 'c> {
    fmt: &'a mut Formatter<'b>,
    padding: &'c str,
    state: State,
}

impl<'a, 'b: 'a, 'c> PadAdapter<'a, 'b, 'c> {
    /// Creates a new pad adapter with default padding.
    pub fn new(fmt: &'a mut Formatter<'b>) -> Self {
        Self {
            fmt,
            padding: "    ",
            state: Default::default(),
        }
    }

    /// Creates a new pad adapter with the padding.
    pub fn with_padding(fmt: &'a mut Formatter<'b>, padding: &'c str) -> Self {
        Self {
            fmt,
            padding,
            state: Default::default(),
        }
    }

    pub fn alternate(&self) -> bool {
        self.fmt.alternate()
    }

    pub fn write(&mut self, value: impl std::fmt::Display) -> std::fmt::Result {
        match self.fmt.alternate() {
            true => write!(self, "{value:#}"),
            false => write!(self, "{value}"),
        }
    }
}

impl Write for PadAdapter<'_, '_, '_> {
    fn write_str(&mut self, mut s: &str) -> fmt::Result {
        while !s.is_empty() {
            if self.state.on_newline {
                self.fmt.write_str(self.padding)?;
            }
            let split = match s.find('\n') {
                Some(pos) => {
                    self.state.on_newline = true;
                    pos + 1
                }
                None => {
                    self.state.on_newline = false;
                    s.len()
                }
            };
            self.fmt.write_str(&s[..split])?;
            s = &s[split..];
        }
        Ok(())
    }
}

struct State {
    on_newline: bool,
}

impl Default for State {
    fn default() -> Self {
        Self { on_newline: true }
    }
}
