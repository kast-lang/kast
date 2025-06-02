use std::fmt::Write;

pub trait Formatter: std::fmt::Write {
    fn alternate(&self) -> bool;
}

pub trait FormatterExt: Formatter {
    fn write(&mut self, value: impl std::fmt::Display) -> std::fmt::Result {
        match self.alternate() {
            true => write!(self, "{value:#}"),
            false => write!(self, "{value}"),
        }
    }
}

impl<T: Formatter + ?Sized> FormatterExt for T {}

impl Formatter for std::fmt::Formatter<'_> {
    fn alternate(&self) -> bool {
        std::fmt::Formatter::alternate(self)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum OuterMode {
    Surrounded,
    Free,
}

pub struct Moded<T> {
    pub normal: T,
    pub alternate: T,
}

/// Include writespace, it will be auto trimmed in beginning/end of lines
pub enum Separator<'a> {
    /// a,\nb,\nc
    Seq(&'a str),
    /// a\n+ b\n+ c
    /// differs from seq in before/after newline
    BinOp(&'a str),
    /// | a | b | c
    Before(&'a str),
    /// a; b; c;
    After(&'a str),
}

pub fn padded_items<T>(
    fmt: &mut dyn Formatter,
    items: impl IntoIterator<Item = T>,
    outer_mode: OuterMode,
    sep: Moded<Separator<'_>>,
    mut write_item: impl FnMut(&mut dyn Formatter, T) -> std::fmt::Result,
) -> std::fmt::Result {
    let alternate = fmt.alternate();
    let sep = match alternate {
        false => sep.normal,
        true => sep.alternate,
    };
    let mut f = PadAdapter::new(fmt);
    let f = &mut f;
    let mut first = true;
    for value in items.into_iter() {
        if first && outer_mode == OuterMode::Surrounded {
            writeln!(f)?;
        }
        match sep {
            Separator::Seq(s) | Separator::After(s) => {
                if !first {
                    if alternate {
                        writeln!(f, "{}", s.trim_end())?;
                    } else {
                        write!(f, "{s}")?;
                    }
                }
            }
            Separator::BinOp(s) => {
                if !first {
                    if alternate {
                        writeln!(f)?;
                        write!(f, "{}", s.trim_start())?;
                    } else {
                        write!(f, "{s}")?;
                    }
                }
            }
            Separator::Before(s) => {
                if alternate {
                    if !first {
                        writeln!(f)?;
                    }
                    write!(f, "{}", s.trim_start())?;
                } else if first {
                    write!(f, "{}", s.trim_start())?;
                } else {
                    write!(f, "{s}")?;
                }
            }
        }
        first = false;
        write_item(f, value)?;
    }
    if let Separator::After(s) = sep {
        write!(f, "{}", s.trim_end())?;
    }
    if outer_mode == OuterMode::Surrounded {
        writeln!(f)?;
    }
    Ok(())
}

pub struct PadAdapter<'a, 'b> {
    fmt: &'a mut dyn Formatter,
    padding: &'b str,
    state: State,
}

impl<'a, 'c> PadAdapter<'a, 'c> {
    /// Creates a new pad adapter with default padding.
    pub fn new(fmt: &'a mut dyn Formatter) -> Self {
        Self {
            padding: if fmt.alternate() { "    " } else { "" },
            fmt,
            state: Default::default(),
        }
    }

    /// Creates a new pad adapter with the padding.
    pub fn with_padding(fmt: &'a mut dyn Formatter, padding: &'c str) -> Self {
        Self {
            fmt,
            padding,
            state: Default::default(),
        }
    }

    pub fn alternate(&self) -> bool {
        self.fmt.alternate()
    }
}

impl std::fmt::Write for PadAdapter<'_, '_> {
    fn write_str(&mut self, mut s: &str) -> std::fmt::Result {
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

impl Formatter for PadAdapter<'_, '_> {
    fn alternate(&self) -> bool {
        PadAdapter::alternate(self)
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
