//! One `%` conversion of a format string, parsed.

use std::{iter::Peekable, str::Chars};

/// Where the text sits in its field.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Align {
    Right,
    Left,
    Center,
}

/// What a non-negative number is prefixed with.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Sign {
    None,
    Plus,
    Space,
}

/// What fills the field around the text.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Pad {
    Space,
    /// A leading `0` on the width: zeroes, behind a number's sign.
    Zero,
    /// `'X'`: the quoted string, cycled.
    Custom(String),
}

impl Pad {
    /// `n` characters of padding.
    pub(super) fn run(&self, n: usize) -> String {
        match self {
            Pad::Space => " ".repeat(n),
            Pad::Zero => "0".repeat(n),
            Pad::Custom(s) => s.chars().cycle().take(n).collect(),
        }
    }
}

/// A width or precision: a number, or `*` for the next argument.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Size {
    Fixed(usize),
    FromArg,
}

/// How a string is laid out.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Mode {
    Plain,
    /// `=`: word-wrapped into lines of the field width.
    Column,
    /// `#`: newline-separated words laid out in columns, `ls` style.
    Table,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Spec {
    pub align: Align,
    pub sign: Sign,
    pub pad: Pad,
    pub width: Option<Size>,
    pub precision: Option<Size>,
    pub mode: Mode,
    /// `s d i b o x X e E f g G c O`.
    pub conversion: char,
}

/// Why a conversion could not be parsed.
#[derive(Debug, PartialEq, Eq)]
pub(super) enum SpecError {
    Unknown(char),
    Unterminated,
}

/// The digits at the front of `chars`, `first` before them, as a number.
fn number(first: Option<char>, chars: &mut Peekable<Chars>) -> usize {
    let mut n = first.and_then(|c| c.to_digit(10)).unwrap_or(0) as usize;
    while let Some(d) = chars.peek().and_then(|c| c.to_digit(10)) {
        chars.next();
        n = n.saturating_mul(10).saturating_add(d as usize);
    }
    n
}

/// A number or `*`; `None` for neither.
fn size(chars: &mut Peekable<Chars>) -> Option<Size> {
    match chars.peek() {
        Some('*') => {
            chars.next();
            Some(Size::FromArg)
        }
        Some(c) if c.is_ascii_digit() => Some(Size::Fixed(number(None, chars))),
        _ => None,
    }
}

/// The conversion after a `%`, `chars` left after its type letter.
pub(super) fn parse(chars: &mut Peekable<Chars>) -> Result<Spec, SpecError> {
    let mut spec = Spec {
        align: Align::Right,
        sign: Sign::None,
        pad: Pad::Space,
        width: None,
        precision: None,
        mode: Mode::Plain,
        conversion: 's',
    };
    loop {
        let Some(c) = chars.next() else {
            return Err(SpecError::Unterminated);
        };
        match c {
            '-' => spec.align = Align::Left,
            '|' => spec.align = Align::Center,
            '+' => spec.sign = Sign::Plus,
            ' ' => spec.sign = Sign::Space,
            '=' => spec.mode = Mode::Column,
            '#' => spec.mode = Mode::Table,
            // Justified text is laid out as left-aligned.
            '$' => {}
            '\'' => {
                let mut pad = String::new();
                loop {
                    match chars.next() {
                        None => return Err(SpecError::Unterminated),
                        Some('\'') => break,
                        Some('\\') => match chars.next() {
                            Some(escaped) => pad.push(escaped),
                            None => return Err(SpecError::Unterminated),
                        },
                        Some(x) => pad.push(x),
                    }
                }
                spec.pad = Pad::Custom(pad);
            }
            '*' => spec.width = Some(Size::FromArg),
            '.' => spec.precision = Some(size(chars).unwrap_or(Size::Fixed(0))),
            ':' => {
                if chars.peek() == Some(&'0') {
                    spec.pad = Pad::Zero;
                }
                let both = size(chars).unwrap_or(Size::Fixed(0));
                spec.width = Some(both);
                spec.precision = Some(both);
            }
            '0' if spec.width.is_none() => {
                spec.pad = Pad::Zero;
                if let Some(width) = size(chars) {
                    spec.width = Some(width);
                }
            }
            '0'..='9' => spec.width = Some(Size::Fixed(number(Some(c), chars))),
            's' | 'd' | 'i' | 'b' | 'o' | 'x' | 'X' | 'e' | 'E' | 'f' | 'g' | 'G' | 'c' | 'O' => {
                spec.conversion = c;
                return Ok(spec);
            }
            'Q' => {
                spec.conversion = 'O';
                return Ok(spec);
            }
            other => return Err(SpecError::Unknown(other)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parsed(s: &str) -> Spec {
        parse(&mut s.chars().peekable()).unwrap()
    }

    #[test]
    fn a_bare_letter_is_a_right_aligned_field_of_no_width() {
        let spec = parsed("s");
        assert_eq!(
            (spec.align, spec.width, spec.conversion),
            (Align::Right, None, 's')
        );
    }

    #[test]
    fn flags_width_and_precision_combine() {
        let spec = parsed("-|+ 12.3f");
        assert_eq!(spec.align, Align::Center);
        assert_eq!(spec.sign, Sign::Space);
        assert_eq!(spec.width, Some(Size::Fixed(12)));
        assert_eq!(spec.precision, Some(Size::Fixed(3)));
        assert_eq!(spec.conversion, 'f');
    }

    #[test]
    fn a_leading_zero_on_the_width_is_the_zero_pad() {
        let spec = parsed("05d");
        assert_eq!(
            (spec.pad.clone(), spec.width),
            (Pad::Zero, Some(Size::Fixed(5)))
        );
    }

    #[test]
    fn a_quoted_pad_string_may_escape_its_quote() {
        let spec = parsed(r"7'\''s");
        assert_eq!(spec.pad, Pad::Custom("'".into()));
    }

    #[test]
    fn colon_sets_width_and_precision_together() {
        let spec = parsed(":6s");
        assert_eq!(spec.width, Some(Size::Fixed(6)));
        assert_eq!(spec.precision, Some(Size::Fixed(6)));
    }

    #[test]
    fn stars_take_the_sizes_from_the_arguments() {
        let spec = parsed("*.*s");
        assert_eq!(spec.width, Some(Size::FromArg));
        assert_eq!(spec.precision, Some(Size::FromArg));
    }

    #[test]
    fn column_and_table_modes_are_flags() {
        assert_eq!(parsed("=12s").mode, Mode::Column);
        assert_eq!(parsed("-40#s").mode, Mode::Table);
    }

    #[test]
    fn an_unknown_letter_and_a_missing_one_are_distinct_errors() {
        assert_eq!(
            parse(&mut "y".chars().peekable()),
            Err(SpecError::Unknown('y'))
        );
        assert_eq!(
            parse(&mut "5".chars().peekable()),
            Err(SpecError::Unterminated)
        );
    }
}
