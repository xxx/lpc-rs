use lpc_rs_telnet::ColourDepth;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Colour {
    Indexed(u8),
    Rgb([u8; 3]),
}

const ANSI: [[u8; 3]; 16] = [
    [0, 0, 0],
    [128, 0, 0],
    [0, 128, 0],
    [128, 128, 0],
    [0, 0, 128],
    [128, 0, 128],
    [0, 128, 128],
    [192, 192, 192],
    [128, 128, 128],
    [255, 0, 0],
    [0, 255, 0],
    [255, 255, 0],
    [0, 0, 255],
    [255, 0, 255],
    [0, 255, 255],
    [255, 255, 255],
];

fn palette(n: u8) -> [u8; 3] {
    match n {
        0..16 => ANSI[n as usize],
        16..232 => {
            let n = n - 16;
            let level = |v: u8| if v == 0 { 0 } else { 55 + v * 40 };
            [level(n / 36), level(n / 6 % 6), level(n % 6)]
        }
        _ => [8 + (n - 232) * 10; 3],
    }
}

fn nearest(rgb: [u8; 3], range: std::ops::RangeInclusive<u8>) -> u8 {
    range
        .min_by_key(|&n| {
            rgb.into_iter()
                .zip(palette(n))
                .map(|(a, b)| (i32::from(a) - i32::from(b)).pow(2))
                .sum::<i32>()
        })
        .unwrap_or(0)
}

impl Colour {
    fn rgb(self) -> [u8; 3] {
        match self {
            Self::Indexed(n) => palette(n),
            Self::Rgb(rgb) => rgb,
        }
    }

    pub(super) fn sgr(self, depth: ColourDepth, background: bool) -> String {
        let base = if background { 48 } else { 38 };
        match depth {
            ColourDepth::Plain => String::new(),
            ColourDepth::TrueColour if matches!(self, Self::Rgb(_)) => {
                let [r, g, b] = self.rgb();
                format!("{base};2;{r};{g};{b}")
            }
            ColourDepth::TrueColour | ColourDepth::Indexed => {
                let n = match self {
                    Self::Indexed(n) => n,
                    Self::Rgb(rgb) => nearest(rgb, 16..=255),
                };
                format!("{base};5;{n}")
            }
            ColourDepth::Ansi8 | ColourDepth::Ansi16 => {
                let max = if depth == ColourDepth::Ansi8 { 7 } else { 15 };
                let n = match self {
                    Self::Indexed(n) if n <= max => n,
                    _ => nearest(self.rgb(), 0..=max),
                };
                let code = if n < 8 { 30 + n } else { 90 + n - 8 };
                (code + if background { 10 } else { 0 }).to_string()
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct Style {
    pub(super) foreground: Option<Colour>,
    pub(super) background: Option<Colour>,
    flags: u8,
}

impl Style {
    pub(super) fn token(&mut self, name: &str) -> bool {
        match name {
            "RESET" => *self = Self::default(),
            "BOLD" => self.flags |= 1,
            "ITALIC" => self.flags |= 2,
            "UNDERLINE" => self.flags |= 4,
            "FLASH" => self.flags |= 8,
            "REVERSE" => self.flags |= 16,
            "FG_-1" | "INITTERM" | "WINDOW" | "ENDTERM" | "STATUS" => {}
            _ => {
                let (name, background) = if let Some(name) = name.strip_prefix("B_") {
                    (name, true)
                } else if let Some(name) = name.strip_prefix("BG_") {
                    let Ok(n) = name.parse::<u8>() else {
                        return false;
                    };
                    self.background = Some(Colour::Indexed(n));
                    return true;
                } else if let Some(name) = name.strip_prefix("FG_") {
                    let Ok(n) = name.parse::<u8>() else {
                        return false;
                    };
                    self.foreground = Some(Colour::Indexed(n));
                    return true;
                } else {
                    (name, false)
                };
                let colour = match name {
                    "BLACK" => Colour::Indexed(0),
                    "RED" => Colour::Indexed(1),
                    "GREEN" => Colour::Indexed(2),
                    "YELLOW" => Colour::Indexed(3),
                    "BLUE" => Colour::Indexed(4),
                    "MAGENTA" => Colour::Indexed(5),
                    "CYAN" => Colour::Indexed(6),
                    "WHITE" => Colour::Indexed(7),
                    "ORANGE" => Colour::Indexed(208),
                    _ => match hex(name) {
                        Some(rgb) => Colour::Rgb(rgb),
                        None => return false,
                    },
                };
                if background {
                    self.background = Some(colour)
                } else {
                    self.foreground = Some(colour)
                }
            }
        }
        true
    }

    pub(super) fn sgr(&mut self, params: &str) {
        let Some(params) = params
            .split(';')
            .map(|s| {
                if s.is_empty() {
                    Some(0)
                } else {
                    s.parse::<u16>().ok()
                }
            })
            .collect::<Option<Vec<_>>>()
        else {
            return;
        };
        let mut next = *self;
        let mut iter = params.into_iter();
        while let Some(p) = iter.next() {
            match p {
                0 => next = Self::default(),
                1 => next.flags |= 1,
                3 => next.flags |= 2,
                4 => next.flags |= 4,
                5 => next.flags |= 8,
                7 => next.flags |= 16,
                22 => next.flags &= !1,
                23 => next.flags &= !2,
                24 => next.flags &= !4,
                25 => next.flags &= !8,
                27 => next.flags &= !16,
                30..=37 => next.foreground = Some(Colour::Indexed((p - 30) as u8)),
                40..=47 => next.background = Some(Colour::Indexed((p - 40) as u8)),
                90..=97 => next.foreground = Some(Colour::Indexed((p - 90 + 8) as u8)),
                100..=107 => next.background = Some(Colour::Indexed((p - 100 + 8) as u8)),
                39 => next.foreground = None,
                49 => next.background = None,
                38 | 48 => {
                    let colour = match iter.next() {
                        Some(5) => match iter.next().and_then(|n| u8::try_from(n).ok()) {
                            Some(n) => Colour::Indexed(n),
                            None => return,
                        },
                        Some(2) => {
                            let mut rgb = [0; 3];
                            for value in &mut rgb {
                                let Some(n) = iter.next().and_then(|n| u8::try_from(n).ok()) else {
                                    return;
                                };
                                *value = n;
                            }
                            Colour::Rgb(rgb)
                        }
                        _ => return,
                    };
                    if p == 38 {
                        next.foreground = Some(colour)
                    } else {
                        next.background = Some(colour)
                    }
                }
                _ => {}
            }
        }
        *self = next;
    }

    pub(super) fn sequence(self, depth: ColourDepth) -> String {
        if depth == ColourDepth::Plain || self == Self::default() {
            return String::new();
        }
        let mut params = Vec::new();
        for (bit, sgr) in [(1, "1"), (2, "3"), (4, "4"), (8, "5"), (16, "7")] {
            if self.flags & bit != 0 {
                params.push(sgr.to_owned());
            }
        }
        if let Some(colour) = self.foreground {
            params.push(colour.sgr(depth, false));
        }
        if let Some(colour) = self.background {
            params.push(colour.sgr(depth, true));
        }
        format!("\x1b[{}m", params.join(";"))
    }
}

fn hex(name: &str) -> Option<[u8; 3]> {
    let digits = name.strip_prefix('#')?;
    if !matches!(digits.len(), 3 | 6) || !digits.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    let size = digits.len() / 3;
    let mut rgb = [0; 3];
    for (i, value) in rgb.iter_mut().enumerate() {
        *value = u8::from_str_radix(&digits[i * size..(i + 1) * size], 16).ok()?;
        if size == 1 {
            *value *= 17;
        }
    }
    Some(rgb)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rgb_foregrounds_and_backgrounds_degrade_for_every_profile() {
        let red = Colour::Rgb([255, 0, 0]);
        for (bits, fg, bg) in [
            (24, "38;2;255;0;0", "48;2;255;0;0"),
            (8, "38;5;196", "48;5;196"),
            (4, "91", "101"),
            (3, "31", "41"),
            (0, "", ""),
        ] {
            let depth = ColourDepth::from_bits(bits).unwrap();
            assert_eq!(red.sgr(depth, false), fg);
            assert_eq!(red.sgr(depth, true), bg);
        }
    }

    #[test]
    fn grayscale_competes_with_the_cube_and_indices_are_preserved() {
        assert_eq!(
            Colour::Rgb([128; 3]).sgr(ColourDepth::Indexed, false),
            "38;5;244"
        );
        for n in 0..=255 {
            assert_eq!(
                Colour::Indexed(n).sgr(ColourDepth::Indexed, false),
                format!("38;5;{n}")
            );
            assert_eq!(
                Colour::Indexed(n).sgr(ColourDepth::TrueColour, true),
                format!("48;5;{n}")
            );
        }
    }

    #[test]
    fn malformed_extended_sgr_does_not_apply_partial_styles() {
        let mut style = Style::default();
        style.sgr("1;38;2;999;0;0");
        assert_eq!(style, Style::default());
        style.sgr("1;38;5;196;48;2;0;1;2");
        assert_eq!(
            style.sequence(ColourDepth::TrueColour),
            "\x1b[1;38;5;196;48;2;0;1;2m"
        );
        style.sgr("22;39;49");
        assert_eq!(style, Style::default());
    }
}
