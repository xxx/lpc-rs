//! TTYPE/MTTS capability reports, independent of socket delivery.

/// The supported color encoding, expressed in bits per color.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ColourDepth {
    /// No color or style escapes.
    Plain = 0,
    /// SGR 30–37 and 40–47.
    Ansi8 = 3,
    /// Basic colors plus SGR 90–97 and 100–107.
    Ansi16 = 4,
    /// Xterm's 256-entry palette.
    Indexed = 8,
    /// RGB using semicolon-form SGR.
    TrueColour = 24,
}

impl ColourDepth {
    /// Decode an explicit bit depth; unknown values are rejected.
    pub fn from_bits(bits: i64) -> Option<Self> {
        match bits {
            0 => Some(Self::Plain),
            3 => Some(Self::Ansi8),
            4 => Some(Self::Ansi16),
            8 => Some(Self::Indexed),
            24 => Some(Self::TrueColour),
            _ => None,
        }
    }
}

/// What a client has reported through TTYPE; absent fields are unknown.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TerminalInfo {
    /// The first terminal name, usually the MUD client's name.
    pub client_name: Option<String>,
    /// The second name, usually the terminal emulation.
    pub terminal_type: Option<String>,
    /// The explicit MTTS capability mask, including unrecognized bits.
    pub mtts: Option<u32>,
    /// The best supported profile justified by the client's reports.
    pub colour_depth: Option<ColourDepth>,
}

#[derive(Debug, Default)]
pub(crate) struct Terminal {
    pub(crate) info: TerminalInfo,
    seen: Vec<String>,
    pending: bool,
}

impl Terminal {
    pub(crate) fn start(&mut self) {
        *self = Self::default();
        self.pending = true;
    }

    /// Whether another SEND is needed after this response.
    pub(crate) fn receive(&mut self, payload: &[u8]) -> bool {
        let Some((&0, name)) = payload.split_first() else {
            return false;
        };
        if !self.pending
            || name.is_empty()
            || name.len() > 128
            || !name.iter().all(|b| (32..=126).contains(b))
        {
            return false;
        }
        let name = String::from_utf8_lossy(name).trim().to_ascii_uppercase();
        if name.is_empty() {
            return false;
        }
        self.pending = false;
        if let Some(mask) = name.strip_prefix("MTTS ") {
            if !mask.is_empty()
                && mask.bytes().all(|b| b.is_ascii_digit())
                && let Ok(mask) = mask.parse::<u32>()
            {
                self.info.mtts = Some(mask);
                self.info.colour_depth = Some(if mask & 256 != 0 {
                    ColourDepth::TrueColour
                } else if mask & 8 != 0 {
                    ColourDepth::Indexed
                } else if mask & 1 != 0 {
                    ColourDepth::Ansi8
                } else {
                    ColourDepth::Plain
                });
            } else {
                self.pending = true;
            }
            return false;
        }
        match self.seen.len() {
            0 => self.info.client_name = Some(name.clone()),
            1 => self.info.terminal_type = Some(name.clone()),
            _ => {}
        }
        if self.seen.contains(&name) {
            return false;
        }
        if let Some(depth) = name_depth(&name) {
            self.info.colour_depth = Some(depth);
        }
        self.seen.push(name);
        self.pending = self.seen.len() < 8;
        self.pending
    }
}

fn name_depth(name: &str) -> Option<ColourDepth> {
    if name.ends_with("-TRUECOLOR") {
        Some(ColourDepth::TrueColour)
    } else if name.ends_with("-256COLOR") {
        Some(ColourDepth::Indexed)
    } else {
        match name {
            "DUMB" => Some(ColourDepth::Plain),
            "ANSI" | "VT100" | "LINUX" => Some(ColourDepth::Ansi8),
            "XTERM" | "SCREEN" | "TMUX" => Some(ColourDepth::Ansi16),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn report(terminal: &mut Terminal, name: &str) -> bool {
        terminal.receive(&[&[0], name.as_bytes()].concat())
    }

    #[test]
    fn explicit_mtts_overrides_terminal_name_hints() {
        for (mask, depth) in [(0, 0), (1, 3), (8, 8), (256, 24), (269, 24)] {
            let mut terminal = Terminal::default();
            terminal.start();
            assert!(report(&mut terminal, "Client"));
            assert!(report(&mut terminal, "XTERM-TRUECOLOR"));
            assert!(!report(&mut terminal, &format!("MTTS {mask}")));
            assert_eq!(terminal.info.mtts, Some(mask));
            assert_eq!(terminal.info.colour_depth, ColourDepth::from_bits(depth));
        }
    }

    #[test]
    fn legacy_names_only_claim_known_capabilities() {
        for (name, depth) in [
            ("unknown", None),
            ("Mudlet", None),
            ("dumb", Some(0)),
            ("ANSI", Some(3)),
            ("XTERM", Some(4)),
            ("xterm-256color", Some(8)),
            ("XTERM-TRUECOLOR", Some(24)),
        ] {
            let mut terminal = Terminal::default();
            terminal.start();
            assert!(report(&mut terminal, name));
            assert!(!report(&mut terminal, name));
            assert_eq!(
                terminal.info.colour_depth,
                depth.and_then(ColourDepth::from_bits)
            );
            assert_eq!(terminal.info.terminal_type, terminal.info.client_name);
        }
    }

    #[test]
    fn cycles_and_unbounded_lists_stop() {
        let mut terminal = Terminal::default();
        terminal.start();
        assert!(report(&mut terminal, "CLIENT"));
        assert!(report(&mut terminal, "ANSI"));
        assert!(!report(&mut terminal, "CLIENT"));
        terminal.start();
        for n in 0..8 {
            assert_eq!(report(&mut terminal, &format!("TYPE{n}")), n < 7);
        }
        assert!(!report(&mut terminal, "XTERM-TRUECOLOR"));
        assert_eq!(terminal.info.colour_depth, None);
    }

    #[test]
    fn malformed_and_unsolicited_reports_do_not_set_capabilities() {
        let mut terminal = Terminal::default();
        assert!(!report(&mut terminal, "MTTS 269"));
        terminal.start();
        for payload in [&b""[..], b"\x01ANSI", b"\0", b"\0ANSI\n", &[0, 255]] {
            assert!(!terminal.receive(payload));
            assert_eq!(terminal.info, TerminalInfo::default());
        }
        assert!(!report(&mut terminal, &"A".repeat(129)));
        for mask in ["MTTS -1", "MTTS +256", "MTTS 4294967296", "MTTS nope"] {
            terminal.start();
            assert!(!report(&mut terminal, mask));
            assert_eq!(terminal.info, TerminalInfo::default());
        }
    }
}
