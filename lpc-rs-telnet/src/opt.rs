//! Option numbers and command bytes.

/// End of subnegotiation.
pub(crate) const SE: u8 = 240;
/// Go ahead: the prompt mark when EOR is not negotiated.
pub(crate) const GA: u8 = 249;
/// Start of subnegotiation.
pub(crate) const SB: u8 = 250;
pub(crate) const WILL: u8 = 251;
pub(crate) const WONT: u8 = 252;
pub(crate) const DO: u8 = 253;
pub(crate) const DONT: u8 = 254;
/// Interpret as command.
pub(crate) const IAC: u8 = 255;
/// The end-of-record command (RFC 885), distinct from the EOR option.
pub(crate) const EOR_CMD: u8 = 239;

/// A telnet option, by number.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Opt {
    /// RFC 857. We echo — or rather, we tell the client to stop — only around a no-echo `input_to`.
    Echo,
    /// RFC 858: suppress go-ahead.
    Sga,
    /// RFC 1091: terminal type. Refused in v1.
    Ttype,
    /// RFC 885: end-of-record prompt marks.
    Eor,
    /// RFC 1073: window size.
    Naws,
    /// RFC 2066: character set.
    Charset,
    /// MUD Server Status Protocol. Supported, not offered.
    Mssp,
    /// MUD Client Compression Protocol v2. Refused in v1.
    Mccp2,
    /// MUD eXtension Protocol.
    Mxp,
    /// Generic MUD Communication Protocol.
    Gmcp,
    /// Any option the session has no policy for.
    Other(u8),
}

impl From<u8> for Opt {
    fn from(byte: u8) -> Self {
        match byte {
            1 => Opt::Echo,
            3 => Opt::Sga,
            24 => Opt::Ttype,
            25 => Opt::Eor,
            31 => Opt::Naws,
            42 => Opt::Charset,
            70 => Opt::Mssp,
            86 => Opt::Mccp2,
            91 => Opt::Mxp,
            201 => Opt::Gmcp,
            other => Opt::Other(other),
        }
    }
}

impl From<Opt> for u8 {
    fn from(opt: Opt) -> Self {
        match opt {
            Opt::Echo => 1,
            Opt::Sga => 3,
            Opt::Ttype => 24,
            Opt::Eor => 25,
            Opt::Naws => 31,
            Opt::Charset => 42,
            Opt::Mssp => 70,
            Opt::Mccp2 => 86,
            Opt::Mxp => 91,
            Opt::Gmcp => 201,
            Opt::Other(byte) => byte,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_byte_round_trips() {
        for byte in 0..=u8::MAX {
            assert_eq!(u8::from(Opt::from(byte)), byte);
        }
    }

    #[test]
    fn known_numbers_are_named() {
        assert_eq!(Opt::from(201), Opt::Gmcp);
        assert_eq!(Opt::from(91), Opt::Mxp);
        assert_eq!(Opt::from(2), Opt::Other(2));
    }
}
