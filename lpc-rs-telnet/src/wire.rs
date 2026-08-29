//! Outbound framing. `&str` payloads cannot contain 0xFF, so only
//! subnegotiation payloads need IAC doubling.

// Until session.rs lands (B5); it removes this.
#![allow(dead_code)]

use bytes::{BufMut, BytesMut};

use crate::opt::{IAC, SB, SE};

/// `IAC <command> <opt>`.
pub(crate) fn negotiate(out: &mut BytesMut, command: u8, opt: u8) {
    out.put_slice(&[IAC, command, opt]);
}

/// `IAC <command>`.
pub(crate) fn command(out: &mut BytesMut, command: u8) {
    out.put_slice(&[IAC, command]);
}

/// `IAC SB <opt> <payload, IAC doubled> IAC SE`.
pub(crate) fn subnegotiation(out: &mut BytesMut, opt: u8, payload: &[u8]) {
    out.reserve(payload.len() + 5);
    out.put_slice(&[IAC, SB, opt]);
    for &byte in payload {
        if byte == IAC {
            out.put_u8(IAC);
        }
        out.put_u8(byte);
    }
    out.put_slice(&[IAC, SE]);
}

/// Text for the client: LF becomes CR LF (an existing CR LF is kept), and
/// with `mxp_literal` the three MXP markup characters become entities.
pub(crate) fn text(out: &mut BytesMut, s: &str, mxp_literal: bool) {
    out.reserve(s.len() + s.len() / 8);
    let mut previous = 0u8;
    for &byte in s.as_bytes() {
        match byte {
            b'\n' if previous != b'\r' => out.put_slice(b"\r\n"),
            b'<' if mxp_literal => out.put_slice(b"&lt;"),
            b'>' if mxp_literal => out.put_slice(b"&gt;"),
            b'&' if mxp_literal => out.put_slice(b"&amp;"),
            other => out.put_u8(other),
        }
        previous = byte;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect(f: impl FnOnce(&mut BytesMut)) -> Vec<u8> {
        let mut out = BytesMut::new();
        f(&mut out);
        out.to_vec()
    }

    #[test]
    fn negotiation_is_iac_command_option() {
        assert_eq!(collect(|o| negotiate(o, 251, 201)), [IAC, 251, 201]);
    }

    #[test]
    fn a_command_is_iac_command() {
        assert_eq!(collect(|o| command(o, 249)), [IAC, 249]);
    }

    #[test]
    fn a_subnegotiation_doubles_iac_in_its_payload() {
        assert_eq!(
            collect(|o| subnegotiation(o, 31, &[0, IAC, 0, 24])),
            [IAC, SB, 31, 0, IAC, IAC, 0, 24, IAC, SE]
        );
    }

    #[test]
    fn lf_becomes_cr_lf() {
        assert_eq!(collect(|o| text(o, "a\nb\n", false)), b"a\r\nb\r\n");
    }

    #[test]
    fn an_existing_cr_lf_is_kept() {
        assert_eq!(collect(|o| text(o, "a\r\nb", false)), b"a\r\nb");
    }

    #[test]
    fn mxp_literal_escapes_markup() {
        assert_eq!(collect(|o| text(o, "a<b>&c", true)), b"a&lt;b&gt;&amp;c");
        assert_eq!(collect(|o| text(o, "a<b>&c", false)), b"a<b>&c");
    }

    #[test]
    fn utf8_passes_through() {
        assert_eq!(collect(|o| text(o, "é", false)), "é".as_bytes());
    }
}
