//! `to_bytes` and `to_text`: a string, byte sequence or int array
//! re-expressed through a named encoding.

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{conversions::cannot_convert, efun_context::EfunContext},
    lpc_ref::LpcRef,
};

/// The encodings the driver converts through.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Encoding {
    Utf8,
    Latin1,
}

impl Encoding {
    /// The encoding `name` spells, matched without regard to case.
    fn parse(name: &str) -> Option<Self> {
        match name.to_ascii_uppercase().as_str() {
            "UTF-8" | "UTF8" => Some(Self::Utf8),
            "ISO-8859-1" | "ISO8859-1" | "LATIN1" | "LATIN-1" => Some(Self::Latin1),
            _ => None,
        }
    }

    /// `s` in this encoding; a character the encoding lacks is the error
    /// text.
    fn encode(self, s: &str) -> std::result::Result<Vec<u8>, String> {
        match self {
            Self::Utf8 => Ok(s.as_bytes().to_vec()),
            Self::Latin1 => s
                .chars()
                .map(|c| {
                    u8::try_from(u32::from(c)).map_err(|_| format!("`{c}` is not in ISO-8859-1"))
                })
                .collect(),
        }
    }

    /// `b` decoded; an invalid sequence is the error text.
    fn decode(self, b: &[u8]) -> std::result::Result<String, String> {
        match self {
            Self::Utf8 => std::str::from_utf8(b)
                .map(str::to_owned)
                .map_err(|_| "not valid UTF-8".to_owned()),
            Self::Latin1 => Ok(b.iter().map(|&x| char::from(x)).collect()),
        }
    }
}

/// The encoding argument 1 names, or `None` when the call passed only one
/// argument.
fn encoding_of<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
) -> Result<Option<Encoding>> {
    if context.arg_count() < 2 {
        return Ok(None);
    }
    let arg = context.arg(1);
    let Some(spelling) = arg.as_str() else {
        return Err(context.runtime_error(format!("{name}: {} is not a string", arg.type_name())));
    };
    match Encoding::parse(spelling) {
        Some(encoding) => Ok(Some(encoding)),
        None => Err(context.runtime_error(format!(
            "{name}: unknown encoding `{spelling}`; UTF-8 or ISO-8859-1"
        ))),
    }
}

/// `element` as one byte, or the error text.
fn byte_of(element: &LpcRef) -> std::result::Result<u8, String> {
    match element {
        LpcRef::Int(i) => u8::try_from(i.0).ok(),
        _ => None,
    }
    .ok_or_else(|| format!("{element} is not a byte"))
}

/// `element` as one character, or the error text.
fn char_of(element: &LpcRef) -> std::result::Result<char, String> {
    match element {
        LpcRef::Int(i) => u32::try_from(i.0).ok().and_then(char::from_u32),
        _ => None,
    }
    .ok_or_else(|| format!("{element} is not a character"))
}

/// `array`'s elements as bytes, or the first element's error text.
fn bytes_of<const N: usize>(
    context: &EfunContext<'_, N>,
    array: &LpcRef,
) -> Result<std::result::Result<Vec<u8>, String>> {
    array.with_array(context.txn(), |elements| {
        elements.iter().map(byte_of).collect()
    })
}

/// `array`'s elements as code points, or the first element's error text.
fn text_of<const N: usize>(
    context: &EfunContext<'_, N>,
    array: &LpcRef,
) -> Result<std::result::Result<String, String>> {
    array.with_array(context.txn(), |elements| {
        elements.iter().map(char_of).collect()
    })
}

/// `to_bytes(x [, encoding])`: a string encoded, an int array as byte
/// values or (with an encoding) as code points, a `bytes` as it is.
pub fn to_bytes<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let encoding = encoding_of(context, "to_bytes")?;
    let named = |e: String| context.runtime_error(format!("to_bytes: {e}"));
    let result = match (context.arg(0), encoding) {
        (bytes @ LpcRef::Bytes(_), _) => bytes.clone(),
        (LpcRef::String(s), Some(encoding)) => {
            LpcRef::from(encoding.encode(s.to_str()).map_err(named)?)
        }
        (LpcRef::String(_), None) => {
            return Err(context.runtime_error("to_bytes: a string needs an encoding"));
        }
        (array @ LpcRef::Array(_), None) => LpcRef::from(bytes_of(context, array)?.map_err(named)?),
        (array @ LpcRef::Array(_), Some(encoding)) => {
            let text = text_of(context, array)?.map_err(named)?;
            LpcRef::from(encoding.encode(&text).map_err(named)?)
        }
        (other, _) => return Err(cannot_convert(context, "to_bytes", other)),
    };
    context.return_efun_result(result);
    Ok(())
}

/// `to_text(x [, encoding])`: a `bytes` decoded, an int array as bytes to
/// decode or (without an encoding) as code points, a string as it is.
pub fn to_text<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let encoding = encoding_of(context, "to_text")?;
    let named = |e: String| context.runtime_error(format!("to_text: {e}"));
    let result = match (context.arg(0), encoding) {
        (string @ LpcRef::String(_), _) => string.clone(),
        (LpcRef::Bytes(b), Some(encoding)) => LpcRef::from(encoding.decode(b).map_err(named)?),
        (LpcRef::Bytes(_), None) => {
            return Err(context.runtime_error("to_text: bytes need an encoding"));
        }
        (array @ LpcRef::Array(_), Some(encoding)) => {
            let bytes = bytes_of(context, array)?.map_err(named)?;
            LpcRef::from(encoding.decode(&bytes).map_err(named)?)
        }
        (array @ LpcRef::Array(_), None) => LpcRef::from(text_of(context, array)?.map_err(named)?),
        (other, _) => return Err(cannot_convert(context, "to_text", other)),
    };
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    async fn result_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    async fn error_of(code: &str) -> String {
        try_run_prog(code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn to_bytes_encodes_utf8_and_latin1() {
        let r = result_of(r#"bytes create() { return to_bytes("é", "UTF-8"); }"#).await;
        assert_eq!(r.as_bytes(), Some([0xc3, 0xa9].as_slice()));

        let r = result_of(r#"bytes create() { return to_bytes("é", "latin1"); }"#).await;
        assert_eq!(r.as_bytes(), Some([0xe9].as_slice()));

        let r = result_of(r#"int create() { return to_bytes("é", "ISO-8859-1")[0]; }"#).await;
        assert_eq!(r, LpcRef::from(233));
    }

    #[tokio::test]
    async fn to_bytes_takes_ints_as_bytes() {
        let code = r#"int create() { return to_bytes(({ 104, 105 })) == to_bytes("hi", "utf8"); }"#;
        assert_eq!(result_of(code).await, LpcRef::from(1));

        let err = error_of("bytes create() { return to_bytes(({ 256 })); }").await;
        assert!(err.contains("to_bytes: 256 is not a byte"), "{err}");
    }

    #[tokio::test]
    async fn to_bytes_takes_code_points_with_an_encoding() {
        let r = result_of(r#"int create() { return sizeof(to_bytes(({ 233 }), "utf-8")); }"#).await;
        assert_eq!(r, LpcRef::from(2));

        let err = error_of(r#"bytes create() { return to_bytes(({ 0xd800 }), "utf-8"); }"#).await;
        assert!(err.contains("to_bytes: 55296 is not a character"), "{err}");
    }

    #[tokio::test]
    async fn to_bytes_of_bytes_is_itself() {
        let code = r#"int create() { bytes b = to_bytes("hi", "utf8"); return to_bytes(b) == b; }"#;
        assert_eq!(result_of(code).await, LpcRef::from(1));
    }

    #[tokio::test]
    async fn to_text_decodes() {
        for (expression, expected) in [
            (r#"to_text(to_bytes("héllo", "utf8"), "UTF-8")"#, "héllo"),
            (r#"to_text(to_bytes(({ 233 })), "latin1")"#, "é"),
            (r#"to_text(({ 104, 105 }), "utf8")"#, "hi"),
            (r#"to_text("x")"#, "x"),
            (r#"to_text(({ 233 }))"#, "é"),
        ] {
            let r = result_of(&format!("string create() {{ return {expression}; }}")).await;
            assert_eq!(r.as_str(), Some(expected), "{expression}");
        }
    }

    #[tokio::test]
    async fn to_text_refuses_invalid_utf8() {
        let code = r#"string create() { return to_text(to_bytes(({ 255 })), "utf-8"); }"#;
        let err = error_of(code).await;
        assert!(err.contains("to_text: not valid UTF-8"), "{err}");
    }

    #[tokio::test]
    async fn latin1_cannot_hold_a_wide_character() {
        let err = error_of(r#"bytes create() { return to_bytes("€", "latin1"); }"#).await;
        assert!(err.contains("to_bytes: `€` is not in ISO-8859-1"), "{err}");
    }

    #[tokio::test]
    async fn an_unknown_encoding_is_an_error() {
        let err = error_of(r#"bytes create() { return to_bytes("x", "KOI8-R"); }"#).await;
        assert!(
            err.contains("to_bytes: unknown encoding `KOI8-R`; UTF-8 or ISO-8859-1"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_conversion_that_needs_an_encoding_says_so() {
        let err = error_of(r#"bytes create() { return to_bytes("x"); }"#).await;
        assert!(
            err.contains("to_bytes: a string needs an encoding"),
            "{err}"
        );

        let err = error_of(r#"string create() { return to_text(to_bytes("x", "utf8")); }"#).await;
        assert!(err.contains("to_text: bytes need an encoding"), "{err}");
    }

    #[tokio::test]
    async fn the_encoding_must_be_a_string() {
        let err = error_of(r#"bytes create() { mixed e = 1; return to_bytes("x", e); }"#).await;
        assert!(err.contains("to_bytes: int is not a string"), "{err}");
    }

    #[tokio::test]
    async fn an_unconvertible_value_is_an_error() {
        let err = error_of("bytes create() { mixed m = ([ ]); return to_bytes(m); }").await;
        assert!(
            err.contains("to_bytes: mapping cannot be converted"),
            "{err}"
        );

        let err = error_of("string create() { mixed m = 1.5; return to_text(m); }").await;
        assert!(err.contains("to_text: float cannot be converted"), "{err}");
    }

    #[tokio::test]
    async fn bytesp_tells_bytes_apart() {
        let r = result_of(r#"int create() { return bytesp(to_bytes("x", "utf8")); }"#).await;
        assert_eq!(r, LpcRef::from(1));

        let r = result_of(r#"int create() { return bytesp("x"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }
}
