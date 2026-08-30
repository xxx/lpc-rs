#![forbid(unsafe_code)]

use decorum::Total;
use indextree::NodeId;

pub mod call_namespace;
pub mod function_arity;
#[allow(clippy::identity_op)]
pub mod function_flags;
pub mod function_receiver;
#[allow(clippy::identity_op)]
pub mod global_var_flags;
pub mod lpc_path;
pub mod lpc_type;
#[allow(clippy::identity_op)]
pub mod lpc_type_union;
pub mod mangle;
#[allow(clippy::identity_op)]
pub mod pragma_flags;
pub mod register;
pub mod register_counter;
pub mod visibility;

/// Name of the user-overridable initializer function for objects
pub const CREATE_FUNCTION: &str = "create";

/// Name of the function for initialization of a program's global variables.
/// Note, this name cannot be parsed, so the user is unable to override it.
pub const INIT_PROGRAM: &str = "init-program";

/// Name of a program's own global-variable initializer; unparseable, so
/// never user-overridden.
pub const INIT_GLOBALS: &str = "init-globals";

/// Reserved efun inherit namespace
pub const EFUN: &str = "efun";

/// The type used for the number of Registers we allow. This limit is also used for
/// the number of local variables (per function), arguments (per function), and
/// global variables (per Program).
/// This can ostensibly be any size, up to and including the platform's `usize`.
/// To go beyond that, a lot of type coercion code will need to be updated.
pub type RegisterSize = u16;

/// Abstracted type to use as in-game `int`s
pub type LpcIntInner = i64;

/// The base float-type that in-game `float`s are backed by
pub type BaseFloat = f64;

/// Abstracted type for in-game `float`s. The wrapper is to handle hashing,
/// ordering, etc.
pub type LpcFloatInner = Total<BaseFloat>;

pub type ScopeId = NodeId;

/// Decode an LPC string literal's backslash escapes into their real characters.
pub fn convert_escapes<T>(s: T) -> String
where
    T: AsRef<str>,
{
    let s = s.as_ref();
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('t') => out.push('\t'),
            Some('v') => out.push('\x0B'),
            Some('f') => out.push('\x0C'),
            Some('a') => out.push('\x07'),
            Some('b') => out.push('\x08'),
            Some('"') => out.push('"'),
            Some('\\') => out.push('\\'),
            Some('\'') => out.push('\''),
            Some(other) => out.push(other),
            None => out.push('\\'),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_decodes_an_escaped_quote() {
        assert_eq!(convert_escapes(r#"\""#), "\"");
    }

    #[test]
    fn it_decodes_an_escaped_backslash() {
        assert_eq!(convert_escapes(r"\\"), "\\");
    }

    #[test]
    fn an_escaped_backslash_before_n_is_not_a_newline() {
        assert_eq!(convert_escapes(r"\\n"), "\\n");
    }

    #[test]
    fn it_decodes_vertical_tab_as_0x0b() {
        assert_eq!(convert_escapes(r"\v"), "\x0B");
    }

    #[test]
    fn an_unknown_escape_drops_the_backslash() {
        assert_eq!(convert_escapes(r"\q"), "q");
    }

    #[test]
    fn a_trailing_lone_backslash_is_kept() {
        assert_eq!(convert_escapes(r"\"), "\\");
    }

    #[test]
    fn a_multibyte_char_after_the_backslash_drops_the_backslash() {
        assert_eq!(convert_escapes("\\é"), "é");
    }
}
