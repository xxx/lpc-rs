use std::path::Path;

use decorum::Total;
use fs_err as fs;
use indextree::NodeId;

pub mod call_namespace;
pub mod function;
pub mod function_arity;
#[allow(clippy::identity_op)]
pub mod function_flags;
#[allow(clippy::identity_op)]
pub mod global_var_flags;
pub mod lpc_path;
pub mod lpc_type;
#[allow(clippy::identity_op)]
pub mod lpc_type_union;
#[allow(clippy::identity_op)]
pub mod pragma_flags;
pub mod register;
pub mod register_counter;
pub mod serialize;
pub mod visibility;

/// Name of the user-overridable initializer function for objects
pub const CREATE_FUNCTION: &str = "create";

/// Name of the function for initialization of a program's global variables.
/// Note, this name cannot be parsed, so the user is unable to override it.
pub const INIT_PROGRAM: &str = "init-program";

/// Reserved efun inherit namespace
pub const EFUN: &str = "efun";

/// Abstracted type to use as in-game `int`s
pub type LpcInt = i64;

/// The base float-type that in-game `float`s are backed by
pub type BaseFloat = f64;

/// Abstracted type for in-game `float`s. The wrapper is to handle hashing,
/// ordering, etc.
pub type LpcFloat = Total<BaseFloat>;

pub type ScopeId = NodeId;

/// Convert various literal escapes to actual escape characters
///
/// # Arguments
///
/// `s` - Something that can be represented as a `&str` through `as_ref`.
pub fn convert_escapes<T>(s: T) -> String
where
    T: AsRef<str>,
{
    s.as_ref()
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\v", "\x0F")
        .replace("\\f", "\x0C")
        .replace("\\a", "\x07")
        .replace("\\b", "\x08")
}

/// A convenience helper to handle adding a trailing newline if one isn't there.
/// This is just a thin wrapper around `read_to_string()`
pub fn read_lpc_file<P>(path: P) -> std::io::Result<String>
where
    P: AsRef<Path>,
{
    fs::read_to_string(path).map(|x| if !x.ends_with('\n') { x + "\n" } else { x })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_lpc_file() {
        let with_newline =
            read_lpc_file("./tests/fixtures/newlines/file_ending_with_newline.h").unwrap();
        assert!(with_newline.ends_with('\n'));

        let path_without = "./tests/fixtures/newlines/file_not_ending_with_newline.h";
        assert!(!fs::read_to_string(path_without).unwrap().ends_with('\n'));
        assert!(read_lpc_file(path_without).unwrap().ends_with('\n'));
    }
}
