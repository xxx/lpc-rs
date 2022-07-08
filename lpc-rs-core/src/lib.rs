use decorum::Total;

pub mod call_namespace;
pub mod function_arity;
pub mod lpc_type;
#[allow(clippy::identity_op)]
pub mod lpc_type_union;
pub mod register;

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

/// Abstracted type for in-game `float`s. The wrapper is to handle hashing, ordering, etc.
pub type LpcFloat = Total<BaseFloat>;

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