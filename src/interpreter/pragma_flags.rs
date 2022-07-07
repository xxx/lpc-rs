use modular_bitfield::prelude::*;

pub const NO_CLONE: &str = "no_clone";
pub const NO_INHERIT: &str = "no_inherit";
pub const NO_SHADOW: &str = "no_shadow";
pub const RESIDENT: &str = "resident";
pub const STRICT_TYPES: &str = "strict_types";

/// A struct to track which `pragma`s have been enabled in a
/// [`Program`](crate::interpreter::program::Program).
#[bitfield(filled = false)]
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct PragmaFlags {
    /// Disallow a [`Program`](crate::interpreter::program::Program)
    /// from being cloned at runtime via `clone_object`
    pub no_clone: bool,

    /// Disallow a [`Program`](crate::interpreter::program::Program)
    /// from being `inherit`ed from
    pub no_inherit: bool,

    /// A large bludgeon to prevent shadowing via both `shadow` directly, as well
    /// as overriding any of the functions within, as if all functions were declared
    /// as `nomask`.
    pub no_shadow: bool,

    /// Prevent the master object from being unloaded from memory, even if all clones
    /// have been removed from the game. This should not be used for normal in-game
    /// objects, but is useful for e.g. singleton daemons that run in the background.
    pub resident: bool,

    /// When true, enforce explicit return types on functions at compile time.
    /// When false, functions can be declared without a return type,
    /// which will be treated as `mixed`.
    pub strict_types: bool,
}
