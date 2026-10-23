//! Where a call by name resolves.

use lpc_rs_function_support::function_prototype::FunctionPrototype;

/// The function a name resolved to, and where: this program's own or
/// inherited functions, the resident simul-efun object, or the efuns — the
/// order of lexical lookup, followed by otherwise missing functions supplied by
/// the receiving composition; the result decides the call instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Callee<'a> {
    /// Defined in the receiving composition: called in the current object.
    Local(&'a FunctionPrototype),
    /// Found in the simul-efun object's program: called there, through a door.
    SimulEfun(&'a FunctionPrototype),
    /// Implemented by the driver.
    Efun(&'a FunctionPrototype),
}

impl<'a> Callee<'a> {
    /// The prototype, wherever it was found.
    pub fn prototype(self) -> &'a FunctionPrototype {
        match self {
            Self::Local(prototype) | Self::SimulEfun(prototype) | Self::Efun(prototype) => {
                prototype
            }
        }
    }
}

impl AsRef<FunctionPrototype> for Callee<'_> {
    fn as_ref(&self) -> &FunctionPrototype {
        self.prototype()
    }
}
