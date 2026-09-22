//! The dispatch mechanism that entered the currently executing function.

/// Independent of the object crossings used by `previous_object`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallOrigin {
    /// A direct call, including an inherited definition.
    Local,
    /// An explicit object call, including a call to the same object.
    CallOther,
    /// A simul-efun called by name.
    Simul,
    /// A named or dynamic function pointer called explicitly.
    FunctionPointer,
    /// An anonymous function called explicitly.
    Closure,
    /// A synchronous callback requested by an efun.
    Efun,
    /// An apply, initialization, command, or input event dispatched by the driver.
    Driver,
    /// A scheduled `call_out` callback.
    CallOut,
}

impl CallOrigin {
    /// The stable LPC spelling returned by `origin()`.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Local => "local",
            Self::CallOther => "call_other",
            Self::Simul => "simul_efun",
            Self::FunctionPointer => "function_pointer",
            Self::Closure => "closure",
            Self::Efun => "efun",
            Self::Driver => "driver",
            Self::CallOut => "call_out",
        }
    }
}
