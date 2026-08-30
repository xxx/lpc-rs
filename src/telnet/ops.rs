use crate::telnet::connection::InputTo;

/// Operations that can be performed on outgoing connections
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionOp {
    /// Send a message to the user
    SendMessage(String),

    /// Set a function to receive the next line of input
    InputTo(InputTo),

    /// Signal to the connection that the server is shutting down.
    Shutdown,

    /// Close the connection once everything queued before this has been sent.
    Close,

    /// A GMCP message; the session drops it while GMCP is off.
    Gmcp {
        /// `Char.Vitals`, `Core.Ping`, …
        package: String,
        /// The body, usually JSON; empty sends the package alone.
        payload: String,
    },

    /// MXP markup sent as written; the session drops it while MXP is off.
    Mxp(String),

    /// Prompt text, then the mark the client negotiated (EOR, GA, or none).
    Prompt(String),

    /// A body was bound to this connection — at login, and at every `exec`.
    Attached,

    /// A command line or `input_to` callback finished behind everything it
    /// queued: run the prompt cycle.
    PromptCycle,
}
