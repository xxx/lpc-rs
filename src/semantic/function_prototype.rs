use crate::semantic::lpc_type::LPCType;
use crate::parser::span::Span;

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    /// The name of the function
    pub name: String,

    /// The return type
    pub return_type: LPCType,

    /// The number of arguments this function accepts.
    /// Varargs are handled elsewhere and are ignored in this count.
    pub num_args: usize,

    /// The number of this function's arguments that have default values set.
    /// This number should always be <= `num_args`.
    pub num_default_args: usize,

    /// Vector of argument types, used for type checking calls.
    pub arg_types: Vec<LPCType>,

    /// The span of the definition of this function, for use in error messaging.
    /// When None, we assume this is an Efun prototype.
    pub span: Option<Span>,

    /// Spans for my arguments
    pub arg_spans: Vec<Span>
}
