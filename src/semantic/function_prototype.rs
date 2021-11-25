use crate::{
    parser::span::Span,
    semantic::{function_flags::FunctionFlags, lpc_type::LpcType},
};
use std::borrow::Cow;
use crate::interpreter::function_type::FunctionArity;

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionPrototype {
    /// The name of the function
    pub name: Cow<'static, str>,

    /// The return type
    pub return_type: LpcType,

    /// The arity of the this function
    pub arity: FunctionArity,

    // /// The number of arguments this function accepts.
    // /// Varargs are handled elsewhere and are ignored in this count.
    // pub num_args: usize,
    //
    // /// The number of this function's arguments that have default values set.
    // /// This number should always be <= `num_args`.
    // pub num_default_args: usize,

    /// Vector of argument types, used for type checking calls.
    pub arg_types: Vec<LpcType>,

    /// The span of the definition of this function, for use in error messaging.
    /// When None, we assume this is an Efun prototype.
    pub span: Option<Span>,

    /// Spans for my arguments
    pub arg_spans: Vec<Span>,

    /// The flags that are set when the function was declared
    pub flags: FunctionFlags,
}
