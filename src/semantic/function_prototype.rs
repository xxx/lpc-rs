use crate::{
    core::lpc_type::LpcType, interpreter::function_type::FunctionArity, parser::span::Span,
    semantic::function_flags::FunctionFlags,
};
use std::borrow::Cow;

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct FunctionPrototype {
    /// The name of the function
    pub name: Cow<'static, str>,

    /// The return type
    pub return_type: LpcType,

    /// The arity of the this function
    pub arity: FunctionArity,

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

impl FunctionPrototype {
    /// Create a new FunctionPrototype
    pub fn new<T>(
        name: T,
        return_type: LpcType,
        arity: FunctionArity,
        flags: FunctionFlags,
        span: Option<Span>,
        arg_types: Vec<LpcType>,
        arg_spans: Vec<Span>,
    ) -> Self
    where
        T: Into<Cow<'static, str>>,
    {
        Self {
            name: name.into(),
            return_type,
            arity,
            arg_types,
            span,
            arg_spans,
            flags,
        }
    }
}
