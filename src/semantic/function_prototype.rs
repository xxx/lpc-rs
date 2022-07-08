use crate::{
    core::{function_arity::FunctionArity, lpc_type::LpcType},
    parser::span::Span,
    semantic::function_flags::FunctionFlags,
};
use itertools::Itertools;
use lazy_format::lazy_format;
use std::{
    borrow::Cow,
    fmt::{Display, Formatter},
};
use serde::{Deserialize, Serialize};

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

impl Display for FunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let flags = self.flags;

        let nomask = lazy_format!(
            if flags.nomask() => ("{}", "nomask ")
            else ("")
        );

        let varargs = lazy_format!(
            if flags.varargs() => ("{}", "varargs ")
            else ("")
        );

        let visibility = lazy_format!("{} ", self.flags.visibility());

        let mut args = self.arg_types.iter().map(|t| t.to_string()).join(", ");

        if self.flags.ellipsis() {
            args.push_str(", ...");
        }

        write!(
            f,
            "{}{}{}{} {}({})",
            nomask, varargs, visibility, self.return_type, self.name, args
        )
    }
}

impl AsRef<FunctionPrototype> for FunctionPrototype {
    fn as_ref(&self) -> &Self {
        self
    }
}
