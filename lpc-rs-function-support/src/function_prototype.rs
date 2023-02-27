use std::{
    borrow::Cow,
    fmt::{Display, Formatter},
};

use derive_builder::Builder;
use itertools::Itertools;
use lazy_format::lazy_format;
use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, lpc_type::LpcType,
};
use lpc_rs_errors::span::Span;
use serde::{Deserialize, Serialize};

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Builder)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
#[readonly::make]
pub struct FunctionPrototype {
    /// The name of the function
    #[builder(setter(into))]
    pub name: Cow<'static, str>,

    /// The return type
    pub return_type: LpcType,

    /// The arity of the this function
    #[builder(default)]
    pub arity: FunctionArity,

    /// Vector of argument types, used for type checking calls.
    #[builder(default)]
    pub arg_types: Vec<LpcType>,

    /// The span of the definition of this function, for use in error messaging.
    /// When None, we assume this is an Efun prototype.
    #[builder(default)]
    pub span: Option<Span>,

    /// Spans for my arguments
    #[builder(default)]
    pub arg_spans: Vec<Span>,

    /// The flags that are set when the function was declared
    #[builder(default)]
    pub flags: FunctionFlags,
}

impl Display for FunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let flags = self.flags;

        let nomask = lazy_format!(
            if flags.nomask() => ("{}", "nomask ")
            else => ""
        );

        let varargs = lazy_format!(
            if flags.varargs() => ("{}", "varargs ")
            else => ""
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
