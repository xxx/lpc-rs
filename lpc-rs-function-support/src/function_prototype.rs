use std::{
    borrow::Cow,
    fmt::{Display, Formatter},
    sync::Arc,
};

use derive_builder::Builder;
use itertools::Itertools;
use lazy_format::lazy_format;
use lpc_rs_core::{
    RegisterSize, function_arity::FunctionArity, function_flags::FunctionFlags, lpc_path::LpcPath,
    lpc_type::LpcType, mangle::Mangle,
};
use lpc_rs_errors::span::Span;

/// Kinds of functions. Used for codegen.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum FunctionKind {
    /// A plain old normal LPC function, defined in a plain old normal LPC object.
    #[default]
    Local,

    /// A simulated efun, which is a plain old normal LPC function,
    /// defined in a very unique and special LPC object, which
    /// allows to be called as if it were an efun.
    SimulEfun,

    /// An efun. These are implemented in Rust, and can be called from anywhere.
    Efun,

    /// A closure: an anonymous function reached only through a pointer.
    Closure,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local => write!(f, "local"),
            Self::SimulEfun => write!(f, "simul_efun"),
            Self::Efun => write!(f, "efun"),
            Self::Closure => write!(f, "closure"),
        }
    }
}

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone, Eq, PartialEq, Builder)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
#[readonly::make]
pub struct FunctionPrototype {
    /// The name of the function
    #[builder(setter(into))]
    pub name: Cow<'static, str>,

    /// The filename where this function is defined. This is empty for efuns.
    #[builder(setter(into))]
    pub filename: Arc<LpcPath>,

    /// The return type
    pub return_type: LpcType,

    /// The kind of function this is
    #[builder(default)]
    pub kind: FunctionKind,

    /// The arity of the this function
    #[builder(default)]
    pub arity: FunctionArity,

    /// Vector of argument types, used for type checking calls.
    #[builder(default)]
    pub arg_types: Vec<LpcType>,

    /// Which declared parameters are `ref`, parallel to `arg_types`; empty
    /// when none is.
    #[builder(default)]
    pub ref_params: Vec<bool>,

    /// The first position of an efun's by-reference tail: every argument
    /// from here on is an lvalue (`sscanf`'s variables).
    #[builder(default)]
    pub ref_tail: Option<RegisterSize>,

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

impl FunctionPrototype {
    /// Is this the prototype for an efun?
    #[inline]
    pub fn is_efun(&self) -> bool {
        self.kind == FunctionKind::Efun
    }

    /// Whether a call with `len` arguments fits this prototype's arity,
    /// `varargs`, and ellipsis.
    pub fn accepts_arg_count(&self, len: usize) -> bool {
        if len > RegisterSize::MAX as usize {
            return false;
        }
        let len = len as RegisterSize;
        let arity = self.arity;
        let required = arity.num_args - arity.num_default_args;

        match (self.flags.varargs(), self.flags.ellipsis()) {
            (true, true) => true,
            (true, false) => len <= arity.num_args,
            (false, true) => len >= required,
            (false, false) => (required..=arity.num_args).contains(&len),
        }
    }

    /// Whether argument `i` (0-based) is passed by reference.
    pub fn is_ref_param(&self, i: usize) -> bool {
        self.ref_params.get(i).copied().unwrap_or(false)
            || self.ref_tail.is_some_and(|from| i >= usize::from(from))
    }

    /// The lowest by-reference position, if the function takes any.
    pub fn first_ref_param(&self) -> Option<usize> {
        self.ref_params
            .iter()
            .position(|&by_ref| by_ref)
            .or_else(|| self.ref_tail.map(usize::from))
    }
}

impl Mangle for FunctionPrototype {
    fn mangle(&self) -> String {
        // name__return_type__filename__flags__arg_types
        let mut name = self.name.to_string();
        name.push_str("__");
        name.push_str(&self.return_type.mangle());
        name.push_str("__");
        name.push_str(&self.filename.mangle());
        name.push_str("__");
        name.push_str(&self.flags.mangle());
        name.push_str("__");
        name.push_str(&self.arg_types.iter().map(|t| t.mangle()).join("_"));
        name
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn prototype(flags: FunctionFlags) -> FunctionPrototype {
        FunctionPrototypeBuilder::default()
            .name("f")
            .filename(Arc::new(LpcPath::default()))
            .return_type(LpcType::Int(false))
            .arity(FunctionArity {
                num_args: 5,
                num_default_args: 3,
            })
            .flags(flags)
            .build()
            .unwrap()
    }

    fn accepted(flags: FunctionFlags) -> Vec<usize> {
        (0..=6)
            .filter(|&n| prototype(flags).accepts_arg_count(n))
            .collect()
    }

    #[test]
    fn accepts_arg_count_follows_varargs_and_ellipsis() {
        assert_eq!(accepted(FunctionFlags::default()), vec![2, 3, 4, 5]);
        assert_eq!(
            accepted(FunctionFlags::default().with_varargs(true)),
            vec![0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            accepted(FunctionFlags::default().with_ellipsis(true)),
            vec![2, 3, 4, 5, 6]
        );
        assert_eq!(
            accepted(
                FunctionFlags::default()
                    .with_varargs(true)
                    .with_ellipsis(true)
            ),
            vec![0, 1, 2, 3, 4, 5, 6]
        );
        assert!(
            !prototype(FunctionFlags::default().with_ellipsis(true)).accepts_arg_count(usize::MAX)
        );
    }

    fn proto(ref_params: Vec<bool>, ref_tail: Option<RegisterSize>) -> FunctionPrototype {
        FunctionPrototypeBuilder::default()
            .name("f")
            .filename(Arc::new("f.c".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(2))
            .arg_types(vec![LpcType::Int(false), LpcType::Int(false)])
            .ref_params(ref_params)
            .ref_tail(ref_tail)
            .build()
            .unwrap()
    }

    #[test]
    fn a_declared_ref_parameter_is_a_ref_position() {
        let p = proto(vec![false, true], None);
        assert!(!p.is_ref_param(0));
        assert!(p.is_ref_param(1));
        assert!(!p.is_ref_param(2));
        assert_eq!(p.first_ref_param(), Some(1));
    }

    #[test]
    fn a_ref_tail_covers_every_position_from_its_start() {
        let p = proto(vec![false, false], Some(2));
        assert!(!p.is_ref_param(1));
        assert!(p.is_ref_param(2));
        assert!(p.is_ref_param(40));
        assert_eq!(p.first_ref_param(), Some(2));
    }

    #[test]
    fn a_plain_prototype_has_no_ref_position() {
        let p = proto(vec![], None);
        assert!(!p.is_ref_param(0));
        assert_eq!(p.first_ref_param(), None);
    }
}
