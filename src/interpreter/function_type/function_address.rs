use std::{
    fmt::{Display, Formatter},
    ops::Deref,
    sync::{Arc, Weak},
};

use educe::Educe;
use lpc_rs_core::{RegisterSize, mangle::Mangle};
use ustr::Ustr;

use crate::interpreter::program::Function;
use crate::interpreter::{
    process::{Process, ProgramImage},
    stm::{TxnHandle, VarId},
};

/// Different ways to store a function address, for handling at runtime.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(Weak<Process>, LocalFunction),

    /// The receiver isn't known until called (i.e. the `&->foo()` syntax)
    Dynamic(Ustr),

    /// The function being called is an efun, and requires the name.
    Efun(Ustr),

    /// The function being called is a simulated efun, and requires the name.
    SimulEfun(Ustr),
}

/// A pointer's binding survives its weak receiver for structural address comparison.
#[derive(Debug, Clone)]
pub struct LocalFunction {
    pub function: Function,
    globals: RegisterSize,
    aliases: Arc<[RegisterSize]>,
    generation: VarId,
    symbol: Ustr,
    retained: Option<Arc<ProgramImage>>,
}

impl Deref for LocalFunction {
    type Target = Function;
    fn deref(&self) -> &Function {
        &self.function
    }
}

impl LocalFunction {
    pub(crate) fn resolve(&self, image: &ProgramImage) -> lpc_rs_errors::Result<Function> {
        if self.retained.is_some()
            || self.function.is_driver_code()
            || image.generation == self.generation
        {
            return Ok(self.function.clone());
        }
        // The symbol includes the declaring source, argument/return types and flags.
        let compatible = image.program.function(self.symbol).filter(|new| {
            let old = &self.function.prototype;
            let new = &new.prototype;
            old.kind == new.kind
                && old.arity == new.arity
                && old.ref_params == new.ref_params
                && old.ref_tail == new.ref_tail
        });
        compatible.cloned().ok_or_else(|| lpc_rs_errors::LpcError::runtime(format!(
            "function pointer target `{}` is missing or incompatible after object recompilation",
            self.function.name()
        )))
    }

    pub(crate) fn retained_image(&self) -> Option<&Arc<ProgramImage>> {
        self.retained.as_ref()
    }
}

impl Display for LocalFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.function.fmt(f)
    }
}

impl PartialEq for LocalFunction {
    fn eq(&self, other: &Self) -> bool {
        self.function.projected(self.globals, &self.aliases)
            == other.function.projected(other.globals, &other.aliases)
    }
}

impl Eq for LocalFunction {}

impl FunctionAddress {
    /// Bind a named function for compatible updates or retain a closure's original image.
    pub(crate) fn local(receiver: &Arc<Process>, function: Function, txn: &TxnHandle) -> Self {
        let image = receiver.image(txn);
        Self::in_image(receiver, function, &image)
    }

    pub(crate) fn in_image(
        receiver: &Arc<Process>,
        function: Function,
        image: &Arc<ProgramImage>,
    ) -> Self {
        Self::Local(
            Arc::downgrade(receiver),
            LocalFunction {
                symbol: ustr::ustr(&function.prototype.mangle()),
                retained: function.is_closure().then(|| image.clone()),
                function,
                globals: image.program.num_globals,
                aliases: image.program.global_views.clone(),
                generation: image.generation,
            },
        )
    }

    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    #[inline]
    pub fn function_name(&self) -> &str {
        match self {
            FunctionAddress::Local(_, x) => x.name(),
            FunctionAddress::Dynamic(x)
            | FunctionAddress::Efun(x)
            | FunctionAddress::SimulEfun(x) => x,
        }
    }
}

impl PartialEq for FunctionAddress {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FunctionAddress::Local(_, x), FunctionAddress::Local(_, y)) => x == y,
            (FunctionAddress::Dynamic(x), FunctionAddress::Dynamic(y)) => x == y,
            (FunctionAddress::Efun(x), FunctionAddress::Efun(y)) => x == y,
            (FunctionAddress::SimulEfun(x), FunctionAddress::SimulEfun(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for FunctionAddress {}

impl Display for FunctionAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionAddress::Local(owner, x) => {
                if let Some(owner) = owner.upgrade() {
                    write!(f, "{}::{}", owner, x)
                } else {
                    write!(f, "<destructed>::{}", x)
                }
            }
            FunctionAddress::Dynamic(x) => write!(f, "dynamic::{x}"),
            FunctionAddress::Efun(x) => write!(f, "efun::{x}"),
            FunctionAddress::SimulEfun(x) => write!(f, "simul_efun::{x}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};
    use lpc_rs_function_support::{
        function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunctionBuilder,
    };

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    fn local_addresses() -> (Arc<Process>, FunctionAddress, FunctionAddress) {
        let program = ProgramBuilder::default()
            .filename(LpcPath::in_game("/target".into()))
            .build()
            .unwrap();
        let receiver = Arc::new(Process::new(program));
        let bind = |name: &'static str| {
            let prototype = FunctionPrototypeBuilder::default()
                .name(name)
                .filename(Arc::new(LpcPath::in_game("/target.c".into())))
                .return_type(LpcType::Void)
                .build()
                .unwrap();
            let function = ProgramFunctionBuilder::default()
                .prototype(prototype)
                .build()
                .unwrap();
            FunctionAddress::local(&receiver, Arc::new(function).into(), &TxnHandle::empty())
        };
        let first = bind("first");
        let second = bind("second");
        (receiver, first, second)
    }

    #[test]
    fn local_identity_and_diagnostics_survive_the_weak_receiver() {
        let (receiver, first, second) = local_addresses();
        let FunctionAddress::Local(owner, local) = &first else {
            unreachable!();
        };
        let FunctionAddress::Local(_, other) = &second else {
            unreachable!();
        };
        assert_eq!(local, &local.clone());
        assert_ne!(local, other);
        assert!(local.to_string().contains("first"));
        assert!(first.to_string().starts_with(&format!("{receiver}::")));

        drop(receiver);
        assert!(owner.upgrade().is_none());
        assert_eq!(local, &local.clone());
        assert_ne!(local, other);
        assert!(first.to_string().starts_with("<destructed>::"));
        assert!(first.to_string().contains("first"));
    }

    #[test]
    fn address_equality_uses_both_kind_and_function_name() {
        let (_receiver, first, second) = local_addresses();
        let addresses = [
            first,
            FunctionAddress::Dynamic(ustr::ustr("first")),
            FunctionAddress::Efun(ustr::ustr("first")),
            FunctionAddress::SimulEfun(ustr::ustr("first")),
        ];
        let different_names = [
            second,
            FunctionAddress::Dynamic(ustr::ustr("second")),
            FunctionAddress::Efun(ustr::ustr("second")),
            FunctionAddress::SimulEfun(ustr::ustr("second")),
        ];
        for (i, address) in addresses.iter().enumerate() {
            assert_eq!(address.function_name(), "first");
            assert_ne!(address, &different_names[i]);
            for (j, other) in addresses.iter().enumerate() {
                assert_eq!(address == other, i == j, "{address:?} and {other:?}");
            }
        }
        for (address, expected) in
            addresses[1..]
                .iter()
                .zip(["dynamic::first", "efun::first", "simul_efun::first"])
        {
            assert_eq!(address.to_string(), expected);
        }
    }
}
