use std::{fmt, ops::Deref, sync::Arc};

use lpc_rs_core::{
    RegisterSize,
    register::{Register, RegisterVariant},
};
use lpc_rs_function_support::program_function::ProgramFunction;

/// Immutable code bound to a global-cell view in its receiver's program.
#[derive(Debug, Clone)]
pub struct Function {
    pub code: Arc<ProgramFunction>,
    // Keep the binding scalar so calls pass a pointer/word pair.
    globals: u64,
}

impl Function {
    pub(crate) fn is_driver_code(&self) -> bool {
        use crate::interpreter::{
            call_frame::ENTRY,
            efun::compose::{COMPOSE_EXECUTOR, COMPOSE_RECEIVER_EXECUTOR},
        };
        self.prototype.is_efun()
            || Arc::ptr_eq(&self.code, &ENTRY)
            || Arc::ptr_eq(&self.code, &COMPOSE_EXECUTOR)
            || Arc::ptr_eq(&self.code, &COMPOSE_RECEIVER_EXECUTOR)
    }

    pub(crate) fn new(code: Arc<ProgramFunction>, start: u32, count: RegisterSize) -> Self {
        Self {
            code,
            globals: u64::from(start) | (u64::from(count) << 32),
        }
    }

    pub(crate) fn globals_start(&self) -> u32 {
        self.globals as u32
    }

    pub(crate) fn global_count(&self) -> RegisterSize {
        (self.globals >> 32) as RegisterSize
    }

    pub(crate) fn projected(
        &self,
        globals: RegisterSize,
        aliases: &[RegisterSize],
    ) -> ProgramFunction {
        let mut code = (*self.code).clone();
        code.rename_registers(|register| match register {
            RegisterVariant::Global(reg) => {
                let index = self.globals_start() as usize + usize::from(reg.index());
                let slot = if index < usize::from(globals) {
                    index as RegisterSize
                } else {
                    aliases[index - usize::from(globals)]
                };
                RegisterVariant::Global(Register(slot))
            }
            other => other,
        });
        code
    }
}

impl From<Arc<ProgramFunction>> for Function {
    fn from(code: Arc<ProgramFunction>) -> Self {
        Self::new(code, 0, 0)
    }
}

impl Deref for Function {
    type Target = ProgramFunction;

    fn deref(&self) -> &Self::Target {
        &self.code
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.code == other.code && self.globals == other.globals
    }
}

impl Eq for Function {}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.code.fmt(f)
    }
}

impl From<ProgramFunction> for Function {
    fn from(code: ProgramFunction) -> Self {
        Arc::new(code).into()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler,
        interpreter::{function_type::function_address::FunctionAddress, process::Process},
        test_support::{lib_holding, temp_lib_config},
    };

    use super::*;

    #[tokio::test]
    async fn addresses_preserve_effective_code_after_receivers_are_dropped() {
        let root = lib_holding(
            "bound-addresses",
            &[
                (
                    "base.c",
                    "int value; int get() { return value; } int unused() { return 9; }",
                ),
                ("padding.c", "int padding;"),
            ],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let a = Arc::new(Process::new(
            compiler
                .compile_string("/a.c", "inherit \"/base\"; inherit \"/padding\";")
                .await
                .unwrap()
                .program,
        ));
        let b = Arc::new(Process::new(
            compiler
                .compile_string("/b.c", "inherit \"/padding\"; inherit \"/base\";")
                .await
                .unwrap()
                .program,
        ));
        let f = a.initial_program().lookup_function("get").unwrap().clone();
        let g = b.initial_program().lookup_function("get").unwrap().clone();
        assert!(Arc::ptr_eq(&f.code, &g.code));
        let unused = Arc::downgrade(&a.initial_program().lookup_function("unused").unwrap().code);
        let first = FunctionAddress::local(&a, f, &crate::interpreter::stm::TxnHandle::default());
        let second = FunctionAddress::local(&b, g, &crate::interpreter::stm::TxnHandle::default());
        assert_ne!(first, second);
        assert_eq!(first, first.clone());
        drop((a, b));
        assert!(unused.upgrade().is_none());
        assert_ne!(first, second);
        assert_eq!(first, first.clone());
    }
}
