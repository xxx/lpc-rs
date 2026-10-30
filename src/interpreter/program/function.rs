use std::{fmt, ops::Deref, sync::Arc};

use lpc_rs_core::{
    RegisterSize,
    register::{Register, RegisterVariant},
    visibility::Visibility,
};
use lpc_rs_function_support::program_function::ProgramFunction;

/// Immutable code bound to a global-cell view in its receiver's program.
#[derive(Debug, Clone)]
pub struct Function {
    pub code: Arc<ProgramFunction>,
    // Pack the view (32-bit start, 16-bit count) and access flags into one word.
    binding: u64,
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
        let visibility = code.prototype.flags.visibility() as u64;
        Self {
            code,
            binding: u64::from(start) | (u64::from(count) << 32) | (visibility << 48),
        }
    }

    /// Visibility in this receiver, including inheritance restrictions.
    #[inline]
    pub fn visibility(&self) -> Visibility {
        match (self.binding >> 48) & 3 {
            1 => Visibility::Private,
            2 => Visibility::Protected,
            _ => Visibility::Public,
        }
    }

    /// Whether another object may call this binding.
    #[inline]
    pub fn public(&self) -> bool {
        self.visibility() == Visibility::Public
    }

    pub(crate) fn private(&self) -> bool {
        self.visibility() == Visibility::Private
    }

    /// Hidden declarations retain their parent's dispatch when inherited again.
    pub(crate) fn hidden(&self) -> bool {
        self.binding & (1 << 50) != 0
    }

    pub(crate) fn inherited(&self, start: u32, visibility: Visibility) -> Self {
        let mut function = Self::new(self.code.clone(), start, self.global_count());
        let visibility = self.visibility().restricted_by(visibility) as u64;
        function.binding = (function.binding & !(3 << 48)) | (visibility << 48);
        if self.private() || self.hidden() {
            function.binding |= 1 << 50;
        }
        function
    }

    pub(crate) fn globals_start(&self) -> u32 {
        self.binding as u32
    }

    pub(crate) fn global_count(&self) -> RegisterSize {
        (self.binding >> 32) as RegisterSize
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
        self.code == other.code && self.binding == other.binding
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
    use lpc_rs_asm::instruction::Instruction;
    use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

    use crate::{
        compiler::Compiler,
        interpreter::{function_type::function_address::FunctionAddress, process::Process},
        test_support::{lib_holding, temp_lib_config},
    };

    use super::*;

    fn code(name: &'static str) -> Arc<ProgramFunction> {
        code_with_visibility(name, Visibility::Public)
    }

    fn code_with_visibility(name: &'static str, visibility: Visibility) -> Arc<ProgramFunction> {
        let prototype = FunctionPrototypeBuilder::default()
            .name(name)
            .filename(Arc::new("/binding.c".into()))
            .return_type(LpcType::Int(false))
            .flags(FunctionFlags::default().with_visibility(visibility))
            .build()
            .unwrap();
        let mut code = ProgramFunction::new(prototype, 2);
        code.instructions = vec![
            Instruction::Copy(
                RegisterVariant::Global(Register(0)),
                RegisterVariant::Local(Register(1)),
            ),
            Instruction::Copy(
                RegisterVariant::Global(Register(1)),
                RegisterVariant::Local(Register(2)),
            ),
            Instruction::Copy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Local(Register(0)),
            ),
            Instruction::Ret,
        ];
        code.debug_spans = vec![None; code.instructions.len()];
        Arc::new(code)
    }

    #[test]
    fn projection_uses_canonical_globals_without_changing_shared_code() {
        let code = code("projected");
        let original = code.instructions.clone();
        for (start, aliases, expected) in [
            (1, vec![], [1, 2]),
            (3, vec![2, 0, 1], [2, 0]),
            (4, vec![2, 0, 1], [0, 1]),
        ] {
            let function = Function::new(code.clone(), start, 2);
            let projected = function.projected(3, &aliases);
            assert_eq!(
                projected.instructions,
                [
                    Instruction::Copy(
                        RegisterVariant::Global(Register(expected[0])),
                        RegisterVariant::Local(Register(1))
                    ),
                    Instruction::Copy(
                        RegisterVariant::Global(Register(expected[1])),
                        RegisterVariant::Local(Register(2))
                    ),
                    Instruction::Copy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(0))
                    ),
                    Instruction::Ret,
                ]
            );
            assert_eq!(projected.prototype, code.prototype);
            assert_eq!(code.instructions, original);
        }
    }

    #[test]
    fn equality_includes_code_global_view_and_visibility() {
        let code = code("first");
        let binding = Function::new(code.clone(), 1, 2);
        assert_eq!(binding, binding.clone());
        assert_eq!(binding, Function::new(Arc::new((*code).clone()), 1, 2));
        for different in [
            Function::new(code.clone(), 2, 2),
            Function::new(code.clone(), 1, 3),
            Function::new(self::code("second"), 1, 2),
            binding.inherited(1, Visibility::Private),
        ] {
            assert_ne!(binding, different);
            assert_ne!(different, binding);
        }
    }

    #[test]
    fn private_inheritance_restricts_a_protected_binding() {
        let declared = Function::new(
            code_with_visibility("declared", Visibility::Protected),
            2,
            2,
        );
        let inherited = Function::new(code("inherited"), 0, 2).inherited(2, Visibility::Protected);
        for (protected, declaration) in [
            (declared, Visibility::Protected),
            (inherited, Visibility::Public),
        ] {
            let private = protected.inherited(4, Visibility::Private);
            assert_eq!(protected.visibility(), Visibility::Protected);
            assert_eq!(private.visibility(), Visibility::Private);
            assert_eq!(private.globals_start(), 4);
            assert_eq!(private.global_count(), 2);
            assert!(Arc::ptr_eq(&private.code, &protected.code));
            assert_eq!(private.prototype.flags.visibility(), declaration);
        }
    }

    #[test]
    fn native_frames_accept_the_transaction_null_mode() {
        use lpc_rs_core::null_mode::NullMode;

        use crate::interpreter::{
            call_frame::ENTRY,
            efun::EFUN_PROTOTYPES,
            stm::{Snapshot, Transaction, TxnHandle, Version},
            task::{SeedEntry, TaskSeed},
        };

        let mut snapshot = Snapshot::new(Version::new(), imbl::HashMap::new());
        snapshot.null_mode = NullMode::Distinct;
        let txn = TxnHandle::new(Transaction::new(snapshot));
        let process = Arc::new(Process::default());
        let seed = |function: Function| TaskSeed {
            process: process.clone(),
            entry: SeedEntry::Function(function),
            args: vec![],
            initializes: false,
        };
        let efun = ProgramFunction::new(EFUN_PROTOTYPES["this_object"].clone(), 0);
        for code in [ENTRY.clone(), Arc::new(efun)] {
            let function = Function::from(code);
            seed(function.clone())
                .build_call_frame(process.clone(), function, &txn, None)
                .expect("native frames can enter a transaction with distinct nulls");
        }

        let ordinary = Function::from(Arc::new((**ENTRY).clone()));
        let error = seed(ordinary.clone())
            .build_call_frame(process, ordinary, &txn, None)
            .unwrap_err();
        assert!(error.to_string().contains("different NULL_MODE"), "{error}");
    }

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
