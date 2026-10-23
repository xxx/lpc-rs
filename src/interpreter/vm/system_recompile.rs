//! System-object selectors and compatibility checks for in-place recompilation.

use std::sync::Arc;

use crate::interpreter::{
    process::Process, program::Program, stm::AuthorityView, task_context::TaskContext,
};
use lpc_rs_errors::{LpcError, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SystemTarget {
    Master,
    Simul,
    Both,
}

impl SystemTarget {
    pub(crate) fn parse(value: &str, ctx: &TaskContext) -> Result<Self> {
        let target = match value {
            "master" => Self::Master,
            "simul_efun" => Self::Simul,
            "both" => Self::Both,
            _ => {
                return Err(LpcError::runtime(
                    "object recompilation: expected master, simul_efun, or both",
                ));
            }
        };
        if target != Self::Master && ctx.config().simul_efun_source().is_none() {
            return Err(LpcError::runtime(
                "object recompilation: no simul-efun source configured",
            ));
        }
        Ok(target)
    }

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Master => "master",
            Self::Simul => "simul_efun",
            Self::Both => "both",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SystemView {
    pub master: Arc<Process>,
    pub authority: Arc<AuthorityView>,
    pub simul: Option<Arc<Process>>,
}

pub(crate) fn compatible_exports(old: &Program, new: &Program) -> Result<()> {
    for (name, function) in old.unmangled_functions.iter() {
        if !function.public()
            || name == lpc_rs_core::INIT_PROGRAM
            || name == lpc_rs_core::INIT_GLOBALS
        {
            continue;
        }
        let Some(replacement) = new.unmangled_functions.get(name) else {
            return Err(LpcError::runtime(format!(
                "object recompilation: simul-efun export `{name}` was removed"
            )));
        };
        let a = &function.prototype;
        let b = &replacement.prototype;
        if !replacement.public()
            || a.return_type != b.return_type
            || a.arity != b.arity
            || a.arg_types != b.arg_types
            || a.ref_params != b.ref_params
            || a.ref_tail != b.ref_tail
            || a.flags != b.flags
        {
            return Err(LpcError::runtime(format!(
                "object recompilation: incompatible simul-efun export `{name}`"
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler,
        test_support::{lib_holding, temp_lib_config},
    };

    use super::*;

    #[tokio::test]
    async fn a_simul_export_cannot_be_hidden_by_changing_inherit_visibility() {
        let root = lib_holding(
            "simul-inherit-recompile",
            &[("base.c", "int exported() { return 1; }")],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let old = compiler
            .compile_string("/simul.c", "inherit \"/base\";")
            .await
            .unwrap()
            .program;
        for modifier in ["private", "protected"] {
            let new = compiler
                .compile_string("/simul.c", format!("{modifier} inherit \"/base\";"))
                .await
                .unwrap()
                .program;
            assert_eq!(
                old.lookup_function("exported").unwrap().prototype.flags,
                new.lookup_function("exported").unwrap().prototype.flags
            );
            let error = compatible_exports(&old, &new).unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("incompatible simul-efun export `exported`"),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn private_inherited_helpers_are_not_part_of_the_simul_export_contract() {
        let root = lib_holding(
            "simul-private-helpers-recompile",
            &[("base.c", "int helper() { return 1; }")],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let old = compiler
            .compile_string(
                "/simul.c",
                "private inherit \"/base\"; int exported() { return helper(); }",
            )
            .await
            .unwrap()
            .program;
        let new = compiler
            .compile_string("/simul.c", "int exported() { return 2; }")
            .await
            .unwrap()
            .program;
        compatible_exports(&old, &new).unwrap();
    }
}
