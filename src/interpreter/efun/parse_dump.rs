//! `parse_dump`: every registered rule, with its owner, as text.

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_dump()`: every rule in the driver, one line each,
/// `"{verb} {rule}  ({owner})"`; a rule whose owner is dead is skipped.
pub fn parse_dump<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let rules = VerbRules::new(context.task_context()).all();
    let out: String = rules
        .iter()
        .filter_map(|rule| {
            let owner = rule.owner()?;
            let protocol = rule.protocol()?;
            let path = LpcPath::new_server(&*owner.filename())
                .as_in_game(&*context.config().lib_dir)
                .to_string_lossy()
                .into_owned();
            Some(format!("{} {}  ({path})\n", rule.verb, protocol.rule))
        })
        .collect();
    context.return_efun_result(LpcRef::from(out));
    Ok(())
}
