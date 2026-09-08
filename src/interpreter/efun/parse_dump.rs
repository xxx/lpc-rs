//! `parse_dump`: every registered rule, with its owner, as text.

use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{
        efun::{efun_context::EfunContext, in_game_name},
        lpc_ref::LpcRef,
    },
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
            let path = in_game_name(context, &owner);
            Some(format!("{} {}  ({path})\n", rule.verb, protocol.rule))
        })
        .collect();
    context.return_efun_result(LpcRef::from(out));
    Ok(())
}
