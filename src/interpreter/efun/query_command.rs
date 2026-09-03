use lpc_rs_errors::Result;
use lpc_rs_utils::lpc_string::LpcString;

use crate::interpreter::efun::efun_context::EfunContext;

/// `query_command`, an efun returning the whole line of the command in
/// progress (after `process_input`); 0 outside a command.
pub async fn query_command<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let line = context
        .task_context()
        .with_command(|state| state.map(|state| state.line.clone()));
    if let Some(line) = line {
        context.return_efun_result(LpcString::from(line.as_str()).into());
    }
    Ok(())
}
