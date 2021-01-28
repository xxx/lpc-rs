use crate::semantic::scope::Scope;
use crate::errors::VarRedefinitionError;
use crate::ast::var_init_node::VarInitNode;

/// Check if a var has already been defined in the local scope.
///
/// # Arguments
///
/// * `name` - The name to check
/// * `scope` - The scope to check
///
/// # Returns
///
/// A `Result` with either `Ok(true)` or `Err(<error string>)`
pub fn check_var_redefinition<'a>(node: &'_ VarInitNode, scope: &'a Scope)
    -> Result<bool, VarRedefinitionError<'a>> {
    if let Some(sym) = scope.lookup(&node.name) {
        Err(VarRedefinitionError {
            symbol: &sym,
            span: node.span
        })
    } else {
        Ok(true)
    }
}