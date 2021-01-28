use crate::semantic::scope::Scope;
use crate::errors::VarRedefinitionError;
use crate::ast::var_init_node::VarInitNode;

/// Check if a var has already been defined in the local scope.
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's a redefinition
/// * `scope` - The scope to check
///
/// # Returns
///
/// A `Result` with either `Ok(())` or `Err(<error object>)`
pub fn check_var_redefinition<'a>(node: &'_ VarInitNode, scope: &'a Scope)
    -> Result<(), VarRedefinitionError<'a>> {
    if let Some(sym) = scope.lookup(&node.name) {
        Err(VarRedefinitionError {
            symbol: &sym,
            span: node.span
        })
    } else {
        Ok(())
    }
}