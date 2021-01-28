use crate::semantic::scope::Scope;

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
pub fn check_var_redefinition(name: &str, scope: &Scope) -> Result<bool, String> {
    if scope.contains(name) {
        Err(format!("Redefinition of `{}`", name))
    } else {
        Ok(true)
    }
}