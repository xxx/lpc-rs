use crate::semantic::lpc_type::LPCVarType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub var_type: LPCVarType,
    // pub privacy: LPCPrivacy,

    /// to which scope do i belong?
    pub scope_id: usize
}

impl Symbol {
    pub fn new(name: &str, var_type: LPCVarType) -> Self {
        Self {
            name: String::from(name),
            var_type,
            scope_id: 0
        }
    }
}
