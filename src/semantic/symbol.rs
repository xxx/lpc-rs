use crate::semantic::lpc_type::LPCVarType;
use crate::ast::var_init_node::VarInitNode;
use crate::asm::register::Register;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub type_: LPCVarType,
    pub array: bool,
    // pub privacy: LPCPrivacy,
    pub static_: bool,
    /// which register is tracking this variable?
    pub location: Option<Register>,

    /// to which scope do i belong?
    pub scope_id: usize
}

impl Symbol {
    pub fn new(name: &str, type_: LPCVarType, array: bool) -> Self {
        Self {
            name: String::from(name),
            type_,
            array,
            static_: false,
            location: None,
            scope_id: 0
        }
    }
}

impl From<&VarInitNode> for Symbol {
    fn from(node: &VarInitNode) -> Self {
        Self::new(&node.name, node.type_, node.array)
    }
}
