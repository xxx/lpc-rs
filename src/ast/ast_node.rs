use core::fmt::Debug;

pub trait ASTNode {
    fn to_str(&self) -> String;
}

impl ASTNode for Box<dyn ASTNode> {
    fn to_str(&self) -> String {
        self.as_ref().to_str()
    }
}

impl Debug for dyn ASTNode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
