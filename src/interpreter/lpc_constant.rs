#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LPCConstant {
    Int(i64),
    String(String)
}

impl From<&String> for LPCConstant {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}