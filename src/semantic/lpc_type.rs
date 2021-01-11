#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCReturnType {
    Void,
    Int(bool),
    Float(bool),
    String(bool),
    Mapping(bool),
    Mixed(bool)
}