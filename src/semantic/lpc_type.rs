use std::fmt;
use fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCVarType {
    Int,
    Float,
    String,
    Mapping,
    Mixed
}

impl Display for LPCVarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let output = match self {
            LPCVarType::Int => "int",
            LPCVarType::Float => "float",
            LPCVarType::String => "string",
            LPCVarType::Mapping => "mapping",
            LPCVarType::Mixed => "mixed",
        };
        write!(f, "{}", output)
    }
}

impl From<String> for LPCVarType {
    fn from(str: String) -> Self {
        match str.as_str() {
            "int" => LPCVarType::Int,
            "float" => LPCVarType::Float,
            "string" => LPCVarType::String,
            "mapping" => LPCVarType::Mapping,
            "mixed" => LPCVarType::Mixed,
            _ => panic!("Unknown LPCType. Cannot convert.")
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCReturnType {
    Void,
    Int(bool),
    Float(bool),
    String(bool),
    Mapping(bool),
    Mixed(bool)
}