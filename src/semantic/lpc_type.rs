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

impl Display for LPCReturnType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_star = |array: &bool| -> &str { if *array { " *" } else { "" } };

        let type_ = match self {
            LPCReturnType::Void => String::from("void"),
            LPCReturnType::Int(array) => format!("int{}", to_star(array)),
            LPCReturnType::Float(array) =>  format!("float{}", to_star(array)),
            LPCReturnType::String(array) =>  format!("string{}", to_star(array)),
            LPCReturnType::Mapping(array) =>  format!("mapping{}", to_star(array)),
            LPCReturnType::Mixed(array) =>  format!("mixed{}", to_star(array)),
        };

        write!(f, "{}", type_)
    }
}