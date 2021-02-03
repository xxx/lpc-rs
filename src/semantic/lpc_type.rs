use std::fmt;
use fmt::{Display, Formatter};

/// The enumeration of types that a variable can be declared as.
/// The first bool is whether it's an array, and the second
/// is whether it's actually mixed (i.e. not type-checked).
/// In such cases, the var still has a real type, it's just not
/// checked when something is assigned to it. It still follows 
/// all of the expression rules, however.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCType {
    Void,
    Int(bool, bool),
    Float(bool, bool),
    String(bool, bool),
    Mapping(bool, bool),
}

impl Display for LPCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_star = |array: &bool| -> &str { if *array { " *" } else { "" } };

        let type_ = match self {
            LPCType::Void => String::from("void"),
            LPCType::Int(array, _mixed) => format!("int{}", to_star(array)),
            LPCType::Float(array, _mixed) => format!("float{}", to_star(array)),
            LPCType::String(array, _mixed) => format!("string{}", to_star(array)),
            LPCType::Mapping(array, _mixed) => format!("mapping{}", to_star(array)),
        };

        write!(f, "{}", type_)
    }
}

impl From<String> for LPCType {
    fn from(str: String) -> Self {
        match str.as_str() {
            "void" => LPCType::Void,
            "int" => LPCType::Int(false, false),
            "float" => LPCType::Float(false, false),
            "string" => LPCType::String(false, false),
            "mapping" => LPCType::Mapping(false, false),
            _ => panic!("Unknown LPCType. Cannot convert."),
        }
    }
}
