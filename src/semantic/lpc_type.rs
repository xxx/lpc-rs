use std::fmt;
use fmt::{Display, Formatter};
use std::ops::BitOr;
use std::collections::HashSet;

/// The enumeration of types that a variable can be declared as.
/// The bool is whether it's an array.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCType {
    Void,
    Int(bool),
    Float(bool),
    String(bool),
    Mapping(bool),
    Mixed(bool),

    // To allow efuns to declare prototypes with multiple allowed types
    // Union(HashSet<LPCType>)
}

// impl BitOr for LPCType {
//     type Output = LPCType;
//
//     fn bitor(self, rhs: Self) -> Self::Output {
//         let mut union = if let LPCType::Union(set) = self {
//             set
//         } else {
//             let mut h = HashSet::new();
//             h.insert(self);
//             h
//         };
//
//         let final_union = match rhs {
//             LPCType::Union(set) => union.union(&set).collect(),
//             x => {
//                 union.insert(x);
//                 union
//             }
//         };
//
//         LPCType::Union(final_union)
//     }
// }

impl Display for LPCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_star = |array: &bool| -> &str { if *array { " *" } else { "" } };

        let type_ = match self {
            LPCType::Void => String::from("void"),
            LPCType::Int(array) => format!("int{}", to_star(array)),
            LPCType::Float(array) => format!("float{}", to_star(array)),
            LPCType::String(array) => format!("string{}", to_star(array)),
            LPCType::Mapping(array) => format!("mapping{}", to_star(array)),
            LPCType::Mixed(array) => format!("mixed{}", to_star(array)),
            // LPCType::Union(types) => {
            //     types.iter().map(|typ| format!("{}", typ)).collect().join(" | ");
            // },
        };

        write!(f, "{}", type_)
    }
}

impl From<String> for LPCType {
    fn from(str: String) -> Self {
        match str.as_str() {
            "void" => LPCType::Void,
            "int" => LPCType::Int(false),
            "float" => LPCType::Float(false),
            "string" => LPCType::String(false),
            "mapping" => LPCType::Mapping(false),
            "mixed" => LPCType::Mixed(false),
            _ => panic!("Unknown LPCType. Cannot convert."),
        }
    }
}
