use std::fmt;
use fmt::{Display, Formatter};
use std::ops::BitOr;
use crate::semantic::lpc_type_union::LPCTypeUnion;

/// The enumeration of types that a variable can be declared as.
/// The bool is whether it's an array.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LPCType {
    Void,
    Int(bool),
    Float(bool),
    Object(bool),
    String(bool),
    Mapping(bool),
    Mixed(bool),

    // To allow efuns to declare prototypes with multiple allowed types
    Union(LPCTypeUnion)
}

impl LPCType {
    /// Allow myself to match on another type. Handles unions seamlessly.
    /// This is the main type-checking routine.
    pub fn matches_type(&self, other: LPCType) -> bool {
        if let LPCType::Union(self_union) = self {
            self_union.matches_type(other)
        } else if let LPCType::Union(other_union) = other {
            other_union.matches_type(*self)
        } else if let LPCType::Mixed(array) = self {
            match other {
                LPCType::Void => false,
                LPCType::Int(arr) => *array == arr,
                LPCType::String(arr) => *array == arr,
                LPCType::Float(arr) => *array == arr,
                LPCType::Object(arr) => *array == arr,
                LPCType::Mapping(arr) => *array == arr,
                LPCType::Mixed(arr) => *array == arr,
                _ => unimplemented!() // Union is handled in an above if-let.
            }
        } else {
            *self == other
        }
    }
}

impl BitOr for LPCType {
    type Output = LPCType;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut union = if let LPCType::Union(u) = self {
            u
        } else {
            let mut u = LPCTypeUnion::new();
            u.insert(self);
            u
        };

        let final_union = match rhs {
            LPCType::Union(u) => union | u,
            x => {
                union.insert(x);
                union
            }
        };

        LPCType::Union(final_union)
    }
}

impl Display for LPCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_star = |array: &bool| -> &str { if *array { " *" } else { "" } };

        let type_ = match self {
            LPCType::Void => String::from("void"),
            LPCType::Int(array) => format!("int{}", to_star(array)),
            LPCType::Float(array) => format!("float{}", to_star(array)),
            LPCType::String(array) => format!("string{}", to_star(array)),
            LPCType::Object(array) => format!("object{}", to_star(array)),
            LPCType::Mapping(array) => format!("mapping{}", to_star(array)),
            LPCType::Mixed(array) => format!("mixed{}", to_star(array)),
            LPCType::Union(union) => format!("{}", union),
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
            "object" => LPCType::Object(false),
            "mapping" => LPCType::Mapping(false),
            "mixed" => LPCType::Mixed(false),
            _ => panic!("Unknown LPCType. Cannot convert."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitor() {
        let lpc_u = LPCType::Int(false) | LPCType::Int(true) | LPCType::Void;

        if let LPCType::Union(union) = lpc_u {
            assert!(union.int());
            assert!(union.int_array());
            assert!(union.void());
            assert!(!union.string());
            assert!(!union.string_array());
            assert!(!union.float());
            assert!(!union.float_array());
            assert!(!union.object());
            assert!(!union.object_array());
            assert!(!union.mapping());
            assert!(!union.mapping_array());
            assert!(!union.mixed());
            assert!(!union.mixed_array());
        } else {
            panic!("no match")
        }
    }

    #[test]
    fn test_matches_type() {
        assert!(LPCType::Void.matches_type(LPCType::Void));
        assert!(!LPCType::Void.matches_type(LPCType::Int(true)));

        let union = LPCType::Void | LPCType::Int(false);
        assert!(LPCType::Void.matches_type(union));
        
        // Void only matches void.
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Void));
        
        // non-array mixed against non-arrays
        assert!(LPCType::Mixed(false).matches_type(LPCType::Int(false)));
        assert!(LPCType::Mixed(false).matches_type(LPCType::String(false)));
        assert!(LPCType::Mixed(false).matches_type(LPCType::Float(false)));
        assert!(LPCType::Mixed(false).matches_type(LPCType::Object(false)));
        assert!(LPCType::Mixed(false).matches_type(LPCType::Mapping(false)));
        assert!(LPCType::Mixed(false).matches_type(LPCType::Mixed(false)));

        // non-array mixed against arrays
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Int(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::String(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Float(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Object(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Mapping(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Mixed(true)));

        // array mixed against non-arrays
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Int(false)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::String(false)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Float(false)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Object(false)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Mapping(false)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Mixed(false)));

        // array mixed against arrays
        assert!(LPCType::Mixed(true).matches_type(LPCType::Int(true)));
        assert!(LPCType::Mixed(true).matches_type(LPCType::String(true)));
        assert!(LPCType::Mixed(true).matches_type(LPCType::Float(true)));
        assert!(LPCType::Mixed(true).matches_type(LPCType::Object(true)));
        assert!(LPCType::Mixed(true).matches_type(LPCType::Mapping(true)));
        assert!(LPCType::Mixed(true).matches_type(LPCType::Mixed(true)));

        // unions
        assert!(LPCType::Mixed(false).matches_type(LPCType::Int(false) | LPCType::String(true)));
        assert!(!LPCType::Mixed(false).matches_type(LPCType::Int(true) | LPCType::String(true)));

        assert!(LPCType::Mixed(true).matches_type(LPCType::Int(false) | LPCType::String(true)));
        assert!(!LPCType::Mixed(true).matches_type(LPCType::Int(false) | LPCType::String(false)));
    }
}
