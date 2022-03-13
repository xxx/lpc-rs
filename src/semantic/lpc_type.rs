use crate::semantic::lpc_type_union::LpcTypeUnion;
use fmt::{Display, Formatter};
use std::{fmt, ops::BitOr};

/// The enumeration of types that a variable can be declared as.
/// The bool is whether it's an array.
#[derive(Debug, Clone, Copy, Eq, PartialOrd, PartialEq)]
pub enum LpcType {
    Void,
    Int(bool),
    Float(bool),
    Object(bool),
    String(bool),
    Mapping(bool),
    Mixed(bool),
    Function(bool),

    // To allow efuns to declare prototypes with multiple allowed types
    Union(LpcTypeUnion),
}

impl LpcType {
    /// See if I match another type. Handles unions seamlessly.
    /// This is the main type-checking routine.
    /// This method is intended to be called on the left hand side type for a
    /// binary expression, passing the right hand side.
    pub fn matches_type(self, other: LpcType) -> bool {
        if let LpcType::Union(self_union) = self {
            self_union.matches_type(other)
        } else if let LpcType::Union(other_union) = other {
            other_union.matches_type(self)
        } else if let LpcType::Mixed(array) = self {
            // "mixed *" only matches arrays (but the elements can be any type)
            // "mixed" is a literal wildcard.
            if array {
                other.is_array()
            } else {
                !matches!(other, LpcType::Void)
            }
        } else if let LpcType::Mixed(array) = other {
            if array {
                self.is_array()
            } else {
                !matches!(self, LpcType::Void)
            }
        } else {
            self == other
        }
    }

    /// Return if we're an array or not
    pub fn is_array(self) -> bool {
        match self {
            LpcType::Void => false,
            LpcType::Int(arr)
            | LpcType::String(arr)
            | LpcType::Float(arr)
            | LpcType::Object(arr)
            | LpcType::Mapping(arr)
            | LpcType::Mixed(arr)
            | LpcType::Function(arr) => arr,
            LpcType::Union(union) => union.is_array(),
        }
    }

    /// Return a copy of me, with the array flag set to `arr`.
    pub fn as_array(self, arr: bool) -> LpcType {
        match self {
            LpcType::Int(_) => LpcType::Int(arr),
            LpcType::String(_) => LpcType::String(arr),
            LpcType::Float(_) => LpcType::Float(arr),
            LpcType::Mapping(_) => LpcType::Mapping(arr),
            LpcType::Mixed(_) => LpcType::Mixed(arr),
            LpcType::Object(_) => LpcType::Object(arr),
            LpcType::Function(_) => LpcType::Function(arr),
            LpcType::Void => self,
            LpcType::Union(_) => self,
        }
    }
}

impl BitOr for LpcType {
    type Output = LpcType;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut union = if let LpcType::Union(u) = self {
            u
        } else {
            let mut u = LpcTypeUnion::new();
            u.insert(self);
            u
        };

        let final_union = match rhs {
            LpcType::Union(u) => union | u,
            x => {
                union.insert(x);
                union
            }
        };

        LpcType::Union(final_union)
    }
}

impl Display for LpcType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_star = |array: &bool| -> &str {
            if *array {
                " *"
            } else {
                ""
            }
        };

        let type_ = match self {
            LpcType::Void => String::from("void"),
            LpcType::Int(array) => format!("int{}", to_star(array)),
            LpcType::Float(array) => format!("float{}", to_star(array)),
            LpcType::String(array) => format!("string{}", to_star(array)),
            LpcType::Object(array) => format!("object{}", to_star(array)),
            LpcType::Mapping(array) => format!("mapping{}", to_star(array)),
            LpcType::Mixed(array) => format!("mixed{}", to_star(array)),
            LpcType::Function(array) => format!("function{}", to_star(array)),
            LpcType::Union(union) => format!("{}", union),
        };

        write!(f, "{}", type_)
    }
}

impl From<&str> for LpcType {
    fn from(str: &str) -> Self {
        match str {
            "void" => LpcType::Void,
            "int" => LpcType::Int(false),
            "float" => LpcType::Float(false),
            "string" => LpcType::String(false),
            "object" => LpcType::Object(false),
            "mapping" => LpcType::Mapping(false),
            "function" => LpcType::Function(false),
            _ => LpcType::Mixed(false),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitor() {
        let lpc_u = LpcType::Int(false) | LpcType::Int(true) | LpcType::Void;

        if let LpcType::Union(union) = lpc_u {
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
        assert!(LpcType::Void.matches_type(LpcType::Void));
        assert!(!LpcType::Void.matches_type(LpcType::Int(true)));

        let union = LpcType::Void | LpcType::Int(false);
        assert!(LpcType::Void.matches_type(union));

        // Void only matches void.
        assert!(!LpcType::Mixed(false).matches_type(LpcType::Void));

        // non-array mixed against non-arrays
        assert!(LpcType::Mixed(false).matches_type(LpcType::Int(false)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::String(false)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Float(false)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Object(false)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Mapping(false)));

        // non-arrays against non-array mixed
        assert!(LpcType::Int(false).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::String(false).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Float(false).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Object(false).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Mapping(false).matches_type(LpcType::Mixed(false)));

        // non-array mixed against arrays
        assert!(LpcType::Mixed(false).matches_type(LpcType::Int(true)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::String(true)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Float(true)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Object(true)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Mapping(true)));

        // arrays against non-array mixed
        assert!(LpcType::Int(true).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::String(true).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Float(true).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Object(true).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Mapping(true).matches_type(LpcType::Mixed(false)));

        // array mixed against non-arrays
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Int(false)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::String(false)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Float(false)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Object(false)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Mapping(false)));

        // non-arrays against array mixed
        assert!(!LpcType::Int(false).matches_type(LpcType::Mixed(true)));
        assert!(!LpcType::String(false).matches_type(LpcType::Mixed(true)));
        assert!(!LpcType::Float(false).matches_type(LpcType::Mixed(true)));
        assert!(!LpcType::Object(false).matches_type(LpcType::Mixed(true)));
        assert!(!LpcType::Mapping(false).matches_type(LpcType::Mixed(true)));

        // array mixed against arrays
        assert!(LpcType::Mixed(true).matches_type(LpcType::Int(true)));
        assert!(LpcType::Mixed(true).matches_type(LpcType::String(true)));
        assert!(LpcType::Mixed(true).matches_type(LpcType::Float(true)));
        assert!(LpcType::Mixed(true).matches_type(LpcType::Object(true)));

        // arrays against array mixed
        assert!(LpcType::Int(true).matches_type(LpcType::Mixed(true)));
        assert!(LpcType::String(true).matches_type(LpcType::Mixed(true)));
        assert!(LpcType::Float(true).matches_type(LpcType::Mixed(true)));
        assert!(LpcType::Object(true).matches_type(LpcType::Mixed(true)));
        assert!(LpcType::Mapping(true).matches_type(LpcType::Mixed(true)));

        // mixed vs. mixed
        assert!(LpcType::Mixed(true).matches_type(LpcType::Mapping(true)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Mixed(false)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Mixed(true)));
        assert!(LpcType::Mixed(false).matches_type(LpcType::Mixed(false)));

        // unions
        assert!(LpcType::Mixed(false).matches_type(LpcType::Int(false) | LpcType::String(true)));
        assert!(!LpcType::Mixed(false).matches_type(LpcType::Int(true) | LpcType::String(true)));

        assert!(LpcType::Mixed(true).matches_type(LpcType::Int(false) | LpcType::String(true)));
        assert!(!LpcType::Mixed(true).matches_type(LpcType::Int(false) | LpcType::String(false)));
    }
}
