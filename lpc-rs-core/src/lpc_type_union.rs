use std::{
    convert::TryInto,
    fmt,
    fmt::{Display, Formatter},
    ops::BitOr,
};

use modular_bitfield::prelude::*;

use crate::{class::ClassId, lpc_type::LpcType, mangle::Mangle};

/// A type that exists solely to allow for union types while remaining `Copy`.
/// I hate it.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Hash)]
pub struct LpcTypeUnion {
    pub void: bool,
    pub int: bool,
    pub int_array: bool,
    pub string: bool,
    pub string_array: bool,
    pub bytes: bool,
    pub bytes_array: bool,
    pub float: bool,
    pub float_array: bool,
    pub object: bool,
    pub object_array: bool,
    pub mapping: bool,
    pub mapping_array: bool,
    pub mixed: bool,
    pub mixed_array: bool,
    pub function: bool,
    pub function_array: bool,
    pub nil: bool,
}

impl LpcTypeUnion {
    /// Insert a new type into the union
    pub fn insert(&mut self, type_: LpcType) -> Result<(), ClassId> {
        match type_ {
            LpcType::Class(id, _) => return Err(id),
            LpcType::Nil => self.set_nil(true),
            LpcType::Void => self.set_void(true),
            LpcType::Int(array) => {
                if array {
                    self.set_int_array(true)
                } else {
                    self.set_int(true)
                }
            }
            LpcType::String(array) => {
                if array {
                    self.set_string_array(true)
                } else {
                    self.set_string(true)
                }
            }
            LpcType::Bytes(array) => {
                if array {
                    self.set_bytes_array(true)
                } else {
                    self.set_bytes(true)
                }
            }
            LpcType::Float(array) => {
                if array {
                    self.set_float_array(true)
                } else {
                    self.set_float(true)
                }
            }
            LpcType::Object(array) => {
                if array {
                    self.set_object_array(true)
                } else {
                    self.set_object(true)
                }
            }
            LpcType::Mapping(array) => {
                if array {
                    self.set_mapping_array(true)
                } else {
                    self.set_mapping(true)
                }
            }
            LpcType::Mixed(array) => {
                if array {
                    self.set_mixed_array(true)
                } else {
                    self.set_mixed(true)
                }
            }
            LpcType::Function(array) => {
                if array {
                    self.set_function_array(true)
                } else {
                    self.set_function(true)
                }
            }
            LpcType::Union(other) => *self = *self | other,
        }
        Ok(())
    }

    /// Do we match against another type?
    pub fn matches_type(self, other: LpcType) -> bool {
        if self.mixed() && other != LpcType::Void {
            return true;
        }

        if self.mixed_array() && other.is_array() {
            return true;
        }

        match other {
            LpcType::Nil => self.nil(),
            LpcType::Void => self.void(),
            LpcType::Int(array) => {
                if array {
                    self.int_array()
                } else {
                    self.int()
                }
            }
            LpcType::String(array) => {
                if array {
                    self.string_array()
                } else {
                    self.string()
                }
            }
            LpcType::Bytes(array) => {
                if array {
                    self.bytes_array()
                } else {
                    self.bytes()
                }
            }
            LpcType::Float(array) => {
                if array {
                    self.float_array()
                } else {
                    self.float()
                }
            }
            LpcType::Object(array) => {
                if array {
                    self.object_array()
                } else {
                    self.object()
                }
            }
            LpcType::Mapping(array) => {
                if array {
                    self.mapping_array()
                } else {
                    self.mapping()
                }
            }
            LpcType::Mixed(array) => {
                if array {
                    self.is_array()
                } else {
                    self.types()
                        .into_iter()
                        .any(|member| member != LpcType::Void)
                }
            }
            LpcType::Function(array) => {
                if array {
                    self.function_array()
                } else {
                    self.function()
                }
            }
            LpcType::Class(_, _) => false,
            LpcType::Union(other_union) => self.into_bytes() == other_union.into_bytes(),
        }
    }

    /// The member types, in declaration order.
    pub fn types(self) -> Vec<LpcType> {
        let members = [
            (self.nil(), LpcType::Nil),
            (self.void(), LpcType::Void),
            (self.int(), LpcType::Int(false)),
            (self.int_array(), LpcType::Int(true)),
            (self.string(), LpcType::String(false)),
            (self.string_array(), LpcType::String(true)),
            (self.bytes(), LpcType::Bytes(false)),
            (self.bytes_array(), LpcType::Bytes(true)),
            (self.float(), LpcType::Float(false)),
            (self.float_array(), LpcType::Float(true)),
            (self.object(), LpcType::Object(false)),
            (self.object_array(), LpcType::Object(true)),
            (self.mapping(), LpcType::Mapping(false)),
            (self.mapping_array(), LpcType::Mapping(true)),
            (self.mixed(), LpcType::Mixed(false)),
            (self.mixed_array(), LpcType::Mixed(true)),
            (self.function(), LpcType::Function(false)),
            (self.function_array(), LpcType::Function(true)),
        ];

        members
            .into_iter()
            .filter_map(|(present, type_)| present.then_some(type_))
            .collect()
    }

    /// Is at least one of our types an array?
    pub fn is_array(self) -> bool {
        self.int_array()
            || self.string_array()
            || self.bytes_array()
            || self.float_array()
            || self.object_array()
            || self.mapping_array()
            || self.mixed_array()
            || self.function_array()
    }
}

impl Display for LpcTypeUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .types()
            .iter()
            .map(|i| format!("{i}"))
            .collect::<Vec<_>>()
            .join(" | ");

        write!(f, "{s}")
    }
}

impl Mangle for LpcTypeUnion {
    fn mangle(&self) -> String {
        let mut vec = vec![];

        if self.nil() {
            vec.push(LpcType::Nil.mangle());
        }
        if self.void() {
            vec.push(LpcType::Void.mangle())
        }

        if self.int() {
            vec.push(LpcType::Int(false).mangle());
        }

        if self.int_array() {
            vec.push(LpcType::Int(true).mangle());
        }

        if self.string() {
            vec.push(LpcType::String(false).mangle());
        }

        if self.string_array() {
            vec.push(LpcType::String(true).mangle());
        }

        if self.bytes() {
            vec.push(LpcType::Bytes(false).mangle());
        }

        if self.bytes_array() {
            vec.push(LpcType::Bytes(true).mangle());
        }

        if self.float() {
            vec.push(LpcType::Float(false).mangle());
        }

        if self.float_array() {
            vec.push(LpcType::Float(true).mangle());
        }

        if self.object() {
            vec.push(LpcType::Object(false).mangle());
        }

        if self.object_array() {
            vec.push(LpcType::Object(true).mangle());
        }

        if self.mapping() {
            vec.push(LpcType::Mapping(false).mangle());
        }

        if self.mapping_array() {
            vec.push(LpcType::Mapping(true).mangle());
        }

        if self.mixed() {
            vec.push(LpcType::Mixed(false).mangle());
        }

        if self.mixed_array() {
            vec.push(LpcType::Mixed(true).mangle());
        }

        if self.function() {
            vec.push(LpcType::Function(false).mangle());
        }

        if self.function_array() {
            vec.push(LpcType::Function(true).mangle());
        }

        vec.iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join("|")
    }
}

impl BitOr for LpcTypeUnion {
    type Output = LpcTypeUnion;

    fn bitor(self, rhs: Self) -> Self::Output {
        let my_bytes = self.into_bytes();
        let rhs_bytes = rhs.into_bytes();

        let combined = my_bytes
            .iter()
            .enumerate()
            .map(|(index, byte)| byte | rhs_bytes[index])
            .collect::<Vec<_>>()
            .try_into()
            .expect("Unexpected length trying to recombine bytes?");

        Self::from_bytes(combined).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MEMBERS: [LpcType; 18] = [
        LpcType::Nil,
        LpcType::Void,
        LpcType::Int(false),
        LpcType::Int(true),
        LpcType::String(false),
        LpcType::String(true),
        LpcType::Bytes(false),
        LpcType::Bytes(true),
        LpcType::Float(false),
        LpcType::Float(true),
        LpcType::Object(false),
        LpcType::Object(true),
        LpcType::Mapping(false),
        LpcType::Mapping(true),
        LpcType::Mixed(false),
        LpcType::Mixed(true),
        LpcType::Function(false),
        LpcType::Function(true),
    ];

    #[test]
    fn inserting_each_primitive_preserves_its_identity_and_array_kind() {
        for member in MEMBERS {
            let mut union = LpcTypeUnion::new();
            union.insert(member).unwrap();

            assert_eq!(union.types(), vec![member]);
            assert_eq!(union.is_array(), member.is_array(), "{member}");
        }
    }

    #[test]
    fn empty_unions_have_no_members_or_array_kind() {
        let union = LpcTypeUnion::new();

        assert!(union.types().is_empty());
        assert!(!union.is_array());
        assert_eq!(union.mangle(), "");
        assert_eq!(union.to_string(), "");
    }

    #[test]
    fn duplicate_members_survive_union_insertion_and_bitwise_combination() {
        let mut union = LpcTypeUnion::new();
        union.insert(LpcType::String(false)).unwrap();
        let original = union;

        union.insert(LpcType::Union(original)).unwrap();

        assert_eq!(union.types(), vec![LpcType::String(false)]);
        assert_eq!((original | original).types(), vec![LpcType::String(false)]);
    }

    #[test]
    fn union_equality_distinguishes_different_member_sets() {
        let mut union = LpcTypeUnion::new();
        union.insert(LpcType::Int(false)).unwrap();
        let mut other = LpcTypeUnion::new();
        other.insert(LpcType::String(false)).unwrap();

        assert!(union.matches_type(LpcType::Union(union)));
        assert!(!union.matches_type(LpcType::Union(other)));
    }

    #[test]
    fn a_single_member_union_preserves_primitive_compatibility() {
        for expected in MEMBERS {
            let mut union = LpcTypeUnion::new();
            union.insert(expected).unwrap();

            for found in MEMBERS {
                assert_eq!(
                    LpcType::Union(union).matches_type(found),
                    expected.matches_type(found),
                    "union of {expected} against {found}"
                );
            }
        }
    }

    #[test]
    fn two_member_unions_accept_values_allowed_by_either_member() {
        for left in MEMBERS {
            for right in MEMBERS {
                let union = left | right;
                for found in MEMBERS {
                    assert_eq!(
                        union.matches_type(found),
                        left.matches_type(found) || right.matches_type(found),
                        "{left} | {right} against {found}"
                    );
                }
            }
        }
    }

    #[test]
    fn mangling_retains_scalar_function_members() {
        let union = LpcType::String(false) | LpcType::Function(false);

        assert_eq!(union.mangle(), "s|F");
        assert_ne!(
            union.mangle(),
            (LpcType::String(false) | LpcType::String(false)).mangle()
        );
    }

    #[test]
    fn all_members_keep_their_canonical_order_independent_of_insertion_order() {
        let mut union = LpcTypeUnion::new();
        for member in MEMBERS.into_iter().rev() {
            union.insert(member).unwrap();
        }

        assert_eq!(union.types(), MEMBERS);
        assert_eq!(
            union.to_string(),
            "nil | void | int | int * | string | string * | bytes | bytes * | float | float * | object | object * | mapping | mapping * | mixed | mixed * | function | function *"
        );
        assert_eq!(
            union.mangle(),
            "n|v|i|i*|s|s*|b|b*|f|f*|o|o*|m|m*|x|x*|F|F*"
        );
    }

    #[test]
    fn a_function_member_displays() {
        let union = LpcType::Function(false) | LpcType::String(false);

        assert_eq!(union.to_string(), "string | function");
    }

    #[test]
    fn types_lists_every_member_in_display_order() {
        let LpcType::Union(union) = LpcType::String(false) | LpcType::Int(false) else {
            panic!("not a union");
        };

        assert_eq!(
            union.types(),
            vec![LpcType::Int(false), LpcType::String(false)]
        );
    }

    #[test]
    fn inserting_a_union_keeps_both_sides() {
        let LpcType::Union(other) = LpcType::Function(false) | LpcType::Function(true) else {
            panic!("not a union");
        };
        let mut union = LpcTypeUnion::new();
        union.set_string(true);

        union.insert(LpcType::Union(other)).unwrap();

        assert!(union.string());
        assert!(union.function());
        assert!(union.function_array());
    }

    #[test]
    fn test_bitor() {
        let lpc_u = LpcType::Int(false) | LpcType::Int(true) | LpcType::Void;
        let lpc_u2 = LpcType::String(true) | LpcType::Object(false);

        if let (LpcType::Union(u1), LpcType::Union(u2)) = (lpc_u, lpc_u2) {
            let union = u1 | u2;

            assert!(union.int());
            assert!(union.int_array());
            assert!(union.void());
            assert!(!union.string());
            assert!(union.string_array());
            assert!(!union.float());
            assert!(!union.float_array());
            assert!(union.object());
            assert!(!union.object_array());
            assert!(!union.mapping_array());
            assert!(!union.mapping());
            assert!(!union.mapping_array());
            assert!(!union.mixed());
            assert!(!union.mixed_array());
            assert!(!union.function());
            assert!(!union.function_array());
        }
    }

    #[test]
    fn test_matches_type() {
        let mut union = LpcTypeUnion::new();
        union.set_string_array(true);
        union.set_void(true);

        assert!(union.matches_type(LpcType::String(true)));
        assert!(!union.matches_type(LpcType::String(false)));
        assert!(union.matches_type(LpcType::Void));
        assert!(!union.matches_type(LpcType::Int(false)));
        assert!(!union.matches_type(LpcType::Mapping(true)));

        union.set_mixed(true);
        assert!(union.matches_type(LpcType::Int(false)));
        assert!(union.matches_type(LpcType::Int(true)));
        assert!(union.matches_type(LpcType::Float(false)));
        assert!(union.matches_type(LpcType::Float(true)));
        assert!(union.matches_type(LpcType::String(false)));
        assert!(union.matches_type(LpcType::String(true)));
        assert!(union.matches_type(LpcType::Mapping(false)));
        assert!(union.matches_type(LpcType::Mapping(true)));
        assert!(union.matches_type(LpcType::Object(false)));
        assert!(union.matches_type(LpcType::Mapping(true)));
        assert!(union.matches_type(LpcType::Mixed(false)));
        assert!(union.matches_type(LpcType::Mixed(true)));
        assert!(union.matches_type(LpcType::Function(false)));
        assert!(union.matches_type(LpcType::Function(true)));

        let mut union = LpcTypeUnion::new();
        union.set_mixed_array(true);
        union.set_mapping(true);
        assert!(union.matches_type(LpcType::Mapping(false)));
    }
}
