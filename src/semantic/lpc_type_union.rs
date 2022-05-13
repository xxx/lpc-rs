use crate::semantic::lpc_type::LpcType;
use modular_bitfield::prelude::*;
use std::{
    convert::TryInto,
    fmt,
    fmt::{Display, Formatter},
    ops::BitOr,
};

/// A type that exists solely to allow for union types while remaining `Copy`.
/// I hate it.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Serialize, Deserialize)]
pub struct LpcTypeUnion {
    pub void: bool,
    pub int: bool,
    pub int_array: bool,
    pub string: bool,
    pub string_array: bool,
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
}

impl LpcTypeUnion {
    /// Insert a new type into the union
    pub fn insert(&mut self, type_: LpcType) {
        match type_ {
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
            LpcType::Union(other) => {
                // merge the other into me
                self.set_void(other.void());
                self.set_int(other.int());
                self.set_int_array(other.int_array());
                self.set_string(other.string());
                self.set_string_array(other.string_array());
                self.set_float(other.float());
                self.set_float_array(other.float_array());
                self.set_object(other.object());
                self.set_object_array(other.object_array());
                self.set_mapping(other.mapping());
                self.set_mapping_array(other.mapping_array());
                self.set_mixed(other.mixed());
                self.set_mixed_array(other.mixed_array());
            }
        }
    }

    /// Do we match against another type?
    pub fn matches_type(self, other: LpcType) -> bool {
        if self.mixed() {
            return true;
        }

        if self.mixed_array() {
            return other.is_array();
        }

        match other {
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
                    self.int()
                        || self.string()
                        || self.float()
                        || self.object()
                        || self.mapping()
                        || self.mixed()
                }
            }
            LpcType::Function(array) => {
                if array {
                    self.function_array()
                } else {
                    self.function()
                }
            }
            LpcType::Union(other_union) => self.into_bytes() == other_union.into_bytes(),
        }
    }

    /// Is at least one of our types an array?
    pub fn is_array(self) -> bool {
        self.int_array()
            || self.string_array()
            || self.float_array()
            || self.object_array()
            || self.mapping_array()
            || self.mixed_array()
            || self.function_array()
    }
}

impl Display for LpcTypeUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut vec = vec![];

        // facepalm.

        if self.void() {
            vec.push(LpcType::Void)
        }

        if self.int() {
            vec.push(LpcType::Int(false));
        }

        if self.int_array() {
            vec.push(LpcType::Int(true));
        }

        if self.string() {
            vec.push(LpcType::String(false));
        }

        if self.string_array() {
            vec.push(LpcType::String(true));
        }

        if self.float() {
            vec.push(LpcType::Float(false));
        }

        if self.float_array() {
            vec.push(LpcType::Float(true));
        }

        if self.object() {
            vec.push(LpcType::Object(false));
        }

        if self.object_array() {
            vec.push(LpcType::Object(true));
        }

        if self.mapping() {
            vec.push(LpcType::Mapping(false));
        }

        if self.mapping_array() {
            vec.push(LpcType::Mapping(true));
        }

        if self.mixed() {
            vec.push(LpcType::Mixed(false));
        }

        if self.mixed_array() {
            vec.push(LpcType::Mixed(true));
        }

        if self.function_array() {
            vec.push(LpcType::Function(true));
        }

        let s = vec
            .iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<_>>()
            .join(" | ");

        write!(f, "{}", s)
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
    use crate::semantic::lpc_type::LpcType;

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
    }
}
