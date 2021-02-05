use std::ops::BitOr;
use std::convert::TryInto;
use modular_bitfield::prelude::*;
use crate::semantic::lpc_type::LPCType;
use std::fmt::Display;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::fmt;

/// A type that exists solely to allow for union types while remaining `Copy`.
/// I hate it.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct LPCTypeUnion {
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
    pub mixed_array: bool
}

impl LPCTypeUnion {
    pub fn insert(&mut self, type_: LPCType) {
        match type_ {
            LPCType::Void => {
                self.set_void(true)
            }
            LPCType::Int(array) => {
                if array { self.set_int_array(true) } else { self.set_int(true) }
            }
            LPCType::String(array) => {
                if array { self.set_string_array(true) } else { self.set_string(true) }
            }
            LPCType::Float(array) => {
                if array { self.set_float_array(true) } else { self.set_float(true) }
            }
            LPCType::Object(array) => {
                if array { self.set_object_array(true) } else { self.set_object(true) }
            }
            LPCType::Mapping(array) => {
                if array { self.set_mapping_array(true) } else { self.set_mapping(true) }
            }
            LPCType::Mixed(array) => {
                if array { self.set_mixed_array(true) } else { self.set_mixed(true) }
            }
            LPCType::Union(_) => panic!(
                "Cannot insert LPCTypeUnion into another LPCTypeUnion. It makes no sense to do so."
            )
        }
    }

    pub fn matches_type(&self, other: LPCType) -> bool {
        match other {
            LPCType::Void => {
                self.void()
            }
            LPCType::Int(array) => {
                if array { self.int_array() } else { self.int() }
            }
            LPCType::String(array) => {
                if array { self.string_array() } else { self.string() }
            }
            LPCType::Float(array) => {
                if array { self.float_array() } else { self.float() }
            }
            LPCType::Object(array) => {
                if array { self.object_array() } else { self.object() }
            }
            LPCType::Mapping(array) => {
                if array { self.mapping_array() } else { self.mapping() }
            }
            LPCType::Mixed(array) => {
                if array {
                    self.int_array() ||
                    self.string_array() ||
                    self.float_array() ||
                    self.object_array() ||
                    self.mapping_array() ||
                    self.mixed_array()
                } else {
                    self.int() ||
                    self.string() ||
                    self.float() ||
                    self.object() ||
                    self.mapping() ||
                    self.mixed()
                }
            }
            LPCType::Union(other_union) => {
                self.into_bytes() == other_union.into_bytes()
            }
        }
    }
}

impl Display for LPCTypeUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut vec = vec![];
        
        // facepalm.
        
        if self.void() {
            vec.push(LPCType::Void)
        }
        
        if self.int() {
            vec.push(LPCType::Int(false));
        }
        
        if self.int_array() {
            vec.push(LPCType::Int(true));
        }
        
        if self.string() {
            vec.push(LPCType::String(false));
        }
        
        if self.string_array() {
            vec.push(LPCType::String(true));
        }
        
        if self.float() {
            vec.push(LPCType::Float(false));
        }
        
        if self.float_array() {
            vec.push(LPCType::Float(true));
        }
        
        if self.object() {
            vec.push(LPCType::Object(false));
        }
        
        if self.object_array() {
            vec.push(LPCType::Object(true));
        }
        
        if self.mapping() {
            vec.push(LPCType::Mapping(false));
        }
        
        if self.mapping_array() {
            vec.push(LPCType::Mapping(true));
        }
        
        if self.mixed() {
            vec.push(LPCType::Mixed(false));
        }
        
        if self.mixed_array() {
            vec.push(LPCType::Mixed(true));
        }

        let s = vec.iter().map(|i| format!("{}", i)).collect::<Vec<_>>().join(" | ");
        
        write!(f, "{}", s)
    }
}

impl BitOr for LPCTypeUnion {
    type Output = LPCTypeUnion;

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
    use crate::semantic::lpc_type::LPCType;

    #[test]
    fn test_bitor() {
        let lpc_u = LPCType::Int(false) | LPCType::Int(true) | LPCType::Void;
        let lpc_u2 = LPCType::String(true) | LPCType::Object(false);

        if let (LPCType::Union(u1), LPCType::Union(u2)) = (lpc_u, lpc_u2) {
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
        }
    }

    #[test]
    fn test_matches_type() {
        let mut union = LPCTypeUnion::new();
        union.set_string_array(true);
        union.set_void(true);

        assert!(union.matches_type(LPCType::String(true)));
        assert!(!union.matches_type(LPCType::String(false)));
        assert!(union.matches_type(LPCType::Void));
        assert!(!union.matches_type(LPCType::Int(false)));
        assert!(!union.matches_type(LPCType::Mapping(true)));
    }
}
