use std::ops::BitOr;
use std::convert::TryInto;
use modular_bitfield::prelude::*;
use crate::semantic::lpc_type::LPCType;

/// A type that exists solely to allow for union types while remaining `Copy`.
/// I hate it.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct LPCTypeUnion {
    void: bool,
    int: bool,
    int_array: bool,
    string: bool,
    string_array: bool,
    float: bool,
    float_array: bool,
    object: bool,
    object_array: bool,
    mapping: bool,
    mapping_array: bool,
    mixed: bool,
    mixed_array: bool
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
}

impl BitOr for LPCTypeUnion {
    type Output = LPCTypeUnion;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut my_bytes = self.into_bytes();
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
