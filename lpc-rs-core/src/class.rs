//! Nominal class identities and immutable field definitions.

use std::{fmt, sync::Arc};

use ustr::Ustr;

use crate::{lpc_type::LpcType, mangle::Mangle};

/// A class's canonical defining source and name, independent of its field layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassId {
    pub source: Ustr,
    pub name: Ustr,
}

impl Mangle for ClassId {
    fn mangle(&self) -> String {
        format!(
            "c{}:{}{}:{}",
            self.source.len(),
            self.source,
            self.name.len(),
            self.name
        )
    }
}

impl fmt::Display for ClassId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "class {} ({})", self.name, self.source)
    }
}

/// A field's declared type refers to nominal identities, allowing recursive classes.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassField {
    pub name: Ustr,
    pub type_: LpcType,
}

/// One compiled view of a class, in declaration order.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassDefinition {
    pub id: ClassId,
    pub fields: Box<[ClassField]>,
}

/// Definitions visible to one defining program and retained by its compiled code.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassScope {
    pub definitions: Box<[Arc<ClassDefinition>]>,
}

impl ClassScope {
    pub fn get(&self, id: ClassId) -> Option<&ClassDefinition> {
        self.definitions
            .iter()
            .find(|definition| definition.id == id)
            .map(AsRef::as_ref)
    }
}
