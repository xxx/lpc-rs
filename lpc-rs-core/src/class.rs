//! Nominal class identities and immutable field definitions.

use std::{collections::HashMap, fmt, sync::Arc};

use ustr::Ustr;

use crate::{lpc_type::LpcType, mangle::Mangle, visibility::Visibility};

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
    /// Restrictions in this source scope; absent entries are public.
    pub visibility: HashMap<ClassId, Visibility>,
}

impl ClassScope {
    /// The visibility of a definition in this source scope.
    pub fn visibility(&self, id: ClassId) -> Visibility {
        self.visibility.get(&id).copied().unwrap_or_default()
    }

    pub fn get(&self, id: ClassId) -> Option<&ClassDefinition> {
        self.definitions
            .iter()
            .find(|definition| definition.id == id)
            .map(AsRef::as_ref)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn definition(source: &str, name: &str) -> Arc<ClassDefinition> {
        Arc::new(ClassDefinition {
            id: ClassId {
                source: source.into(),
                name: name.into(),
            },
            fields: vec![ClassField {
                name: "value".into(),
                type_: LpcType::Int(false),
            }]
            .into_boxed_slice(),
        })
    }

    #[test]
    fn lookup_uses_both_defining_source_and_class_name() {
        let first = definition("/first.h", "Record");
        let another_file = definition("/second.h", "Record");
        let another_name = definition("/first.h", "Other");
        let scope = ClassScope {
            definitions: vec![first.clone(), another_file.clone(), another_name.clone()]
                .into_boxed_slice(),
            visibility: HashMap::new(),
        };

        for expected in [first, another_file, another_name] {
            assert_eq!(scope.get(expected.id), Some(expected.as_ref()));
        }
        assert_eq!(
            scope.get(ClassId {
                source: "/missing.h".into(),
                name: "Record".into()
            }),
            None
        );
    }

    #[test]
    fn visibility_uses_explicit_restrictions_and_defaults_to_public() {
        let private = definition("/records.h", "PrivateRecord");
        let protected = definition("/records.h", "ProtectedRecord");
        let public = definition("/records.h", "PublicRecord");
        let scope = ClassScope {
            definitions: vec![private.clone(), protected.clone(), public.clone()]
                .into_boxed_slice(),
            visibility: HashMap::from([
                (private.id, Visibility::Private),
                (protected.id, Visibility::Protected),
            ]),
        };

        assert_eq!(scope.visibility(private.id), Visibility::Private);
        assert_eq!(scope.visibility(protected.id), Visibility::Protected);
        assert_eq!(scope.visibility(public.id), Visibility::Public);
    }

    #[test]
    fn mangling_prefixes_utf8_byte_lengths_to_prevent_identity_collisions() {
        for (source, name, expected) in [
            ("/a.h", "record", "c4:/a.h6:record"),
            ("/é.h", "Point", "c5:/é.h5:Point"),
        ] {
            let id = ClassId {
                source: source.into(),
                name: name.into(),
            };

            assert_eq!(id.mangle(), expected);
        }
    }
}
