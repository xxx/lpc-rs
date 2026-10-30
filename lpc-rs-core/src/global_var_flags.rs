use std::fmt::{Display, Formatter};

use modular_bitfield::prelude::*;

use crate::visibility::Visibility;

/// A struct to keep track of the various boolean flags that can be set
/// on global variables, like `public` and `private`.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Default, Hash)]
pub struct GlobalVarFlags {
    #[bits = 2]
    pub visibility: Visibility,
    /// Exclude this global from save/restore; set by either `static` or `nosave`.
    pub is_static: bool,
    /// Prevent inheriting programs from redeclaring this variable.
    pub nomask: bool,
}

impl Display for GlobalVarFlags {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = self.visibility().to_string();
        if self.is_static() {
            s.push_str(" static");
        }
        if self.nomask() {
            s.push_str(" nomask");
        }
        write!(f, "{s}")
    }
}

impl GlobalVarFlags {
    /// validate a list of strings, returning a list of invalid strings.
    pub fn validate<'a>(strs: &'_ [&'a str]) -> Vec<&'a str> {
        let mut invalid = vec![];
        for s in strs.iter() {
            match *s {
                "public" | "private" | "protected" | "static" | "nosave" | "nomask" => {}
                _ => {
                    invalid.push(*s);
                }
            }
        }

        invalid
    }
}

impl<T> From<Vec<T>> for GlobalVarFlags
where
    T: AsRef<str>,
{
    fn from(vec: Vec<T>) -> Self {
        let mut flags = Self::default();
        for s in vec {
            match s.as_ref() {
                "public" => {
                    flags.set_visibility(Visibility::Public);
                }
                "private" => {
                    flags.set_visibility(Visibility::Private);
                }
                "protected" => {
                    flags.set_visibility(Visibility::Protected);
                }
                "static" | "nosave" => {
                    flags.set_is_static(true);
                }
                "nomask" => {
                    flags.set_nomask(true);
                }
                _ => {}
            }
        }

        flags
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validation_accepts_every_supported_modifier() {
        let modifiers = [
            "public",
            "private",
            "protected",
            "static",
            "nosave",
            "nomask",
        ];

        assert!(GlobalVarFlags::validate(&modifiers).is_empty());
    }

    #[test]
    fn validation_preserves_unknown_modifiers_in_input_order() {
        let modifiers = ["varargs", "private", "unknown", "nomask", "varargs"];

        assert_eq!(
            GlobalVarFlags::validate(&modifiers),
            vec!["varargs", "unknown", "varargs"]
        );
    }

    #[test]
    fn storage_aliases_combine_with_visibility_and_nomask() {
        for (modifier, visibility) in [
            ("public", Visibility::Public),
            ("private", Visibility::Private),
            ("protected", Visibility::Protected),
        ] {
            for storage in ["static", "nosave"] {
                let flags = GlobalVarFlags::from(vec![modifier, storage, "nomask"]);
                let expected = GlobalVarFlags::default()
                    .with_visibility(visibility)
                    .with_is_static(true)
                    .with_nomask(true);

                assert_eq!(flags, expected, "{modifier} {storage}");
            }
        }
    }

    #[test]
    fn the_last_visibility_modifier_preserves_storage_flags() {
        for (modifiers, expected) in [
            (["private", "public"], Visibility::Public),
            (["public", "private"], Visibility::Private),
            (["private", "protected"], Visibility::Protected),
        ] {
            let flags = GlobalVarFlags::from(vec!["static", "nomask", modifiers[0], modifiers[1]]);

            assert_eq!(flags.visibility(), expected);
            assert!(flags.is_static());
            assert!(flags.nomask());
        }
    }

    #[test]
    fn owned_unknown_modifiers_leave_known_flags_intact() {
        let modifiers = ["unknown", "private", "nosave", "nomask"]
            .into_iter()
            .map(str::to_owned)
            .collect::<Vec<_>>();
        let expected = GlobalVarFlags::default()
            .with_visibility(Visibility::Private)
            .with_is_static(true)
            .with_nomask(true);

        assert_eq!(GlobalVarFlags::from(modifiers), expected);
    }

    #[test]
    fn display_keeps_modifiers_in_canonical_order() {
        for (visibility, name) in [
            (Visibility::Public, "public"),
            (Visibility::Private, "private"),
            (Visibility::Protected, "protected"),
        ] {
            for (storage, nomask, suffix) in [
                (false, false, ""),
                (true, false, " static"),
                (false, true, " nomask"),
                (true, true, " static nomask"),
            ] {
                let flags = GlobalVarFlags::default()
                    .with_visibility(visibility)
                    .with_is_static(storage)
                    .with_nomask(nomask);

                assert_eq!(flags.to_string(), format!("{name}{suffix}"));
            }
        }
    }

    #[test]
    fn test_from() {
        let vec = vec!["public", "private"];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Private);

        let vec = vec!["protected", "static"];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Protected);
        assert!(flags.is_static());

        let vec: Vec<&'static str> = vec![];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Public);
    }

    #[test]
    fn nomask_combines_with_visibility_and_storage_flags() {
        let names = vec!["protected", "static", "nomask"];
        assert!(GlobalVarFlags::validate(&names).is_empty());
        let flags = GlobalVarFlags::from(names);
        assert!(flags.nomask());
        assert!(flags.is_static());
        assert_eq!(flags.visibility(), Visibility::Protected);
        assert_eq!(flags.to_string(), "protected static nomask");
        assert!(!GlobalVarFlags::default().nomask());
    }
}
