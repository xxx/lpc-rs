use modular_bitfield::prelude::*;

use crate::{mangle::Mangle, visibility::Visibility};

/// A struct to keep track of the various boolean flags that can be set
/// on functions, like `varargs` and `static`, as well as whether the function
/// takes ellipsis args.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Hash, Default)]
pub struct FunctionFlags {
    pub ellipsis: bool,
    pub varargs: bool,
    pub nomask: bool,
    #[bits = 2]
    pub visibility: Visibility,
}

impl FunctionFlags {
    /// Is the function public?
    #[inline]
    pub fn public(&self) -> bool {
        self.visibility() == Visibility::Public
    }

    /// Is the function private?
    #[inline]
    pub fn private(&self) -> bool {
        self.visibility() == Visibility::Private
    }

    /// Is the function protected?
    #[inline]
    pub fn protected(&self) -> bool {
        self.visibility() == Visibility::Protected
    }
}

impl Mangle for FunctionFlags {
    fn mangle(&self) -> String {
        let mut result = String::new();

        match self.visibility() {
            Visibility::Public => {
                result.push_str("pb");
            }
            Visibility::Private => {
                result.push_str("pv");
            }
            Visibility::Protected => {
                result.push_str("pt");
            }
        }

        let mut flag_str = String::new();

        if self.ellipsis() {
            flag_str.push('e');
        }

        if self.varargs() {
            flag_str.push('v');
        }

        if self.nomask() {
            flag_str.push('n');
        }

        if !flag_str.is_empty() {
            result.push('_');
            result.push_str(&flag_str);
        }

        result
    }
}

impl<T> From<&[T]> for FunctionFlags
where
    T: AsRef<str>,
{
    fn from(vec: &[T]) -> Self {
        let mut flags = Self::default();
        for s in vec {
            match s.as_ref() {
                "varargs" => {
                    flags.set_varargs(true);
                }
                "nomask" => {
                    flags.set_nomask(true);
                }
                "public" => {
                    flags.set_visibility(Visibility::Public);
                }
                "private" => {
                    flags.set_visibility(Visibility::Private);
                }
                "protected" => {
                    flags.set_visibility(Visibility::Protected);
                }
                "static" => {
                    flags.set_visibility(Visibility::Protected);
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
    fn visibility_predicates_match_the_declared_visibility() {
        for (visibility, expected) in [
            (Visibility::Public, (true, false, false)),
            (Visibility::Private, (false, true, false)),
            (Visibility::Protected, (false, false, true)),
        ] {
            let flags = FunctionFlags::default()
                .with_visibility(visibility)
                .with_ellipsis(true)
                .with_varargs(true)
                .with_nomask(true);

            assert_eq!(
                (flags.public(), flags.private(), flags.protected()),
                expected,
                "{visibility}"
            );
        }
    }

    #[test]
    fn empty_modifiers_default_to_public_without_optional_flags() {
        let modifiers: &[&str] = &[];
        let flags = FunctionFlags::from(modifiers);

        assert_eq!(
            (
                flags.visibility(),
                flags.ellipsis(),
                flags.varargs(),
                flags.nomask()
            ),
            (Visibility::Public, false, false, false)
        );
    }

    #[test]
    fn optional_modifiers_set_only_their_own_flags() {
        for (modifier, varargs, nomask) in [("varargs", true, false), ("nomask", false, true)] {
            let flags = FunctionFlags::from(&[modifier][..]);

            assert_eq!(
                (
                    flags.visibility(),
                    flags.ellipsis(),
                    flags.varargs(),
                    flags.nomask()
                ),
                (Visibility::Public, false, varargs, nomask),
                "{modifier}"
            );
        }
    }

    #[test]
    fn optional_modifiers_combine_with_visibility() {
        let flags = FunctionFlags::from(&["varargs", "private", "nomask"][..]);
        let expected = FunctionFlags::default()
            .with_visibility(Visibility::Private)
            .with_varargs(true)
            .with_nomask(true);

        assert_eq!(flags, expected);
    }

    #[test]
    fn the_last_visibility_modifier_takes_precedence() {
        for (modifiers, expected) in [
            (["private", "public"], Visibility::Public),
            (["protected", "private"], Visibility::Private),
            (["private", "protected"], Visibility::Protected),
            (["static", "public"], Visibility::Public),
        ] {
            let flags = FunctionFlags::from(&modifiers[..]);

            assert_eq!(flags.visibility(), expected, "{modifiers:?}");
        }
    }

    #[test]
    fn static_functions_have_protected_visibility() {
        let flags = FunctionFlags::from(&["static"][..]);

        assert_eq!(flags.visibility(), Visibility::Protected);
    }

    #[test]
    fn unknown_modifiers_preserve_the_known_flags() {
        let flags = FunctionFlags::from(&["unknown", "varargs", "private", "ignored"][..]);
        let expected = FunctionFlags::default()
            .with_visibility(Visibility::Private)
            .with_varargs(true);

        assert_eq!(flags, expected);
    }

    #[test]
    fn mangling_distinguishes_visibility_without_an_optional_suffix() {
        for (visibility, expected) in [
            (Visibility::Public, "pb"),
            (Visibility::Private, "pv"),
            (Visibility::Protected, "pt"),
        ] {
            let flags = FunctionFlags::default().with_visibility(visibility);

            assert_eq!(flags.mangle(), expected, "{visibility}");
        }
    }

    #[test]
    fn mangling_preserves_optional_flag_combinations_in_order() {
        for (ellipsis, varargs, nomask, expected) in [
            (false, false, false, "pb"),
            (true, false, false, "pb_e"),
            (false, true, false, "pb_v"),
            (false, false, true, "pb_n"),
            (true, true, false, "pb_ev"),
            (true, false, true, "pb_en"),
            (false, true, true, "pb_vn"),
            (true, true, true, "pb_evn"),
        ] {
            let flags = FunctionFlags::default()
                .with_ellipsis(ellipsis)
                .with_varargs(varargs)
                .with_nomask(nomask);

            assert_eq!(flags.mangle(), expected, "{flags:?}");
        }
    }
}
