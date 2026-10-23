use std::{
    cmp::Ordering,
    ffi::OsStr,
    fmt::Display,
    hash::{Hash, Hasher},
    path::Path,
};

use crate::string::MAX_STRING_LENGTH;
use lpc_rs_errors::{Result, lpc_error};
use ustr::Ustr;

/// An enum to differentiate between statically and dynamically created strings.
#[derive(Debug, Clone)]
pub enum LpcString {
    /// A string literal from compiled code.
    Static(Ustr),

    /// A dynamically created string.
    Dynamic(String),
}

impl LpcString {
    /// Get the string as a `&str`.
    #[inline]
    pub fn to_str(&self) -> &str {
        match self {
            LpcString::Static(s) => s.as_str(),
            LpcString::Dynamic(s) => s.as_str(),
        }
    }

    /// Get the length of the string
    #[inline]
    pub fn len(&self) -> usize {
        self.to_str().len()
    }

    /// The length LPC sees: characters, not bytes.
    #[inline]
    pub fn char_count(&self) -> usize {
        let s = self.to_str();
        if s.is_ascii() {
            s.len()
        } else {
            s.chars().count()
        }
    }

    /// Get whether the string is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get an iterator over the characters in the string
    #[inline]
    pub fn chars(&self) -> std::str::Chars<'_> {
        self.to_str().chars()
    }

    /// `a` followed by `b` in one allocation, or an error past
    /// [`MAX_STRING_LENGTH`].
    pub fn concat(a: &str, b: &str) -> Result<Self> {
        let len = a.len() + b.len();
        if len > MAX_STRING_LENGTH {
            return Err(lpc_error!("overflow in string concatenation"));
        }
        let mut s = String::with_capacity(len);
        s.push_str(a);
        s.push_str(b);
        Ok(Self::Dynamic(s))
    }
}

impl From<String> for LpcString {
    #[inline]
    fn from(s: String) -> Self {
        Self::Dynamic(s)
    }
}

impl From<&String> for LpcString {
    #[inline]
    fn from(s: &String) -> Self {
        Self::Dynamic(s.to_string())
    }
}

impl From<&str> for LpcString {
    #[inline]
    fn from(s: &str) -> Self {
        Self::Dynamic(s.to_string())
    }
}

impl PartialEq for LpcString {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.to_str() == other.to_str()
    }
}

impl PartialEq<String> for LpcString {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.to_str() == other.as_str()
    }
}

impl PartialEq<&String> for LpcString {
    #[inline]
    fn eq(&self, other: &&String) -> bool {
        self.to_str() == other.as_str()
    }
}

impl PartialEq<&str> for LpcString {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.to_str() == *other
    }
}

impl PartialEq<&str> for &LpcString {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.to_str() == *other
    }
}

impl Eq for LpcString {}

impl PartialOrd for LpcString {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LpcString {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_str().cmp(other.to_str())
    }
}

impl Hash for LpcString {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_str().hash(state)
    }
}

impl Display for LpcString {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl AsRef<str> for &LpcString {
    #[inline]
    fn as_ref(&self) -> &str {
        self.to_str()
    }
}

impl AsRef<Path> for &LpcString {
    #[inline]
    fn as_ref(&self) -> &Path {
        self.to_str().as_ref()
    }
}

impl AsRef<OsStr> for LpcString {
    #[inline]
    fn as_ref(&self) -> &OsStr {
        self.to_str().as_ref()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;

    use ustr::ustr;

    use super::*;

    fn values(text: &str) -> [LpcString; 2] {
        [
            LpcString::Static(ustr(text)),
            LpcString::Dynamic(text.to_owned()),
        ]
    }

    fn hash_value<T: Hash + ?Sized>(value: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn string_views_preserve_bytes_scalars_and_borrowed_identity() {
        for (text, bytes, characters) in [
            ("", 0, 0),
            ("hello", 5, 5),
            ("é", 2, 1),
            ("a\u{301}", 3, 2),
            ("😀", 4, 1),
            ("a\0b", 3, 3),
        ] {
            for value in values(text) {
                assert_eq!(value.to_str(), text);
                assert_eq!(value.len(), bytes);
                assert_eq!(value.char_count(), characters);
                assert_eq!(value.is_empty(), text.is_empty());
                assert_eq!(value.chars().collect::<String>(), text);
                assert_eq!(value.to_string(), text);
                let borrowed = &value;
                let string = <&LpcString as AsRef<str>>::as_ref(&borrowed);
                let path = <&LpcString as AsRef<Path>>::as_ref(&borrowed);
                let native: &OsStr = value.as_ref();
                assert_eq!(string, text);
                assert_eq!(path, Path::new(text));
                assert_eq!(native, OsStr::new(text));
            }
        }
    }

    #[test]
    fn equality_overloads_compare_contents_for_both_string_representations() {
        let same = "é".to_owned();
        let different = "other".to_owned();
        for value in values("é") {
            assert!(<LpcString as PartialEq<String>>::eq(&value, &same));
            assert!(!<LpcString as PartialEq<String>>::eq(&value, &different));
            assert!(<LpcString as PartialEq<&String>>::eq(&value, &&same));
            assert!(!<LpcString as PartialEq<&String>>::eq(&value, &&different));
            assert!(<LpcString as PartialEq<&str>>::eq(&value, &same.as_str()));
            assert!(!<LpcString as PartialEq<&str>>::eq(
                &value,
                &different.as_str()
            ));
            let borrowed = &value;
            assert!(<&LpcString as PartialEq<&str>>::eq(
                &borrowed,
                &same.as_str()
            ));
            assert!(!<&LpcString as PartialEq<&str>>::eq(
                &borrowed,
                &different.as_str()
            ));
            for other in values("é") {
                assert_eq!(value, other);
            }
            for other in values("other") {
                assert_ne!(value, other);
            }
        }
    }

    #[test]
    fn ordering_and_hashing_match_native_string_contents() {
        for left in ["", "a", "b", "é", "a\u{301}"] {
            for right in ["", "a", "b", "é", "a\u{301}"] {
                for left_value in values(left) {
                    for right_value in values(right) {
                        assert_eq!(left_value.cmp(&right_value), left.cmp(right));
                        assert_eq!(left_value.partial_cmp(&right_value), Some(left.cmp(right)));
                    }
                    assert_eq!(hash_value(&left_value), hash_value(left));
                }
            }
        }
    }

    #[test]
    fn owned_and_borrowed_conversions_preserve_the_payload() {
        let text = "é\0value".to_owned();
        let owned = LpcString::from(text.clone());
        let borrowed = LpcString::from(&text);
        let slice = LpcString::from(text.as_str());

        for value in [owned, borrowed, slice] {
            assert_eq!(value.to_str(), text);
            assert!(matches!(value, LpcString::Dynamic(_)));
        }
    }

    #[test]
    fn concatenation_accepts_the_exact_byte_cap_and_rejects_one_more_scalar() {
        let prefix = "é".repeat(MAX_STRING_LENGTH / 2 - 1);
        let full = LpcString::concat(&prefix, "é").unwrap();

        assert_eq!(full.len(), MAX_STRING_LENGTH);
        assert_eq!(full.char_count(), MAX_STRING_LENGTH / 2);
        assert!(LpcString::concat(full.to_str(), "é").is_err());
        assert_eq!(LpcString::concat("", full.to_str()).unwrap(), full);
    }

    #[test]
    fn a_static_string_is_its_literal() {
        let s = LpcString::Static(ustr("hello"));
        assert_eq!(s.to_str(), "hello");
        assert_eq!(s, LpcString::from("hello"));
        assert_eq!(s.len(), 5);
    }

    #[test]
    fn concat_joins_in_order() {
        let joined = LpcString::concat("hello, ", "world").unwrap();
        assert_eq!(joined, "hello, world");
    }

    #[test]
    fn concat_past_the_maximum_length_is_an_error() {
        let full = "a".repeat(MAX_STRING_LENGTH);
        let result = LpcString::concat(&full, "b");
        assert_eq!(
            result.unwrap_err().to_string(),
            "overflow in string concatenation"
        );
    }
}
