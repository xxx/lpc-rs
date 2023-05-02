use std::{
    cmp::Ordering,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::Add,
    path::Path,
    sync::Arc,
};

use lpc_rs_errors::{lpc_error, Result};
use lpc_rs_utils::string::MAX_STRING_LENGTH;
use string_interner::{DefaultSymbol, StringInterner, Symbol};

use crate::interpreter::{heap::Heap, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef};

/// An enum to differentiate between statically and dynamically created strings.
#[derive(Debug, Clone)]
pub enum LpcString {
    /// A static string, indexing into its twinned string set.
    Static(usize, Arc<StringInterner>),

    /// A dynamically created string.
    Dynamic(String),
}

impl LpcString {
    /// Get the string as a `&str`.
    #[inline]
    pub fn to_str(&self) -> &str {
        match self {
            LpcString::Static(s, strings) => strings
                .resolve(DefaultSymbol::try_from_usize(*s).unwrap())
                .unwrap_or(""),
            LpcString::Dynamic(ref s) => s.as_str(),
        }
    }

    /// Get the length of the string
    #[inline]
    pub fn len(&self) -> usize {
        self.to_str().len()
    }

    /// Get whether the string is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get an iterator over the characters in the string
    #[inline]
    pub fn chars(&self) -> std::str::Chars {
        self.to_str().chars()
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

impl Eq for LpcString {}

impl PartialOrd for LpcString {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.to_str().partial_cmp(other.to_str())
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

impl<T> IntoLpcRef for T
where
    T: Into<LpcString>,
{
    fn into_lpc_ref(self, memory: &Heap) -> LpcRef {
        memory.alloc_string(self.into())
    }
}

impl Add<LpcString> for LpcString {
    type Output = Result<Self>;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        let new_capacity = self.len() + rhs.len();
        if new_capacity <= MAX_STRING_LENGTH {
            let mut s = String::with_capacity(self.len() + rhs.len());
            s.push_str(self.to_str());
            s.push_str(rhs.to_str());

            Ok(Self::from(s))
        } else {
            Err(lpc_error!("overflow in string concatenation"))
        }
    }
}
