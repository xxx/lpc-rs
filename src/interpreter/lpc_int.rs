use std::fmt::{Display, Formatter};
use delegate::delegate;
use serde::{Deserialize, Serialize};
use lpc_rs_core::LpcIntInner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LpcInt(pub LpcIntInner);

impl LpcInt {
    pub const MAX: LpcInt = LpcInt(LpcIntInner::MAX);
    pub const MIN: LpcInt = LpcInt(LpcIntInner::MIN);

    delegate! {
        to self.0 {
            pub fn wrapping_add(self, rhs: LpcIntInner) -> LpcIntInner;
            pub fn wrapping_sub(self, rhs: LpcIntInner) -> LpcIntInner;
        }
    }
}

impl Display for LpcInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<LpcIntInner> for LpcInt {
    fn from(i: LpcIntInner) -> Self {
        Self(i)
    }
}

impl From<LpcInt> for LpcIntInner {
    fn from(i: LpcInt) -> Self {
        i.0
    }
}

impl PartialEq<LpcIntInner> for LpcInt {
    fn eq(&self, other: &LpcIntInner) -> bool {
        self.0 == *other
    }
}