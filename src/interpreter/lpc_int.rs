use std::{
    fmt::{Display, Formatter},
    ops::Add,
};

use delegate::delegate;
use lpc_rs_core::{BaseFloat, LpcFloatInner, LpcIntInner};
use serde::{Deserialize, Serialize};

use crate::interpreter::lpc_float::LpcFloat;

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

impl From<LpcInt> for bool {
    fn from(i: LpcInt) -> Self {
        i.0 != 0
    }
}

impl From<bool> for LpcInt {
    fn from(b: bool) -> Self {
        Self::from(if b { 1 } else { 0 })
    }
}

impl Add<LpcInt> for LpcInt {
    type Output = LpcInt;

    fn add(self, rhs: LpcInt) -> Self::Output {
        Self::Output::from(self.0.wrapping_add(rhs.0))
    }
}

impl Add<LpcFloat> for LpcInt {
    type Output = LpcFloat;

    fn add(self, rhs: LpcFloat) -> Self::Output {
        Self::Output::from(LpcFloatInner::from(self.0 as BaseFloat) + rhs.0)
    }
}
