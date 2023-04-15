use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use lpc_rs_core::{BaseFloat, LpcFloatInner};
use crate::interpreter::lpc_int::LpcInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LpcFloat(pub LpcFloatInner);

impl Display for LpcFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<BaseFloat> for LpcFloat {
    fn from(i: BaseFloat) -> Self {
        Self(LpcFloatInner::from(i))
    }
}

impl From<LpcFloatInner> for LpcFloat {
    fn from(i: LpcFloatInner) -> Self {
        Self(i)
    }
}

impl From<LpcInt> for LpcFloat {
    fn from(i: LpcInt) -> Self {
        Self(LpcFloatInner::from(i.0 as BaseFloat))
    }
}

impl PartialEq<BaseFloat> for LpcFloat {
    fn eq(&self, other: &BaseFloat) -> bool {
        self.0 == *other
    }
}