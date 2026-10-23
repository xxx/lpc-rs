use std::fmt::{Display, Formatter};

use lpc_rs_core::{BaseFloat, LpcFloatInner};

use crate::interpreter::lpc_int::LpcInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[cfg(test)]
mod tests {
    use super::LpcFloat;

    #[test]
    fn comparison_with_base_floats_distinguishes_unequal_values() {
        for (left, right, expected) in [
            (-2.5, -2.5, true),
            (-2.5, 2.5, false),
            (-0.0, 0.0, true),
            (1.5, -1.5, false),
        ] {
            assert_eq!(LpcFloat::from(left) == right, expected, "{left}, {right}");
        }
    }
}
