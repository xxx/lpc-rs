use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use lpc_rs_core::LpcIntInner as CoreInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LpcInt(pub CoreInt);

impl Display for LpcInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}