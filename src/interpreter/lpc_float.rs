use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use lpc_rs_core::LpcFloat as CoreFloat;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LpcFloat(pub CoreFloat);

impl Display for LpcFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
