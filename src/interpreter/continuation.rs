//! What a frame is waiting on while callee frames above it run.

use crate::interpreter::call_frame::CollectionCall;

/// The call a frame has in flight; the eval loop advances it on every
/// `Ret` into the frame.
#[derive(Debug, Clone)]
pub enum Pending {
    /// A collection `->` mid-way.
    Collection(CollectionCall),
}
