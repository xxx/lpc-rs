//! Shared concrete styles, control filtering, and Unicode terminal layout.

mod colour;
mod format;

pub(super) use colour::Style;
pub(super) use format::{LIMIT, Part, Text, next_part};
