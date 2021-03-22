use crate::errors::lazy_files::FileId;
use std::ops::Range;

/// Store the details of a code span, for use in error messaging.
/// `r` is set such that `span.l..span.r` will return the correct span of chars.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub l: usize,
    pub r: usize,
    pub file_id: FileId,
}

impl Span {
    pub fn new(file_id: FileId, range: Range<usize>) -> Self {
        Self {
            file_id,
            l: range.start,
            r: range.end,
        }
    }
}

impl From<&Span> for Range<usize> {
    fn from(span: &Span) -> Self {
        span.l..span.r
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self { Self::from(&span) }
}
