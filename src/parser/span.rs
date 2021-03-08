use std::ops::Range;

/// Store the endpoints of a text span, for use in error messaging.
/// `r` is set such that span.l..span.r will return the correct span of chars.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub l: usize,
    pub r: usize,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.l..span.r
    }
}
