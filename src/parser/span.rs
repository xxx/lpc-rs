/// Store the endpoints of a text span, for use in error messaging.
/// `r` is set such that span.l..span.r will return the correct span of chars.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    pub l: usize,
    pub r: usize,
}
