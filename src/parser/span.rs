use crate::errors::lazy_files::FileId;
use std::ops::Range;
use crate::ast::expression_node::first_span;

/// Store the details of a code span, for use in error messaging.
/// `r` is set such that `span.l..span.r` will return the correct span of chars.
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Span {
    /// Left index of the span
    pub l: usize,
    /// Right index of the span
    pub r: usize,
    /// The ID of the file in the global [`FILE_CACHE`](struct@crate::errors::lazy_files::FILE_CACHE)
    pub file_id: FileId,
}

/// combine two [`Span`]s together, handling `None` cases.
pub fn combine_spans(left: Option<Span>, right: Option<Span>) -> Span {
    let file_id = match left {
        Some(x) => x.file_id,
        None => {
            match right {
                Some(x) => x.file_id,
                None => 0
            }
        }
    };

    let l = if let Some(span) = left {
        span.l
    } else {
        0
    };
    let r = if let Some(span) = right {
        span.r
    } else {
        0
    };

    Span {
        l,
        r,
        file_id
    }
}

impl Span {
    /// Create a new [`Span`]
    ///
    /// # Arguments
    /// `file_id` - The [`FileId`] of the file from the [`FILE_CACHE`](struct@crate::errors::lazy_files::FILE_CACHE)
    /// `range` - The range containing the start and end indices of the span.
    ///           It's assumed that `l..r` is the correct code span in error messaging.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::errors::lazy_files::FileCache;
    /// use lpc_rs::parser::span::Span;
    ///
    /// let file_id = FileCache::insert("tests/fixtures/code/example.c");
    /// let span = Span::new(file_id, 1..8);
    /// ```
    pub fn new(file_id: FileId, range: Range<usize>) -> Self {
        Self {
            file_id,
            l: range.start,
            r: range.end,
        }
    }

    pub fn combine(left: Option<Span>, right: Option<Span>) -> Option<Self> {
        if left.is_none() {
            return right;
        }
        if right.is_none() {
            return left;
        }
        let l = left.unwrap();
        let r = right.unwrap();

        let span = Self {
            file_id: l.file_id,
            l: l.l,
            r: r.r,
        };

        Some(span)
    }
}

impl From<&Span> for Range<usize> {
    fn from(span: &Span) -> Self {
        span.l..span.r
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        Self::from(&span)
    }
}
