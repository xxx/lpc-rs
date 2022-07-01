use crate::errors::lazy_files::{FileId, FILE_CACHE};
use codespan_reporting::files::Files;
use if_chain::if_chain;
use std::{
    fmt::{Display, Formatter},
    ops::Range,
};

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

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let files = FILE_CACHE.read();

        if_chain! {
            if let Ok(name) = files.name(self.file_id);
            if let Ok(idx) = files.line_index(self.file_id, self.l);
            if let Ok(line_num) = files.line_number(self.file_id, idx);
            if let Ok(column_num) = files.column_number(self.file_id, idx, line_num);
            then {
                write!(f, "{}:{}:{}", name, line_num, column_num)
            } else {
                write!(f, "{:?}", self)
            }
        }
    }
}

/// combine two [`Span`]s together, handling `None` cases.
pub fn combine_spans(left: Option<Span>, right: Option<Span>) -> Span {
    match (left, right) {
        (Some(x), None) | (None, Some(x)) => x,
        (Some(ls), Some(rs)) => {
            // We're not going to deal with it when they cross file boundaries.
            if ls.file_id != rs.file_id {
                return ls;
            }

            let file_id = ls.file_id;

            let l = ls.l;
            let r = rs.r;

            Span { l, r, file_id }
        }
        (None, None) => Span {
            l: 0,
            r: 0,
            file_id: 0,
        },
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
