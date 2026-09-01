use std::{
    fmt::{Display, Formatter},
    ops::Range,
};

use codespan_reporting::files::Files;

use crate::source_map::{FileId, SOURCE_MAP};

/// Store the details of a code span, for use in error messaging.
/// `r` is set such that `span.l()..span.r()` will return the correct span of
/// chars. 12 bytes, and the file id's niche lets `Option<Span>` share them.
#[derive(Hash, Copy, Clone, Eq, PartialEq, PartialOrd)]
pub struct Span {
    l: u32,
    r: u32,
    // The SOURCE_MAP id, stored +1 for the niche.
    file_id: std::num::NonZeroU32,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("l", &self.l())
            .field("r", &self.r())
            .field("file_id", &self.file_id())
            .finish()
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let files = SOURCE_MAP.read();

        if let Ok(name) = files.name(self.file_id())
            && let Ok(idx) = files.line_index(self.file_id(), self.l())
            && let Ok(line_num) = files.line_number(self.file_id(), idx)
            && let Ok(column_num) = files.column_number(self.file_id(), idx, self.l())
        {
            write!(f, "{name}:{line_num}:{column_num}")
        } else {
            write!(f, "{self:?}")
        }
    }
}

impl Span {
    /// Create a new [`Span`]
    ///
    /// # Arguments
    /// `file_id` - The [`FileId`] of the file in the
    /// [`SOURCE_MAP`](static@crate::source_map::SOURCE_MAP). `range` - The
    /// range containing the start and end indices of the span.
    ///           It's assumed that `l..r` is the correct code span in error
    /// messaging.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs_errors::{source_map::SOURCE_MAP, span::Span};
    ///
    /// let file_id = SOURCE_MAP
    ///     .write()
    ///     .add("/example.c".to_owned(), "int x = 123;\n".to_owned());
    /// let span = Span::new(file_id, 1..8);
    /// ```
    #[inline]
    pub fn new(file_id: FileId, range: Range<usize>) -> Self {
        // Saturate: a span is a diagnostic, and a wrong caret on a 4 GiB
        // input never justifies a panic.
        let clamp = |x: usize| u32::try_from(x).unwrap_or(u32::MAX);
        Self {
            l: clamp(range.start),
            r: clamp(range.end),
            file_id: std::num::NonZeroU32::MIN.saturating_add(clamp(file_id)),
        }
    }

    /// Left index of the span
    #[inline]
    pub fn l(&self) -> usize {
        self.l as usize
    }

    /// Right index of the span
    #[inline]
    pub fn r(&self) -> usize {
        self.r as usize
    }

    /// The ID of the file in the global [`SOURCE_MAP`](static@SOURCE_MAP)
    #[inline]
    pub fn file_id(&self) -> FileId {
        (self.file_id.get() - 1) as FileId
    }

    /// The span from `left`'s start to `right`'s end; `left` alone when the
    /// two lie in different files.
    pub fn join(left: Span, right: Span) -> Self {
        if left.file_id != right.file_id {
            return left;
        }

        Self {
            l: left.l,
            r: right.r,
            file_id: left.file_id,
        }
    }

    /// [`Span::join`] over spans that may be missing; a missing side yields
    /// the other, and two missing sides stay missing.
    pub fn combine(left: Option<Span>, right: Option<Span>) -> Option<Self> {
        match (left, right) {
            (Some(l), Some(r)) => Some(Self::join(l, r)),
            (l, r) => l.or(r),
        }
    }

    /// Return the string of the actual source code that this span represents.
    /// Formatting will be as it appears in the source file.
    pub fn code(&self) -> Option<String> {
        let files = SOURCE_MAP.read();

        files.get(self.file_id()).ok().and_then(|f| {
            f.source()
                .get(self.l()..self.r())
                .map(|code| code.to_string())
        })
    }
}

impl From<&Span> for Range<usize> {
    #[inline]
    fn from(span: &Span) -> Self {
        span.l()..span.r()
    }
}

impl From<Span> for Range<usize> {
    #[inline]
    fn from(span: Span) -> Self {
        Self::from(&span)
    }
}

pub trait HasSpan {
    /// Return a [`Span`] for this object
    fn span(&self) -> Span;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_span_is_twelve_bytes_and_option_shares_them() {
        assert_eq!(std::mem::size_of::<Span>(), 12);
        assert_eq!(std::mem::size_of::<Option<Span>>(), 12);
    }

    #[test]
    fn accessors_round_trip_and_saturate() {
        let span = Span::new(7, 3..9);
        assert_eq!(span.file_id(), 7);
        assert_eq!(span.l(), 3);
        assert_eq!(span.r(), 9);

        let big = Span::new(usize::MAX, usize::MAX..usize::MAX);
        assert_eq!(big.file_id(), u32::MAX as usize - 1);
        assert_eq!(big.l(), u32::MAX as usize);
        assert_eq!(big.r(), u32::MAX as usize);
    }

    #[test]
    fn display_is_the_name_line_and_column_of_the_start() {
        let file_id = crate::source_map::SOURCE_MAP
            .write()
            .add("/where.c".to_owned(), "int x;\nint y = 42;\n".to_owned());
        assert_eq!(Span::new(file_id, 0..3).to_string(), "/where.c:1:1");
        assert_eq!(Span::new(file_id, 15..17).to_string(), "/where.c:2:9");
    }

    #[test]
    fn combine_keeps_a_missing_span_missing() {
        assert_eq!(Span::combine(None, None), None);
        let only = Some(Span::new(0, 3..5));
        assert_eq!(Span::combine(only, None), only);
        assert_eq!(Span::combine(None, only), only);
    }

    #[test]
    fn combine_stays_in_the_left_file() {
        let left = Span::new(1, 0..4);
        let right = Span::new(2, 10..20);
        assert_eq!(Span::combine(Some(left), Some(right)), Some(left));
        assert_eq!(Span::join(left, right), left);
    }

    #[test]
    fn join_spans_the_two_ends() {
        let left = Span::new(0, 0..4);
        let right = Span::new(0, 10..20);
        assert_eq!(Span::join(left, right), Span::new(0, 0..20));
        assert_eq!(
            Span::combine(Some(left), Some(right)),
            Some(Span::new(0, 0..20))
        );
    }
}
