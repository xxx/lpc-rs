use std::{
    fmt::{Display, Formatter},
    ops::Range,
};

use codespan_reporting::files::Files;

use crate::source_map::{FileId, SOURCE_MAP, SourceMap};

const WARNINGS_SUPPRESSED: u32 = 1 << 31;
const FILE_ID_MASK: u32 = WARNINGS_SUPPRESSED - 1;

/// Store the details of a code span, for use in error messaging.
/// `r` is set such that `span.l()..span.r()` will return the correct span of
/// chars. 12 bytes, and the file id's niche lets `Option<Span>` share them.
#[derive(Hash, Copy, Clone, Eq, PartialEq, PartialOrd)]
pub struct Span {
    l: u32,
    r: u32,
    // The low 31 bits hold the source id +1; the high bit suppresses warnings.
    file_id: std::num::NonZeroU32,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("l", &self.l())
            .field("r", &self.r())
            .field("file_id", &self.file_id())
            .field("warnings_suppressed", &self.warnings_suppressed())
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
    /// `file_id` - The [`FileId`] in this compilation's diagnostic sources, up to 2^31 - 2.
    /// `range` - The start and end byte offsets of the span.
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
            file_id: std::num::NonZeroU32::MIN.saturating_add(clamp(file_id).min(FILE_ID_MASK - 1)),
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

    /// The file ID in the diagnostic storage used for this compilation.
    #[inline]
    pub fn file_id(&self) -> FileId {
        ((self.file_id.get() & FILE_ID_MASK) - 1) as FileId
    }

    /// Whether warnings at this source occurrence were disabled during preprocessing.
    #[inline]
    pub fn warnings_suppressed(&self) -> bool {
        self.file_id.get() & WARNINGS_SUPPRESSED != 0
    }

    /// Carry warning policy with a source occurrence without changing its location.
    #[inline]
    pub fn with_warnings_suppressed(mut self, suppressed: bool) -> Self {
        self.file_id = std::num::NonZeroU32::MIN.saturating_add(
            self.file_id() as u32 | if suppressed { WARNINGS_SUPPRESSED } else { 0 },
        );
        self
    }

    /// The span from `left`'s start to `right`'s end; `left` alone when the
    /// two lie in different files; warning policy comes from `left`.
    pub fn join(left: Span, right: Span) -> Self {
        if left.file_id() != right.file_id() {
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

    /// Return source text from the global registry; use [`Self::code_in`] for scoped sources.
    /// Formatting will be as it appears in the source file.
    pub fn code(&self) -> Option<String> {
        let files = SOURCE_MAP.read();
        self.code_in(&files)
    }

    /// Read this span from its compilation's diagnostic sources.
    pub fn code_in(&self, files: &SourceMap) -> Option<String> {
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
        assert_eq!(big.file_id(), FILE_ID_MASK as usize - 1);
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
    fn warning_policy_preserves_source_lookup_and_rendering() {
        for sources in [
            crate::source_map::DiagnosticSources::default(),
            crate::source_map::DiagnosticSources::scoped(),
        ] {
            let id = sources.add("/warning.c".into(), "int value;".into());
            let original = Span::new(id, 4..9);
            let suppressed = original.with_warnings_suppressed(true);
            assert!(suppressed.warnings_suppressed());
            assert_eq!(suppressed.file_id(), id);
            assert_eq!(
                suppressed.code_in(&sources.read()).as_deref(),
                Some("value")
            );
            assert_eq!(Range::from(suppressed), 4..9);
            assert_eq!(suppressed.with_warnings_suppressed(false), original);
            let diagnostic = crate::lpc_error!(Some(suppressed), "still an error");
            let rendered = diagnostic.to_diagnostics();
            assert_eq!(rendered[0].labels[0].file_id, id);
            assert_eq!(rendered[0].labels[0].range, 4..9);
        }
    }

    #[test]
    fn joining_spans_keeps_the_starting_warning_policy() {
        let left = Span::new(0, 1..2);
        let right = Span::new(0, 4..6);
        let span = Span::join(left.with_warnings_suppressed(true), right);
        assert_eq!(Range::from(span), 1..6);
        assert!(span.warnings_suppressed());
        assert!(!Span::join(left, right.with_warnings_suppressed(true)).warnings_suppressed());
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
