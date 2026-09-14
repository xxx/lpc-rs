use std::{mem::size_of_val, ops::Range};

use codespan_reporting::files::{Error, Files};
use memchr::memchr_iter;

/// One immutable source version with compact, eagerly built line indexes.
#[derive(Debug, Clone)]
pub struct SourceFile {
    name: String,
    source: String,
    line_starts: LineStarts,
}

impl SourceFile {
    pub(super) fn new(name: String, source: String) -> Self {
        let line_starts = LineStarts::new(source.len(), memchr_iter(b'\n', source.as_bytes()));
        Self {
            name,
            source,
            line_starts,
        }
    }

    /// The filename registered for this source version.
    pub fn name(&self) -> &String {
        &self.name
    }

    /// The exact source text registered for this version.
    pub fn source(&self) -> &String {
        &self.source
    }

    pub(super) fn line_index_bytes(&self) -> usize {
        self.line_starts.bytes()
    }

    fn line_start(&self, line: usize) -> Result<usize, Error> {
        if let Some(start) = self.line_starts.get(line) {
            Ok(start)
        } else if line == self.line_starts.len() {
            Ok(self.source.len())
        } else {
            Err(Error::LineTooLarge {
                given: line,
                max: self.line_starts.len() - 1,
            })
        }
    }
}

impl<'a> Files<'a> for SourceFile {
    type FileId = ();
    type Name = String;
    type Source = &'a str;

    fn name(&self, (): ()) -> Result<String, Error> {
        Ok(self.name.clone())
    }

    fn source(&self, (): ()) -> Result<&str, Error> {
        Ok(&self.source)
    }

    fn line_index(&self, (): (), byte_index: usize) -> Result<usize, Error> {
        Ok(self.line_starts.line_index(byte_index))
    }

    fn line_range(&self, (): (), line_index: usize) -> Result<Range<usize>, Error> {
        let start = self.line_start(line_index)?;
        let end = self.line_start(line_index + 1)?;
        Ok(start..end)
    }
}

#[derive(Debug, Clone)]
enum LineStarts {
    Compact(Box<[u32]>),
    Wide(Box<[usize]>),
}

impl LineStarts {
    fn new(source_len: usize, newlines: impl Iterator<Item = usize> + Clone) -> Self {
        let count = newlines.clone().count() + 1;
        if u32::try_from(source_len).is_ok() {
            let mut starts = Vec::with_capacity(count);
            starts.push(0);
            starts.extend(newlines.map(|offset| (offset + 1) as u32));
            Self::Compact(starts.into_boxed_slice())
        } else {
            let mut starts = Vec::with_capacity(count);
            starts.push(0);
            starts.extend(newlines.map(|offset| offset + 1));
            Self::Wide(starts.into_boxed_slice())
        }
    }

    fn line_index(&self, byte: usize) -> usize {
        let result = match self {
            Self::Compact(starts) => starts.binary_search_by_key(&byte, |&start| start as usize),
            Self::Wide(starts) => starts.binary_search(&byte),
        };
        result.unwrap_or_else(|next| next - 1)
    }

    fn get(&self, line: usize) -> Option<usize> {
        match self {
            Self::Compact(starts) => starts.get(line).map(|&start| start as usize),
            Self::Wide(starts) => starts.get(line).copied(),
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Compact(starts) => starts.len(),
            Self::Wide(starts) => starts.len(),
        }
    }

    fn bytes(&self) -> usize {
        match self {
            Self::Compact(starts) => size_of_val(starts.as_ref()),
            Self::Wide(starts) => size_of_val(starts.as_ref()),
        }
    }
}

#[cfg(test)]
mod tests {
    use codespan_reporting::files::SimpleFile;

    use super::*;

    #[test]
    fn compact_indexes_preserve_codespan_locations_and_errors() {
        for source in ["", "a", "a\n", "\n\n", "α💖\r\nx", "one\rtwo"] {
            let original = SimpleFile::new("/source.c".to_owned(), source.to_owned());
            let compact = SourceFile::new("/source.c".to_owned(), source.to_owned());
            assert_eq!(compact.name(), original.name());
            assert_eq!(compact.source(), original.source());
            for byte in 0..source.len() + 3 {
                assert_eq!(
                    compact.line_index((), byte).unwrap(),
                    original.line_index((), byte).unwrap(),
                );
                assert_eq!(
                    compact.location((), byte).unwrap(),
                    original.location((), byte).unwrap(),
                );
            }
            for line in (0..source.bytes().filter(|&b| b == b'\n').count() + 4).chain([usize::MAX])
            {
                assert_eq!(
                    format!("{:?}", compact.line_range((), line)),
                    format!("{:?}", original.line_range((), line)),
                );
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn wide_offsets_preserve_positions_beyond_the_u32_limit() {
        let limit = u32::MAX as usize;
        let compact = LineStarts::new(limit, [limit - 1].into_iter());
        assert!(matches!(compact, LineStarts::Compact(_)));
        assert_eq!(compact.get(1), Some(limit));
        assert_eq!(compact.line_index(usize::MAX), 1);
        assert_eq!(compact.bytes(), 2 * size_of::<u32>());

        let wide = LineStarts::new(limit + 2, [limit - 1, limit + 1].into_iter());
        assert!(matches!(wide, LineStarts::Wide(_)));
        assert_eq!(wide.get(1), Some(limit));
        assert_eq!(wide.get(2), Some(limit + 2));
        assert_eq!(wide.line_index(limit + 1), 1);
        assert_eq!(wide.line_index(usize::MAX), 2);
        assert_eq!(wide.bytes(), 3 * size_of::<usize>());
    }
}
