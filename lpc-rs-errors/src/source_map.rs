use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasher,
    ops::Range,
    sync::LazyLock,
};

use codespan_reporting::files::{Error, Files};
use parking_lot::RwLock;

mod source_file;

pub use source_file::SourceFile;

/// For readability
pub type FileId = usize;

/// Every file registered for diagnostics, holding the text it was compiled
/// from: `Span`s index this text, so rendering never re-reads the disk.
/// Unchanged files reuse their id; changed text gets a new id so existing
/// spans keep indexing the version they were compiled from.
pub static SOURCE_MAP: LazyLock<RwLock<SourceMap>> =
    LazyLock::new(|| RwLock::new(SourceMap::default()));

/// Immutable diagnostic sources, shared across recompiles by name and contents.
#[derive(Debug, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
    by_content: HashMap<u64, Vec<FileId>>,
}

/// Retained source storage, excluding allocator overhead and hidden container capacities.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMapStats {
    /// Number of distinct filename/content pairs.
    pub file_versions: usize,
    /// Number of distinct filenames across all versions.
    pub unique_files: usize,
    /// UTF-8 text bytes across all versions, excluding spare capacity.
    pub source_bytes: usize,
    /// Source string buffer capacities, including spare capacity.
    pub source_capacity_bytes: usize,
    /// Filename string buffer capacities across all versions.
    pub filename_capacity_bytes: usize,
    /// Bytes occupied by the line-start arrays, excluding allocator rounding.
    pub line_index_bytes: usize,
}

impl SourceMap {
    /// Register a source version, reusing its id when its name and text match.
    pub fn add(&mut self, name: String, source: String) -> FileId {
        let hash = self.by_content.hasher().hash_one((&name, &source));
        let bucket = self.by_content.entry(hash).or_default();
        for &id in bucket.iter() {
            let file = self.files.get(id).expect("indexed source exists");
            if file.name() == &name && file.source() == &source {
                return id;
            }
        }
        let id = self.files.len();
        self.files.push(SourceFile::new(name, source));
        bucket.push(id);
        id
    }

    /// The immutable source version associated with an id.
    pub fn get(&self, id: FileId) -> Result<&SourceFile, Error> {
        self.files.get(id).ok_or(Error::FileMissing)
    }

    /// Inspect retained versions without copying or scanning their source text.
    ///
    /// The temporary filename set is excluded from the returned measurements.
    /// Returns an error if a registered file cannot be read.
    pub fn stats(&self) -> Result<SourceMapStats, Error> {
        let mut stats = SourceMapStats::default();
        let mut names = HashSet::new();
        for &id in self.by_content.values().flatten() {
            let file = self.get(id)?;
            names.insert(file.name().as_str());
            stats.file_versions += 1;
            stats.source_bytes += file.source().len();
            stats.source_capacity_bytes += file.source().capacity();
            stats.filename_capacity_bytes += file.name().capacity();
            stats.line_index_bytes += file.line_index_bytes();
        }
        stats.unique_files = names.len();
        Ok(stats)
    }
}

impl<'a> Files<'a> for SourceMap {
    type FileId = FileId;
    type Name = String;
    type Source = &'a str;

    fn name(&self, id: FileId) -> Result<String, Error> {
        Ok(self.get(id)?.name().clone())
    }

    fn source(&self, id: FileId) -> Result<&str, Error> {
        Ok(self.get(id)?.source())
    }

    fn line_index(&self, id: FileId, byte_index: usize) -> Result<usize, Error> {
        self.get(id)?.line_index((), byte_index)
    }

    fn line_range(&self, id: FileId, line_index: usize) -> Result<Range<usize>, Error> {
        self.get(id)?.line_range((), line_index)
    }
}

#[cfg(test)]
mod tests {
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term::{self, termcolor::NoColor},
    };

    use super::*;

    #[test]
    fn missing_file_ids_report_file_missing() {
        let mut sources = SourceMap::default();
        sources.add("/present.c".into(), "int x;".into());
        for id in [1, usize::MAX] {
            assert!(matches!(sources.get(id), Err(Error::FileMissing)));
            assert!(matches!(sources.name(id), Err(Error::FileMissing)));
            assert!(matches!(sources.source(id), Err(Error::FileMissing)));
            assert!(matches!(sources.line_index(id, 0), Err(Error::FileMissing)));
            assert!(matches!(sources.line_range(id, 0), Err(Error::FileMissing)));
        }
    }

    #[test]
    fn compact_storage_preserves_rendered_diagnostics_across_source_versions() {
        let inputs = [
            ("/source.c", "int value = 1;\n// café 💖\n"),
            ("/source.c", "int value = 2;\n"),
            ("/include.h", "#define VALUE 2\n"),
            ("/empty.c", ""),
        ];
        let mut original = SimpleFiles::new();
        let mut compact = SourceMap::default();
        for (name, source) in inputs {
            let expected = original.add(name.to_owned(), source.to_owned());
            assert_eq!(compact.add(name.to_owned(), source.to_owned()), expected);
        }
        let heart = inputs[0].1.find('💖').unwrap();
        let diagnostics = [
            Diagnostic::error()
                .with_message("source locations")
                .with_labels(vec![
                    Label::primary(0, heart..heart + '💖'.len_utf8()).with_message("old version"),
                    Label::secondary(1, 0..3).with_message("new version"),
                    Label::secondary(2, 0..7).with_message("include"),
                ]),
            Diagnostic::warning().with_labels(vec![Label::primary(3, 0..0)]),
            Diagnostic::error().with_labels(vec![Label::primary(0, 0..inputs[0].1.len())]),
        ];
        fn render<'a>(
            files: &'a impl Files<'a, FileId = FileId>,
            diagnostic: &Diagnostic<FileId>,
        ) -> Vec<u8> {
            let mut writer = NoColor::new(Vec::new());
            term::emit_to_write_style(&mut writer, &term::Config::default(), files, diagnostic)
                .unwrap();
            writer.into_inner()
        }
        for diagnostic in diagnostics {
            assert_eq!(
                render(&compact, &diagnostic),
                render(&original, &diagnostic)
            );
        }
    }

    #[test]
    fn stats_distinguish_text_bytes_from_buffer_capacity() {
        let mut sources = SourceMap::default();
        assert_eq!(sources.stats().unwrap(), SourceMapStats::default());
        let mut name = String::with_capacity(64);
        name.push_str("/café.c");
        let mut text = String::with_capacity(128);
        text.push_str("// café\n\n");
        let expected = SourceMapStats {
            file_versions: 1,
            unique_files: 1,
            source_bytes: text.len(),
            source_capacity_bytes: text.capacity(),
            filename_capacity_bytes: name.capacity(),
            line_index_bytes: 3 * size_of::<u32>(),
        };
        sources.add(name, text);
        assert_eq!(sources.stats().unwrap(), expected);
        assert_eq!(sources.stats().unwrap(), expected);
    }

    #[test]
    fn line_index_bytes_include_empty_and_unterminated_lines() {
        let mut sources = SourceMap::default();
        for (name, text) in [("/empty.c", ""), ("/one.c", "x"), ("/two.c", "x\ny")] {
            sources.add(name.into(), text.into());
        }
        assert_eq!(
            sources.stats().unwrap().line_index_bytes,
            4 * size_of::<u32>()
        );
    }

    #[test]
    fn repeated_failed_source_versions_share_one_entry() {
        let mut sources = SourceMap::default();
        let source = "int move() {\n    return 1\n}\n";
        let id = sources.add("/cover.c".into(), source.into());
        let before = sources.stats().unwrap();
        for _ in 0..1000 {
            assert_eq!(sources.add("/cover.c".into(), source.into()), id);
        }
        assert!(sources.get(id + 1).is_err());
        assert_eq!(sources.stats().unwrap(), before);
    }

    #[test]
    fn edits_preserve_old_locations_and_reverting_reuses_the_old_version() {
        let mut sources = SourceMap::default();
        let old = "int move() {\n    return 1\n}\n";
        let fixed = "int move() { return 1; }\n";
        let old_id = sources.add("/cover.c".into(), old.into());
        let fixed_id = sources.add("/cover.c".into(), fixed.into());
        assert_ne!(old_id, fixed_id);
        assert_eq!(sources.source(old_id).unwrap(), old);
        assert_eq!(sources.source(fixed_id).unwrap(), fixed);
        assert_eq!(
            sources.line_index(old_id, old.find('}').unwrap()).unwrap(),
            2
        );
        assert_eq!(
            sources
                .line_index(fixed_id, fixed.find('}').unwrap())
                .unwrap(),
            0
        );
        assert_eq!(sources.add("/cover.c".into(), old.into()), old_id);
        let stats = sources.stats().unwrap();
        assert_eq!((stats.file_versions, stats.unique_files), (2, 1));
        assert_eq!(stats.source_bytes, old.len() + fixed.len());
    }

    #[test]
    fn identical_text_under_different_names_keeps_distinct_locations() {
        let mut sources = SourceMap::default();
        let first = sources.add("/first.c".into(), "int x;".into());
        let second = sources.add("/second.c".into(), "int x;".into());
        assert_ne!(first, second);
        assert_eq!(sources.name(first).unwrap(), "/first.c");
        assert_eq!(sources.name(second).unwrap(), "/second.c");
        let stats = sources.stats().unwrap();
        assert_eq!((stats.file_versions, stats.unique_files), (2, 2));
        assert_eq!(stats.source_bytes, 2 * "int x;".len());
    }
}
