use std::{collections::HashMap, hash::BuildHasher, ops::Range, sync::LazyLock};

use codespan_reporting::files::{Error, Files, SimpleFile, SimpleFiles};
use parking_lot::RwLock;

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
    files: SimpleFiles<String, String>,
    by_content: HashMap<u64, Vec<FileId>>,
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
        let id = self.files.add(name, source);
        bucket.push(id);
        id
    }

    /// The immutable source version associated with an id.
    pub fn get(&self, id: FileId) -> Result<&SimpleFile<String, String>, Error> {
        self.files.get(id)
    }
}

impl<'a> Files<'a> for SourceMap {
    type FileId = FileId;
    type Name = String;
    type Source = &'a str;

    fn name(&self, id: FileId) -> Result<String, Error> {
        self.files.name(id)
    }

    fn source(&self, id: FileId) -> Result<&str, Error> {
        self.files.source(id)
    }

    fn line_index(&self, id: FileId, byte_index: usize) -> Result<usize, Error> {
        self.files.line_index(id, byte_index)
    }

    fn line_range(&self, id: FileId, line_index: usize) -> Result<Range<usize>, Error> {
        self.files.line_range(id, line_index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repeated_failed_source_versions_share_one_entry() {
        let mut sources = SourceMap::default();
        let source = "int move() {\n    return 1\n}\n";
        let id = sources.add("/cover.c".into(), source.into());
        for _ in 0..1000 {
            assert_eq!(sources.add("/cover.c".into(), source.into()), id);
        }
        assert!(sources.get(id + 1).is_err());
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
    }

    #[test]
    fn identical_text_under_different_names_keeps_distinct_locations() {
        let mut sources = SourceMap::default();
        let first = sources.add("/first.c".into(), "int x;".into());
        let second = sources.add("/second.c".into(), "int x;".into());
        assert_ne!(first, second);
        assert_eq!(sources.name(first).unwrap(), "/first.c");
        assert_eq!(sources.name(second).unwrap(), "/second.c");
    }
}
