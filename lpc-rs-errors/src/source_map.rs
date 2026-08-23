use std::sync::LazyLock;

use codespan_reporting::files::SimpleFiles;
use parking_lot::RwLock;

/// For readability
pub type FileId = usize;

/// Every file registered for diagnostics, holding the text it was compiled
/// from: `Span`s index this text, so rendering never re-reads the disk.
/// Append-only — a recompile registers a fresh id, and an already-compiled
/// program's spans keep indexing the text it was built from.
pub static SOURCE_MAP: LazyLock<RwLock<SimpleFiles<String, String>>> =
    LazyLock::new(|| RwLock::new(SimpleFiles::new()));
