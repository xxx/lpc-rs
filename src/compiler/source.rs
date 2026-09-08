use std::{
    fmt,
    path::{Path, PathBuf},
    sync::Arc,
};

use lpc_rs_core::lpc_path::{LpcPath, ResolvedPath, SourceName};
use lpc_rs_utils::config::Config;

/// One compiler input's host location, names, and relative-path bases.
#[derive(Clone)]
pub struct CompilerSource {
    resolved: ResolvedPath,
    input: Arc<LpcPath>,
    program: Arc<LpcPath>,
    name: SourceName,
    include_cwd: PathBuf,
    inheritance_cwd: PathBuf,
}

impl CompilerSource {
    /// Establish an input's identity without restricting host-facing compiler inputs.
    pub fn new(path: impl Into<LpcPath>, config: &Config) -> Self {
        let input = Arc::new(path.into());
        let root = config.paths();
        Self {
            resolved: root.source(&input),
            program: Arc::new(root.program_path(&input)),
            name: root.source_name(&input),
            include_cwd: root.source_cwd(&input),
            inheritance_cwd: root.inheritance_cwd(&input),
            input,
        }
    }

    /// The original identity used by function mangling and configured-file comparisons.
    pub fn input(&self) -> &Arc<LpcPath> {
        &self.input
    }

    /// The host location and canonical diagnostic location used to register source text.
    pub fn resolved(&self) -> &ResolvedPath {
        &self.resolved
    }

    /// The safe input spelling used in file errors and inheritance permission applies.
    pub fn name(&self) -> &SourceName {
        &self.name
    }

    /// The program identity, retaining distinct outside-host inputs internally.
    pub fn program_path(&self) -> &Arc<LpcPath> {
        &self.program
    }

    /// The base for relative include directives.
    pub fn include_cwd(&self) -> &Path {
        &self.include_cwd
    }

    /// The inheritance base, preserving host-origin compilation's filename base.
    pub fn inheritance_cwd(&self) -> &Path {
        &self.inheritance_cwd
    }
}

impl fmt::Debug for CompilerSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CompilerSource")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}
