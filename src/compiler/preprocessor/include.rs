//! The include walk: the one owner of `#include` traversal for a compile.

use std::{
    collections::{HashMap, HashSet},
    fmt,
    io::ErrorKind,
    path::{Path, PathBuf},
    sync::Arc,
};

use lpc_rs_core::lpc_path::{LpcPath, ResolvedPath};
use lpc_rs_errors::{
    LpcError, Result, lpc_error,
    source_map::{DiagnosticSources, FileId},
    span::Span,
};
use lpc_rs_utils::{LpcSource, config::Config};
use tracing::instrument;

use crate::compiler::{
    compile_gate::CompileGate,
    diagnostics::{Diagnostics, latin1_warning},
    source::CompilerSource,
    source_reader::{SourceKind, SourceReader},
};

/// Deepest `#include` nesting allowed, the root file included.
pub(super) const MAX_INCLUDE_DEPTH: usize = 64;

/// The pragma name that marks the current file include-once.
pub(super) const ONCE: &str = "once";

/// The pragma name that forbids textual inclusion of the current file.
pub(super) const NO_INCLUDE: &str = "no_include";

/// One entry form of an include.
#[derive(Debug)]
pub(super) enum IncludeSource<'a> {
    /// `#include <path>`
    System {
        /// The directive's path text.
        path: &'a str,
    },
    /// `#include "path"`
    Local {
        /// The directive's path text.
        path: &'a str,
    },
    /// The configured auto-include file.
    Configured(&'a LpcPath),
}

/// A successfully opened include: its registered id and its text.
#[derive(Debug)]
pub(super) struct Opened {
    /// The diagnostic source ID for this version of the file.
    pub file_id: FileId,
    /// The file's text, shared with the memo.
    pub content: Arc<str>,
}

/// One file on the active include chain.
struct Frame {
    path: ResolvedPath,
    cwd: PathBuf,
}

impl fmt::Debug for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

/// The one owner of `#include` traversal for a compile: resolution,
/// containment, IO, source registration, the active chain, the
/// depth cap, and the `#pragma once` set. The memo, once and cycle keys
/// are canonical server paths; the name registered for rendering is the
/// in-game path.
#[derive(Debug, Default)]
pub(super) struct IncludeWalk {
    sources: DiagnosticSources,
    /// The active chain, root first.
    stack: Vec<Frame>,
    /// One source read and one diagnostic source ID per file per compile.
    memo: HashMap<PathBuf, (FileId, Arc<str>)>,
    /// Files marked `#pragma once`.
    once: HashSet<PathBuf>,
}

impl IncludeWalk {
    pub fn new(sources: DiagnosticSources) -> Self {
        Self {
            sources,
            ..Self::default()
        }
    }

    /// Register the root file's text and push its frame. Called once,
    /// first, by `scan`.
    pub fn open_root(&mut self, source: &CompilerSource, code: &str) -> FileId {
        let path = source.resolved().clone();
        let canon = path.server().to_owned();
        let file_id = self.sources.add(path.name().to_string(), code.to_owned());
        self.memo.insert(canon, (file_id, Arc::from(code)));
        self.stack.push(Frame {
            path,
            cwd: source.include_cwd().to_owned(),
        });
        file_id
    }

    /// Resolve and open one include, pushing its frame. `Ok(None)` is
    /// the `#pragma once` skip; the caller scans `Some` and then
    /// [`close`](Self::close)s. `gate` is asked for every directive but a
    /// configured one, before anything is read.
    #[instrument(skip(self, config, gate, reader, diagnostics))]
    pub async fn open(
        &mut self,
        source: IncludeSource<'_>,
        span: Option<Span>,
        config: &Config,
        gate: Option<&dyn CompileGate>,
        reader: &dyn SourceReader,
        diagnostics: &mut Diagnostics,
    ) -> Result<Option<Opened>> {
        let configured = matches!(source, IncludeSource::Configured(_));
        // An out-of-root path collapses to an empty in-game form, so the
        // error names the directive's own text.
        let directive_text = match &source {
            IncludeSource::System { path } | IncludeSource::Local { path } => (*path).to_string(),
            IncludeSource::Configured(path) => config.paths().source_name(path).to_string(),
        };

        let input = self.resolve(source, config, reader).await;
        let path = config.paths().confine(&input).map_err(|_| {
            lpc_error!(
                span,
                "attempt to include a file outside the root: `{}`",
                directive_text
            )
        })?;
        let canon = path.server().to_owned();

        if let (Some(gate), false) = (gate, configured) {
            let in_game = path.name().as_str();
            let from = self.current_in_game();
            if !gate.include(in_game, &from).await? {
                return Err(lpc_error!(
                    span,
                    "#include \"{}\": permission denied",
                    in_game
                ));
            }
        }

        // Before the cycle check: a once-marked file on the active
        // chain is a skip, not a cycle.
        if self.once.contains(&canon) {
            return Ok(None);
        }

        if self.stack.iter().any(|frame| frame.path.server() == canon) {
            return Err(self.cycle_error(&path, span));
        }

        if self.stack.len() >= MAX_INCLUDE_DEPTH {
            return Err(lpc_error!(span, "`#include` nests too deeply"));
        }

        let (file_id, content) = match self.memo.get(&canon) {
            Some((file_id, content)) => (*file_id, content.clone()),
            None => {
                let source = match reader.read(&canon).await.map(LpcSource::from_bytes) {
                    Ok(source) => source,
                    Err(e) => {
                        if e.kind() == ErrorKind::IsADirectory
                            || reader.kind(&canon).await.ok() == Some(SourceKind::Directory)
                        {
                            return Err(lpc_error!(
                                span,
                                "attempt to include a directory: `{}`",
                                path
                            ));
                        }
                        return Err(lpc_error!(
                            span,
                            "unable to read include file `{}`: {}",
                            path,
                            e
                        ));
                    }
                };
                if source.latin1 {
                    let in_game = path.name().as_str();
                    diagnostics.record(latin1_warning(in_game, span));
                }
                let text = source.text;
                let file_id = self.sources.add(path.name().to_string(), text.clone());
                let content: Arc<str> = Arc::from(text);
                self.memo.insert(canon.clone(), (file_id, content.clone()));
                (file_id, content)
            }
        };

        let cwd = config.paths().source_cwd(path.input());
        self.stack.push(Frame { path, cwd });
        Ok(Some(Opened { file_id, content }))
    }

    /// Pop the active frame — the success and error paths alike.
    pub fn close(&mut self) {
        let popped = self.stack.pop();
        debug_assert!(popped.is_some(), "include close without an open");
    }

    /// Whether the active file is the root rather than an `#include`.
    pub fn at_root(&self) -> bool {
        self.stack.len() == 1
    }

    /// Mark the active file `#pragma once` (idempotent).
    pub fn mark_once(&mut self) {
        let frame = self
            .stack
            .last()
            .expect("a pragma executes inside an open file");
        self.once.insert(frame.path.server().to_owned());
    }

    /// Turn a directive's path into an [`LpcPath`], relative to the
    /// including file — the active frame.
    async fn resolve(
        &self,
        source: IncludeSource<'_>,
        config: &Config,
        reader: &dyn SourceReader,
    ) -> LpcPath {
        let path = match &source {
            IncludeSource::Configured(path) => return (*path).clone(),
            IncludeSource::Local { path } | IncludeSource::System { path } => *path,
        };
        if Path::new(path).is_absolute()
            || config
                .system_include_dirs
                .iter()
                .all(|dir| Path::new(dir.as_str()) == self.cwd())
        {
            return LpcPath::new_in_game(path, self.cwd(), &*config.lib_dir);
        }

        match source {
            IncludeSource::Configured(path) => path.clone(),
            IncludeSource::Local { path } => {
                let local = LpcPath::new_in_game(path, self.cwd(), &*config.lib_dir);
                if reader
                    .kind(config.paths().source(&local).server())
                    .await
                    .is_ok()
                {
                    return local;
                }
                self.in_system_dirs(path, config, reader)
                    .await
                    .unwrap_or(local)
            }
            IncludeSource::System { path } => self
                .in_system_dirs(path, config, reader)
                .await
                .unwrap_or_else(|| LpcPath::new_in_game(path, self.cwd(), &*config.lib_dir)),
        }
    }

    /// The first configured system dir holding `path`, in order.
    async fn in_system_dirs(
        &self,
        path: &str,
        config: &Config,
        reader: &dyn SourceReader,
    ) -> Option<LpcPath> {
        let root = config.paths();
        for dir in &config.system_include_dirs {
            let Ok(candidate) = root.resolve(path, dir.as_str()) else {
                continue;
            };
            if reader.kind(candidate.server()).await.is_ok() {
                return Some(candidate.input().clone());
            }
        }
        None
    }

    /// The including file's directory — the resolution cwd.
    fn cwd(&self) -> &Path {
        self.stack
            .last()
            .map(|frame| frame.cwd.as_path())
            .unwrap_or_else(|| Path::new("/"))
    }

    /// The including file — the active frame — as an in-game path.
    fn current_in_game(&self) -> String {
        self.stack
            .last()
            .map(|frame| frame.path.name().to_string())
            .unwrap_or_else(|| "/".to_string())
    }

    /// The cycle diagnostic; `scan_include` adds the chain labels.
    fn cycle_error(&self, path: &ResolvedPath, span: Option<Span>) -> LpcError {
        lpc_error!(
            span,
            "cyclic `#include`: `{}` is already being included",
            path
        )
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;
    use std::{
        path::Path,
        sync::atomic::{AtomicUsize, Ordering},
    };

    use async_trait::async_trait;

    use super::*;
    use crate::compiler::source_reader::DiskSourceReader;
    use crate::test_support::TempLib;

    fn config_at(root: &Path) -> Config {
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .build()
            .unwrap()
    }

    fn rooted(config: &Config) -> IncludeWalk {
        let mut walk = IncludeWalk::default();
        walk.open_root(
            &CompilerSource::new(
                LpcPath::new_in_game("/main.c", "/", &*config.lib_dir),
                config,
            ),
            "int x;\n",
        );
        walk
    }

    #[derive(Debug, Default)]
    struct CountingReader {
        probes: AtomicUsize,
        reads: AtomicUsize,
    }

    #[async_trait]
    impl SourceReader for CountingReader {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.reads.fetch_add(1, Ordering::Relaxed);
            DiskSourceReader.read(path).await
        }

        async fn kind(&self, path: &Path) -> std::io::Result<SourceKind> {
            self.probes.fetch_add(1, Ordering::Relaxed);
            DiskSourceReader.kind(path).await
        }
    }

    #[tokio::test]
    async fn unambiguous_headers_do_not_need_resolution_probes() {
        let root = TempLib::new("include-probes");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/"])
            .build()
            .unwrap();
        let configured = LpcPath::new_server(root.join("a.h"));
        for (source, probes) in [
            (IncludeSource::Local { path: "a.h" }, 0),
            (IncludeSource::System { path: "a.h" }, 0),
            (IncludeSource::Configured(&configured), 0),
        ] {
            let reader = CountingReader::default();
            let opened = rooted(&config)
                .open(
                    source,
                    None,
                    &config,
                    None,
                    &reader,
                    &mut Diagnostics::default(),
                )
                .await
                .unwrap()
                .unwrap();
            assert_eq!(
                (
                    opened.content.as_ref(),
                    reader.probes.load(Ordering::Relaxed),
                    reader.reads.load(Ordering::Relaxed),
                ),
                ("1\n", probes, 1),
            );
        }
    }

    #[tokio::test]
    async fn absolute_headers_do_not_search_distinct_system_directories() {
        let root = TempLib::new("absolute-header-resolution");
        std::fs::write(root.join("a.h"), "absolute\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/first", "/second"])
            .build()
            .unwrap();
        for source in [
            IncludeSource::Local { path: "/a.h" },
            IncludeSource::System { path: "/a.h" },
        ] {
            let reader = CountingReader::default();
            let opened = rooted(&config)
                .open(
                    source,
                    None,
                    &config,
                    None,
                    &reader,
                    &mut Diagnostics::default(),
                )
                .await
                .unwrap()
                .unwrap();
            assert_eq!(
                (
                    opened.content.as_ref(),
                    reader.probes.load(Ordering::Relaxed),
                    reader.reads.load(Ordering::Relaxed),
                ),
                ("absolute\n", 0, 1),
            );
        }
    }

    #[tokio::test]
    async fn local_headers_precede_distinct_system_directories() {
        let root = TempLib::new("local-header-precedence");
        std::fs::create_dir(root.join("include")).unwrap();
        std::fs::write(root.join("pick.h"), "local\n").unwrap();
        std::fs::write(root.join("include/pick.h"), "system\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/include"])
            .build()
            .unwrap();
        let reader = CountingReader::default();
        let opened = rooted(&config)
            .open(
                IncludeSource::Local { path: "pick.h" },
                None,
                &config,
                None,
                &reader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            (
                opened.content.as_ref(),
                reader.probes.load(Ordering::Relaxed),
                reader.reads.load(Ordering::Relaxed),
            ),
            ("local\n", 1, 1),
        );
    }

    #[tokio::test]
    async fn a_missing_local_header_uses_the_first_existing_system_directory() {
        let root = TempLib::new("local-header-system-fallback");
        for name in ["first", "second"] {
            std::fs::create_dir(root.join(name)).unwrap();
            std::fs::write(root.join(name).join("pick.h"), format!("{name}\n")).unwrap();
        }
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/missing", "/first", "/second"])
            .build()
            .unwrap();
        let reader = CountingReader::default();
        let opened = rooted(&config)
            .open(
                IncludeSource::Local { path: "pick.h" },
                None,
                &config,
                None,
                &reader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            (
                opened.content.as_ref(),
                reader.probes.load(Ordering::Relaxed),
                reader.reads.load(Ordering::Relaxed),
            ),
            ("first\n", 3, 1),
        );
    }

    #[tokio::test]
    async fn system_headers_fall_back_to_the_including_directory() {
        let root = TempLib::new("system-header-local-fallback");
        std::fs::write(root.join("pick.h"), "fallback\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/missing", "/also_missing"])
            .build()
            .unwrap();
        let reader = CountingReader::default();
        let opened = rooted(&config)
            .open(
                IncludeSource::System { path: "pick.h" },
                None,
                &config,
                None,
                &reader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            (
                opened.content.as_ref(),
                reader.probes.load(Ordering::Relaxed),
                reader.reads.load(Ordering::Relaxed),
            ),
            ("fallback\n", 2, 1),
        );
    }

    #[tokio::test]
    async fn a_later_system_directory_is_checked_after_the_including_directory() {
        let root = TempLib::new("later-system-header");
        std::fs::create_dir(root.join("include")).unwrap();
        std::fs::write(root.join("include/pick.h"), "later\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/", "/include"])
            .build()
            .unwrap();
        for (source, probes) in [
            (IncludeSource::Local { path: "pick.h" }, 3),
            (IncludeSource::System { path: "pick.h" }, 2),
        ] {
            let reader = CountingReader::default();
            let opened = rooted(&config)
                .open(
                    source,
                    None,
                    &config,
                    None,
                    &reader,
                    &mut Diagnostics::default(),
                )
                .await
                .unwrap()
                .unwrap();
            assert_eq!(
                (
                    opened.content.as_ref(),
                    reader.probes.load(Ordering::Relaxed),
                    reader.reads.load(Ordering::Relaxed),
                ),
                ("later\n", probes, 1),
            );
        }
    }

    #[tokio::test]
    async fn include_callbacks_still_run_on_unambiguous_memo_and_once_reopens() {
        #[derive(Debug, Default)]
        struct CountingGate(AtomicUsize);

        #[async_trait]
        impl CompileGate for CountingGate {
            async fn inherit(&self, _: &str, _: &str) -> Result<bool> {
                Ok(true)
            }

            async fn include(&self, _: &str, _: &str) -> Result<bool> {
                self.0.fetch_add(1, Ordering::Relaxed);
                Ok(true)
            }
        }

        let root = TempLib::new("unambiguous-header-callbacks");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/"])
            .build()
            .unwrap();
        let reader = CountingReader::default();
        let gate = CountingGate::default();
        let mut walk = rooted(&config);
        let mut diagnostics = Diagnostics::default();
        let first = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                Some(&gate),
                &reader,
                &mut diagnostics,
            )
            .await
            .unwrap()
            .unwrap();
        walk.close();
        let second = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                Some(&gate),
                &reader,
                &mut diagnostics,
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(first.file_id, second.file_id);
        assert!(Arc::ptr_eq(&first.content, &second.content));
        walk.mark_once();
        walk.close();
        let third = walk
            .open(
                IncludeSource::System { path: "a.h" },
                None,
                &config,
                Some(&gate),
                &reader,
                &mut diagnostics,
            )
            .await
            .unwrap();
        assert!(third.is_none());
        assert_eq!(
            (
                gate.0.load(Ordering::Relaxed),
                reader.probes.load(Ordering::Relaxed),
                reader.reads.load(Ordering::Relaxed),
            ),
            (3, 0, 1),
        );
    }

    #[tokio::test]
    async fn directory_diagnostics_do_not_depend_on_the_readers_error_kind() {
        #[derive(Debug)]
        struct DirectoryReader;

        #[async_trait]
        impl SourceReader for DirectoryReader {
            async fn read(&self, _: &Path) -> std::io::Result<Vec<u8>> {
                Err(std::io::Error::from(ErrorKind::PermissionDenied))
            }

            async fn kind(&self, _: &Path) -> std::io::Result<SourceKind> {
                Ok(SourceKind::Directory)
            }
        }

        let root = TempLib::new("include-directory-read-error");
        let config = config_at(&root);
        let error = rooted(&config)
            .open(
                IncludeSource::Local { path: "directory" },
                None,
                &config,
                None,
                &DirectoryReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap_err();
        assert_eq!(
            error.message(),
            "attempt to include a directory: `/directory`"
        );
    }

    #[tokio::test]
    async fn system_directories_are_searched_before_the_local_fallback() {
        let parent = TempLib::new("include-search");
        let root = parent.join("lib");
        std::fs::create_dir_all(root.join("include")).unwrap();
        std::fs::write(root.join("pick.h"), "root decoy\n").unwrap();
        std::fs::write(root.join("include/pick.h"), "inside\n").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .system_include_dirs(vec!["/missing", "/include", "/"])
            .build()
            .unwrap();
        let mut walk = rooted(&config);
        let opened = walk
            .open(
                IncludeSource::System { path: "pick.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(&*opened.content, "inside\n");
        assert_eq!(
            Span::new(opened.file_id, 0..1).to_string(),
            "/include/pick.h:1:1"
        );
    }

    #[tokio::test]
    async fn a_configured_include_in_a_sibling_directory_is_rejected_without_disclosing_it() {
        let parent = TempLib::new("private-include-host");
        let root = parent.join("lib");
        std::fs::create_dir(&root).unwrap();
        std::fs::create_dir(parent.join("lib-other")).unwrap();
        let outside = parent.join("lib-other/auto.h");
        std::fs::write(&outside, "outside\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);
        let error = walk
            .open(
                IncludeSource::Configured(&LpcPath::new_server(outside)),
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "attempt to include a file outside the root: `<outside mudlib>`"
        );
        assert_eq!(walk.memo.len(), 1);
    }

    #[tokio::test]
    async fn a_reopened_file_reuses_its_id_and_text() {
        let root = TempLib::new("memo");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);

        let first = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .expect("not once-marked");
        walk.close();
        let second = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .expect("not once-marked");

        assert_eq!(first.file_id, second.file_id);
        assert!(Arc::ptr_eq(&first.content, &second.content));
    }

    #[tokio::test]
    async fn an_open_of_an_active_file_is_a_cycle() {
        let root = TempLib::new("cycle");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);

        walk.open(
            IncludeSource::Local { path: "a.h" },
            None,
            &config,
            None,
            &DiskSourceReader,
            &mut Diagnostics::default(),
        )
        .await
        .unwrap();
        let e = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap_err();

        let msg = e.to_string();
        assert!(msg.starts_with("cyclic `#include`"), "{msg}");
        assert!(msg.contains("a.h"), "{msg}");
    }

    #[tokio::test]
    async fn a_once_marked_file_opens_to_nothing() {
        let root = TempLib::new("once");
        std::fs::write(root.join("o.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);

        walk.open(
            IncludeSource::Local { path: "o.h" },
            None,
            &config,
            None,
            &DiskSourceReader,
            &mut Diagnostics::default(),
        )
        .await
        .unwrap();
        walk.mark_once();
        walk.close();

        let reopened = walk
            .open(
                IncludeSource::Local { path: "o.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap();
        assert!(reopened.is_none());
    }

    #[tokio::test]
    async fn once_precedes_the_cycle_check() {
        // Bound so the directory outlives this statement.
        let root = TempLib::new("once-cycle");
        let config = config_at(&root);
        let mut walk = rooted(&config);
        // Marks the root, which stays on the stack.
        walk.mark_once();

        let reopened = walk
            .open(
                IncludeSource::Local { path: "main.c" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap();
        assert!(reopened.is_none());
    }

    #[tokio::test]
    async fn the_depth_cap_fires_at_the_limit() {
        let root = TempLib::new("depth");
        for i in 0..MAX_INCLUDE_DEPTH + 5 {
            std::fs::write(root.join(format!("h{i}.h")), "1\n").unwrap();
        }
        let config = config_at(&root);
        let mut walk = rooted(&config);

        let mut opened = 0;
        let err = loop {
            let path = format!("h{opened}.h");
            match walk
                .open(
                    IncludeSource::Local { path: &path },
                    None,
                    &config,
                    None,
                    &DiskSourceReader,
                    &mut Diagnostics::default(),
                )
                .await
            {
                Ok(Some(_)) => opened += 1,
                Ok(None) => panic!("nothing is once-marked"),
                Err(e) => break e,
            }
        };

        // The root frame counts: 63 nested opens fill the cap.
        assert_eq!(opened, MAX_INCLUDE_DEPTH - 1);
        assert_eq!(err.to_string(), "`#include` nests too deeply");
    }

    #[tokio::test]
    async fn files_are_registered_under_their_in_game_names() {
        let root = TempLib::new("in-game-names");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);
        let opened = walk
            .open(
                IncludeSource::Local { path: "a.h" },
                None,
                &config,
                None,
                &DiskSourceReader,
                &mut Diagnostics::default(),
            )
            .await
            .unwrap()
            .unwrap();
        let root_id = walk.memo[walk.stack[0].path.server()].0;
        assert_eq!(Span::new(root_id, 0..3).to_string(), "/main.c:1:1");
        assert_eq!(Span::new(opened.file_id, 0..1).to_string(), "/a.h:1:1");

        // A root named by its server path, as `lpcc` does.
        let mut walk = IncludeWalk::default();
        let server_root = walk.open_root(
            &CompilerSource::new(LpcPath::new_server(root.join("main.c")), &config),
            "int x;\n",
        );
        assert_eq!(Span::new(server_root, 0..3).to_string(), "/main.c:1:1");
    }

    #[tokio::test]
    async fn open_close_balances_the_stack() {
        let root = TempLib::new("balance");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);
        assert_eq!(walk.stack.len(), 1, "open_root leaves the root frame");

        walk.open(
            IncludeSource::Local { path: "a.h" },
            None,
            &config,
            None,
            &DiskSourceReader,
            &mut Diagnostics::default(),
        )
        .await
        .unwrap();
        assert_eq!(walk.stack.len(), 2, "open pushes the included frame");

        walk.close();
        assert_eq!(walk.stack.len(), 1, "close pops back to the root frame");

        walk.close();
        assert!(
            walk.stack.is_empty(),
            "closing the root frame empties the stack"
        );
    }

    #[tokio::test]
    async fn active_include_frames_identify_their_sources_in_debug_output() {
        let root = TempLib::new("include-stack-debug");
        std::fs::write(root.join("active.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);
        assert!(format!("{:?}", walk.stack).contains("/main.c"));
        walk.open(
            IncludeSource::Local { path: "active.h" },
            None,
            &config,
            None,
            &DiskSourceReader,
            &mut Diagnostics::default(),
        )
        .await
        .unwrap()
        .unwrap();
        let active = format!("{:?}", walk.stack);
        assert!(active.contains("/main.c"), "{active}");
        assert!(active.contains("/active.h"), "{active}");
        walk.close();
        let closed = format!("{:?}", walk.stack);
        assert!(closed.contains("/main.c"), "{closed}");
        assert!(!closed.contains("/active.h"), "{closed}");
    }
}
