//! The include walk: the one owner of `#include` traversal for a compile.

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{
    LpcError, Result, lpc_error,
    source_map::{FileId, SOURCE_MAP},
    span::Span,
};
use lpc_rs_utils::{config::Config, read_lpc_file};
use tracing::instrument;

use crate::compiler::compile_gate::CompileGate;

/// Deepest `#include` nesting allowed, the root file included.
pub(super) const MAX_INCLUDE_DEPTH: usize = 64;

/// The pragma name that marks the current file include-once.
pub(super) const ONCE: &str = "once";

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
    /// The file's `SOURCE_MAP` id — one per file per compile.
    pub file_id: FileId,
    /// The file's text, shared with the memo.
    pub content: Arc<str>,
}

/// One file on the active include chain.
#[derive(Debug)]
struct Frame {
    /// Canonical server path — the memo/once/cycle key.
    canon: PathBuf,
    /// The file as resolved; nested resolution derives its cwd from this.
    path: LpcPath,
}

/// The one owner of `#include` traversal for a compile: resolution,
/// containment, IO, `SOURCE_MAP` registration, the active chain, the
/// depth cap, and the `#pragma once` set. The memo, once and cycle keys
/// are canonical server paths; the name registered for rendering is the
/// in-game path.
#[derive(Debug, Default)]
pub(super) struct IncludeWalk {
    /// The active chain, root first.
    stack: Vec<Frame>,
    /// One disk read and one `SOURCE_MAP` id per file per compile.
    memo: HashMap<PathBuf, (FileId, Arc<str>)>,
    /// Files marked `#pragma once`.
    once: HashSet<PathBuf>,
}

impl IncludeWalk {
    /// Register the root file's text and push its frame. Called once,
    /// first, by `scan`.
    pub fn open_root(&mut self, path: &LpcPath, code: &str, config: &Config) -> FileId {
        let canon = path.as_server(config.lib_dir.as_str()).into_owned();
        let file_id = SOURCE_MAP
            .write()
            .add(in_game_name(&canon, config), code.to_owned());
        self.memo.insert(canon.clone(), (file_id, Arc::from(code)));
        self.stack.push(Frame {
            canon,
            path: path.clone(),
        });
        file_id
    }

    /// Resolve and open one include, pushing its frame. `Ok(None)` is
    /// the `#pragma once` skip; the caller scans `Some` and then
    /// [`close`](Self::close)s. `gate` is asked for every directive but a
    /// configured one, before anything is read.
    #[instrument(skip(self, config, gate))]
    pub async fn open(
        &mut self,
        source: IncludeSource<'_>,
        span: Option<Span>,
        config: &Config,
        gate: Option<&dyn CompileGate>,
    ) -> Result<Option<Opened>> {
        let configured = matches!(source, IncludeSource::Configured(_));
        // Captured before `resolve` consumes `source`: an out-of-root
        // path collapses to an empty in-game form, so the directive's
        // own text is what the error can still name.
        let text = match &source {
            IncludeSource::System { path } | IncludeSource::Local { path } => (*path).to_string(),
            IncludeSource::Configured(path) => path.to_string(),
        };

        let lib_dir = config.lib_dir.as_str();
        let path = self.resolve(source, config).await;
        let canon = path.as_server(lib_dir).into_owned();

        if !path.is_within_root(lib_dir) {
            return Err(lpc_error!(
                span,
                "attempt to include a file outside the root: `{}`",
                text
            ));
        }

        if let (Some(gate), false) = (gate, configured) {
            let in_game = path.as_in_game(lib_dir).display().to_string();
            let from = self.current_in_game(config);
            if !gate.include(&in_game, &from).await? {
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

        if self.stack.iter().any(|frame| frame.canon == canon) {
            return Err(self.cycle_error(&path, span));
        }

        if self.stack.len() >= MAX_INCLUDE_DEPTH {
            return Err(lpc_error!(span, "`#include` nests too deeply"));
        }

        let (file_id, content) = match self.memo.get(&canon) {
            Some((file_id, content)) => (*file_id, content.clone()),
            None => {
                let is_dir = tokio::fs::metadata(&canon)
                    .await
                    .map(|m| m.is_dir())
                    .unwrap_or(false);
                if is_dir {
                    return Err(lpc_error!(
                        span,
                        "attempt to include a directory: `{}`",
                        path
                    ));
                }
                let text = match read_lpc_file(&canon).await {
                    Ok(text) => text,
                    Err(e) => {
                        return Err(lpc_error!(
                            span,
                            "unable to read include file `{}`: {}",
                            path,
                            e
                        ));
                    }
                };
                let file_id = SOURCE_MAP
                    .write()
                    .add(in_game_name(&canon, config), text.clone());
                let content: Arc<str> = Arc::from(text);
                self.memo.insert(canon.clone(), (file_id, content.clone()));
                (file_id, content)
            }
        };

        self.stack.push(Frame { canon, path });
        Ok(Some(Opened { file_id, content }))
    }

    /// Pop the active frame — the success and error paths alike.
    pub fn close(&mut self) {
        let popped = self.stack.pop();
        debug_assert!(popped.is_some(), "include close without an open");
    }

    /// Mark the active file `#pragma once` (idempotent).
    pub fn mark_once(&mut self) {
        let frame = self
            .stack
            .last()
            .expect("a pragma executes inside an open file");
        self.once.insert(frame.canon.clone());
    }

    /// Turn a directive's path into an [`LpcPath`], relative to the
    /// including file — the active frame.
    async fn resolve(&self, source: IncludeSource<'_>, config: &Config) -> LpcPath {
        match source {
            IncludeSource::Configured(path) => path.clone(),
            IncludeSource::Local { path } => {
                LpcPath::new_in_game(path, self.cwd(config), &*config.lib_dir)
            }
            IncludeSource::System { path } => {
                let mut found = None;
                for dir in &config.system_include_dirs {
                    let candidate = LpcPath::new_in_game(path, dir.as_str(), &*config.lib_dir);
                    let exists = tokio::fs::metadata(candidate.as_server(&*config.lib_dir))
                        .await
                        .is_ok();
                    if exists {
                        found = Some(candidate);
                        break;
                    }
                }
                found.unwrap_or_else(|| {
                    LpcPath::new_in_game(path, self.cwd(config), &*config.lib_dir)
                })
            }
        }
    }

    /// The including file's directory — the resolution cwd.
    fn cwd(&self, config: &Config) -> PathBuf {
        self.stack
            .last()
            .map(|frame| {
                frame
                    .path
                    .as_in_game(config.lib_dir.as_str())
                    .parent()
                    .unwrap_or_else(|| Path::new("/"))
                    .to_path_buf()
            })
            .unwrap_or_else(|| PathBuf::from("/"))
    }

    /// The including file — the active frame — as an in-game path.
    fn current_in_game(&self, config: &Config) -> String {
        self.stack
            .last()
            .map(|frame| {
                frame
                    .path
                    .as_in_game(config.lib_dir.as_str())
                    .display()
                    .to_string()
            })
            .unwrap_or_else(|| "/".to_string())
    }

    /// The cycle diagnostic; `scan_include` adds the chain labels.
    fn cycle_error(&self, path: &LpcPath, span: Option<Span>) -> LpcError {
        lpc_error!(
            span,
            "cyclic `#include`: `{}` is already being included",
            path
        )
    }
}

/// The name a file is registered under for rendering: its in-game path.
fn in_game_name(canon: &Path, config: &Config) -> String {
    LpcPath::new_server(canon)
        .as_in_game(config.lib_dir.as_str())
        .display()
        .to_string()
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
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
            &LpcPath::new_in_game("/main.c", "/", &*config.lib_dir),
            "int x;\n",
            config,
        );
        walk
    }

    #[tokio::test]
    async fn a_reopened_file_reuses_its_id_and_text() {
        let root = TempLib::new("memo");
        std::fs::write(root.join("a.h"), "1\n").unwrap();
        let config = config_at(&root);
        let mut walk = rooted(&config);

        let first = walk
            .open(IncludeSource::Local { path: "a.h" }, None, &config, None)
            .await
            .unwrap()
            .expect("not once-marked");
        walk.close();
        let second = walk
            .open(IncludeSource::Local { path: "a.h" }, None, &config, None)
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

        walk.open(IncludeSource::Local { path: "a.h" }, None, &config, None)
            .await
            .unwrap();
        let e = walk
            .open(IncludeSource::Local { path: "a.h" }, None, &config, None)
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

        walk.open(IncludeSource::Local { path: "o.h" }, None, &config, None)
            .await
            .unwrap();
        walk.mark_once();
        walk.close();

        let reopened = walk
            .open(IncludeSource::Local { path: "o.h" }, None, &config, None)
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
            .open(IncludeSource::Local { path: "main.c" }, None, &config, None)
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
                .open(IncludeSource::Local { path: &path }, None, &config, None)
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
            .open(IncludeSource::Local { path: "a.h" }, None, &config, None)
            .await
            .unwrap()
            .unwrap();
        let root_id = walk.memo[&walk.stack[0].canon].0;
        assert_eq!(Span::new(root_id, 0..3).to_string(), "/main.c:1:1");
        assert_eq!(Span::new(opened.file_id, 0..1).to_string(), "/a.h:1:1");

        // A root named by its server path, as `lpcc` does.
        let mut walk = IncludeWalk::default();
        let server_root = walk.open_root(
            &LpcPath::new_server(root.join("main.c")),
            "int x;\n",
            &config,
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

        walk.open(IncludeSource::Local { path: "a.h" }, None, &config, None)
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
}
