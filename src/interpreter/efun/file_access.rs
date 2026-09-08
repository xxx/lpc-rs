//! The shared front of the file efuns: authorization, named IO errors,
//! and access to the calling task's pending file view.

use std::{fmt, path::Path, sync::Arc};

use lpc_rs_core::lpc_path::ResolvedPath;
use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::{
    apply::valid_apply,
    efun::{
        efun_context::EfunContext,
        file_view::{self, Seen},
    },
    lpc_ref::LpcRef,
    stm::Effect,
};

/// One authorized efun access, with its path and diagnostic operation kept together.
#[derive(Debug)]
pub(crate) struct FileAccess {
    path: ResolvedPath,
    efun: &'static str,
}

impl FileAccess {
    /// The resolved path for operation-specific filesystem checks.
    pub(crate) fn path(&self) -> &ResolvedPath {
        &self.path
    }

    /// Transfer the materialized path into a pending effect.
    pub(crate) fn into_path(self) -> ResolvedPath {
        self.path
    }

    /// An error naming the efun and its safe in-game path.
    pub(crate) fn error<const N: usize>(
        &self,
        context: &EfunContext<'_, N>,
        error: impl fmt::Display,
    ) -> LpcError {
        context.runtime_error(format!("{}: {}: {error}", self.efun, self.path))
    }

    /// The path's contents after this task's earlier file effects.
    pub(crate) async fn read<const N: usize>(&self, context: &EfunContext<'_, N>) -> Result<Seen> {
        file_view::read_through(context, self.path.server())
            .await
            .map_err(|e| self.error(context, e))
    }

    /// The pending file view as bytes; missing files and directories are errors.
    pub(crate) async fn read_bytes<const N: usize>(
        &self,
        context: &EfunContext<'_, N>,
    ) -> Result<Vec<u8>> {
        self.read(context)
            .await?
            .into_bytes()
            .map_err(|e| self.error(context, e))
    }

    /// Save text is UTF-8 or Latin-1; an unreadable path is treated as absent.
    pub(crate) async fn read_save<const N: usize>(
        &self,
        context: &EfunContext<'_, N>,
    ) -> Option<String> {
        let Ok(Seen::File(bytes)) = file_view::read_through(context, self.path.server()).await
        else {
            return None;
        };
        Some(match String::from_utf8(bytes) {
            Ok(text) => text,
            Err(e) => e.into_bytes().iter().map(|&b| b as char).collect(),
        })
    }

    /// Require a parent directory on disk or in this task's pending directory creations.
    pub(crate) async fn require_parent<const N: usize>(
        &self,
        context: &EfunContext<'_, N>,
    ) -> Result<()> {
        if parent_is_dir(context, self.path.server())
            .await
            .map_err(|e| self.error(context, e))?
        {
            Ok(())
        } else {
            Err(self.error(context, "parent directory does not exist"))
        }
    }

    /// Record a whole save-file write after checking its parent; IO waits for commit.
    pub(crate) async fn record_save<const N: usize>(
        self,
        context: &EfunContext<'_, N>,
        contents: String,
    ) -> Result<()> {
        self.require_parent(context).await?;
        context.record_effect(Effect::WriteFile {
            path: self.into_path(),
            contents,
        });
        Ok(())
    }

    /// A corrupt save-file line, with the codec's runtime-error prefix removed.
    pub(crate) fn line_error<const N: usize>(
        &self,
        context: &EfunContext<'_, N>,
        line: usize,
        error: LpcError,
    ) -> LpcError {
        context.runtime_error(format!(
            "{}: {} line {line}: {}",
            self.efun,
            self.path,
            error.to_string().trim_start_matches("runtime error: ")
        ))
    }
}

impl fmt::Display for FileAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

/// Authorize an ordinary path relative to the caller's directory; refusal is an error.
pub(crate) async fn authorize<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &'static str,
    apply: &str,
    i: usize,
) -> Result<FileAccess> {
    authorize_or_deny(context, efun, apply, i)
        .await?
        .ok_or_else(|| context.runtime_error(format!("{efun}: permission denied")))
}

/// Ordinary authorization with refusal as `None`, for efuns that treat it as a missing file.
pub(crate) async fn authorize_or_deny<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &'static str,
    apply: &str,
    i: usize,
) -> Result<Option<FileAccess>> {
    authorize_at(context, efun, apply, i, &context.in_game_cwd()).await
}

/// Authorize the root-relative, unsuffixed save name, then append `.o`.
pub(crate) async fn authorize_save<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &'static str,
    apply: &str,
    i: usize,
) -> Result<FileAccess> {
    let mut access = authorize_at(context, efun, apply, i, Path::new("/"))
        .await?
        .ok_or_else(|| context.runtime_error(format!("{efun}: permission denied")))?;
    access.path = access.path.save_file();
    Ok(access)
}

async fn authorize_at<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &'static str,
    apply: &str,
    i: usize,
    cwd: &Path,
) -> Result<Option<FileAccess>> {
    let Some(arg) = context.arg(i).as_str() else {
        return Err(context.runtime_error(format!("{efun}: path must be a string")));
    };
    let path = context
        .config()
        .paths()
        .resolve(arg, cwd)
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?;
    let args = [
        LpcRef::from(path.name().as_str()),
        LpcRef::from(efun),
        LpcRef::from(Arc::downgrade(context.process())),
        context.calling_program(),
    ];
    let allowed = valid_apply(context.task_context(), Some(context.chain()), apply, &args).await?;
    Ok(allowed.then_some(FileAccess { path, efun }))
}

/// Whether `server`'s parent is a directory on disk or in this task's pending creations.
pub(crate) async fn parent_is_dir<const N: usize>(
    context: &EfunContext<'_, N>,
    server: &Path,
) -> std::io::Result<bool> {
    let Some(parent) = server.parent() else {
        return Ok(false);
    };
    if context.has_pending_dir(parent) {
        return Ok(true);
    }
    match tokio::fs::metadata(parent).await {
        Ok(m) => Ok(m.is_dir()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        test_support::{TempLib, string_global, temp_lib_config},
    };

    const ALLOWING: &str = indoc! { r#"
        string calls = "";
        int valid_read(string p, string e, object c, string g) {
            calls += e + ":" + p + "\n";
            return 1;
        }
        int valid_write(string p, string e, object c, string g) {
            calls += e + ":" + p + "\n";
            return 1;
        }
    "# };

    #[tokio::test]
    async fn file_efuns_keep_their_argument_and_authorization_order() {
        let root = TempLib::new("access-order");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap()
            .context
            .process;
        let cases = [
            (r#"read_file("missing", -1)"#, "read_file: negative start"),
            (
                r#"read_bytes("missing", 0, -1)"#,
                "read_bytes: negative length -1",
            ),
            (
                r#"read_chars("missing", 0, -1)"#,
                "read_chars: negative length -1",
            ),
            (
                r#"write_bytes("missing", 0, contents)"#,
                "write_bytes: string is not bytes; convert with to_bytes()",
            ),
            (
                r#"save_map(([ 1: 2 ]), "missing")"#,
                "save_map: a key must be a string, not int",
            ),
        ];

        for (i, (call, expected)) in cases.into_iter().enumerate() {
            let code = format!(
                "string err; void create() {{ mixed contents = \"x\"; err = catch({call}); }}"
            );
            let caller = vm
                .initialize_process_from_code(format!("/room/check{i}.c"), code)
                .await
                .unwrap()
                .context
                .process;
            assert_eq!(
                string_global(&vm, &caller, "err"),
                format!("runtime error: {expected}")
            );
        }
        assert_eq!(
            string_global(&vm, &master, "calls"),
            "read_file:/room/missing\n"
        );
    }

    #[tokio::test]
    async fn common_io_errors_keep_their_wording_and_virtual_paths() {
        let root = TempLib::new("private-access-errors");
        std::fs::create_dir_all(root.join("room/directory")).unwrap();
        std::fs::write(root.join("room/invalid"), [0xff]).unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        let cases = [
            (
                r#"read_file("missing")"#,
                "read_file: /room/missing: No such file or directory (os error 2)",
            ),
            (
                r#"read_bytes("missing")"#,
                "read_bytes: /room/missing: No such file or directory (os error 2)",
            ),
            (
                r#"read_chars("missing")"#,
                "read_chars: /room/missing: No such file or directory (os error 2)",
            ),
            (
                r#"read_file("directory")"#,
                "read_file: /room/directory: Is a directory (os error 21)",
            ),
            (
                r#"read_file("invalid")"#,
                "read_file: /room/invalid: stream did not contain valid UTF-8",
            ),
            (
                r#"read_chars("invalid")"#,
                "read_chars: /room/invalid is not UTF-8",
            ),
            (
                r#"write_file("absent/new", "x")"#,
                "write_file: /room/absent/new: parent directory does not exist",
            ),
            (
                r#"mkdir("absent/new")"#,
                "mkdir: /room/absent/new: parent directory does not exist",
            ),
            (
                r#"save_map(([ "x": 1 ]), "absent/new")"#,
                "save_map: /absent/new.o: parent directory does not exist",
            ),
        ];

        for (i, (call, expected)) in cases.into_iter().enumerate() {
            let code = format!("string err; void create() {{ err = catch({call}); }}");
            let caller = vm
                .initialize_process_from_code(format!("/room/error{i}.c"), code)
                .await
                .unwrap()
                .context
                .process;
            let error = string_global(&vm, &caller, "err");
            assert_eq!(error, format!("runtime error: {expected}"));
            assert!(!error.contains(root.to_str().unwrap()));
        }
    }
}
