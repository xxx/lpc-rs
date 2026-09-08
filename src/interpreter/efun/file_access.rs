//! The shared front of the file efuns: the path argument canonicalized,
//! confined to the lib, and put to the master.

use std::{path::Path, sync::Arc};

pub(crate) use lpc_rs_core::lpc_path::ResolvedPath as FileAccess;
use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::{
    apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::Effect,
};

/// The path in argument `i`, canonicalized against the caller's directory
/// and allowed by the master's `apply` (`valid_read`/`valid_write`) for
/// `efun`. A non-string, a path that leaves the lib (the master is not
/// asked), or a refusal is a runtime error.
pub(crate) async fn authorize<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    apply: &str,
    i: usize,
) -> Result<FileAccess> {
    authorize_or_deny(context, efun, apply, i)
        .await?
        .ok_or_else(|| context.runtime_error(format!("{efun}: permission denied")))
}

/// [`authorize`] with the master's refusal as `None`, for an efun that
/// answers a refusal the way it answers a missing file.
pub(crate) async fn authorize_or_deny<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    apply: &str,
    i: usize,
) -> Result<Option<FileAccess>> {
    let Some(arg) = context.arg(i).as_str() else {
        return Err(context.runtime_error(format!("{efun}: path must be a string")));
    };
    let path = context
        .config()
        .paths()
        .resolve(arg, context.in_game_cwd())
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?;
    let allowed = master_allows(context, efun, apply, path.name().as_str()).await?;
    Ok(allowed.then_some(path))
}

/// Puts `in_game` to the master's `apply` (`valid_read`/`valid_write`) with
/// `efun`'s name, the calling process, and its program; answers whether it
/// allowed the access.
async fn master_allows<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    apply: &str,
    in_game: &str,
) -> Result<bool> {
    let args = [
        LpcRef::from(in_game),
        LpcRef::from(efun),
        LpcRef::from(Arc::downgrade(context.process())),
        context.calling_program(),
    ];
    valid_apply(context.task_context(), Some(context.chain()), apply, &args).await
}

/// Authorize the root-relative, unsuffixed save name, then append `.o`.
pub(crate) async fn authorize_save<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    apply: &str,
    i: usize,
) -> Result<FileAccess> {
    let Some(arg) = context.arg(i).as_str() else {
        return Err(context.runtime_error(format!("{efun}: path must be a string")));
    };
    let path = context
        .config()
        .paths()
        .resolve(arg, "/")
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?;
    if !master_allows(context, efun, apply, path.name().as_str()).await? {
        return Err(context.runtime_error(format!("{efun}: permission denied")));
    }
    Ok(path.save_file())
}

/// Whether `server`'s parent is a directory on disk or one this task's
/// `mkdir` will create at commit; a missing parent is `false`, any other
/// failure the error.
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

/// The write tail shared by `save_object` and `save_map`: checks `access`'s
/// parent directory exists, then records `contents` as a whole-file write to
/// it, delivered once the attempt commits.
pub(crate) async fn record_save<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    access: FileAccess,
    contents: String,
) -> Result<()> {
    let io_error =
        |e: std::io::Error| context.runtime_error(format!("{efun}: {}: {e}", access.name()));
    if !parent_is_dir(context, access.server())
        .await
        .map_err(io_error)?
    {
        return Err(context.runtime_error(format!(
            "{efun}: {}: parent directory does not exist",
            access.name()
        )));
    }
    context.record_effect(Effect::WriteFile {
        path: access,
        contents,
    });
    Ok(())
}

/// A corrupt save-file line as a runtime error: `<efun>: <in_game> line
/// <line>: <e>`, `e`'s own `runtime error: ` prefix stripped.
pub(crate) fn line_error<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    in_game: &str,
    line: usize,
    e: LpcError,
) -> LpcError {
    context.runtime_error(format!(
        "{efun}: {in_game} line {line}: {}",
        e.to_string().trim_start_matches("runtime error: ")
    ))
}
