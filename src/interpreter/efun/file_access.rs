//! The shared front of the file efuns: the path argument canonicalized,
//! confined to the lib, and put to the master.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;

use crate::interpreter::{apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// A file path the master has allowed an efun to touch.
pub(crate) struct FileAccess {
    /// The canonical absolute in-game path: what the master saw, what
    /// messages name.
    pub in_game: String,
    /// The file on the server.
    pub server: PathBuf,
}

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
    let path = context.in_game_path(arg);
    let server = context
        .config()
        .validate_in_game_path(&path, None)
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?
        .into_owned();
    let in_game = path
        .as_in_game(context.config().lib_dir.as_str())
        .display()
        .to_string();
    let args = [
        LpcRef::from(in_game.clone()),
        LpcRef::from(efun),
        LpcRef::from(Arc::downgrade(context.process())),
        context.calling_program(),
    ];
    let allowed = valid_apply(context.task_context(), Some(context.chain()), apply, &args).await?;
    Ok(allowed.then_some(FileAccess { in_game, server }))
}

/// [`authorize`] for a save efun: argument `i` is resolved against the lib
/// root, the master sees it without a suffix, then `.o` is appended to both
/// paths and confinement is checked again.
pub(crate) async fn authorize_save<const N: usize>(
    context: &EfunContext<'_, N>,
    efun: &str,
    apply: &str,
    i: usize,
) -> Result<FileAccess> {
    let Some(arg) = context.arg(i).as_str() else {
        return Err(context.runtime_error(format!("{efun}: path must be a string")));
    };
    let path = LpcPath::new_in_game(arg, "/", &*context.config().lib_dir);
    let in_game = path
        .as_in_game(context.config().lib_dir.as_str())
        .display()
        .to_string();
    context
        .config()
        .validate_in_game_path(&path, None)
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?;
    let args = [
        LpcRef::from(in_game.clone()),
        LpcRef::from(efun),
        LpcRef::from(Arc::downgrade(context.process())),
        context.calling_program(),
    ];
    let allowed = valid_apply(context.task_context(), Some(context.chain()), apply, &args).await?;
    if !allowed {
        return Err(context.runtime_error(format!("{efun}: permission denied")));
    }
    let suffixed = LpcPath::new_in_game(format!("{in_game}.o"), "/", &*context.config().lib_dir);
    let server = context
        .config()
        .validate_in_game_path(&suffixed, None)
        .map_err(|_| context.runtime_error(format!("{efun}: `{arg}` is not a valid path")))?
        .into_owned();
    Ok(FileAccess {
        in_game: format!("{in_game}.o"),
        server,
    })
}

/// Whether `server`'s parent exists and is a directory; a missing parent
/// is `false`, any other failure the error.
pub(crate) async fn parent_is_dir(server: &Path) -> std::io::Result<bool> {
    let Some(parent) = server.parent() else {
        return Ok(false);
    };
    match tokio::fs::metadata(parent).await {
        Ok(m) => Ok(m.is_dir()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(e),
    }
}
