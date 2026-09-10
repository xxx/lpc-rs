//! Apply diagnostics use the server log independently of mudlib error handling.

use lpc_rs_errors::LpcError;

use crate::interpreter::{
    CONNECT, LOGON, QUERY_ALLOW_SHADOW, VALID_EXEC, VALID_INHERIT, VALID_LOAD, VALID_READ,
    VALID_SHUTDOWN, VALID_WRITE, apply::in_game_location, lpc_ref::LpcRef, process::Process,
};

/// Missing security and login applies prevent the requested operation.
pub(crate) fn missing(name: &str, process: Option<&Process>) {
    if matches!(
        name,
        CONNECT
            | LOGON
            | QUERY_ALLOW_SHADOW
            | VALID_EXEC
            | VALID_INHERIT
            | VALID_LOAD
            | VALID_READ
            | VALID_SHUTDOWN
            | VALID_WRITE
    ) {
        tracing::warn!(
            target: "lpc_rs::applies",
            apply = name,
            object = %process.map_or_else(|| "<no master>".into(), Process::filename),
            "Missing apply; operation refused"
        );
    } else {
        tracing::debug!(
            target: "lpc_rs::applies",
            apply = name,
            object = %process.map_or_else(|| "<no master>".into(), Process::filename),
            "Missing optional apply; using driver fallback"
        );
    }
}

/// Argument values are omitted because input callbacks can receive passwords.
pub(crate) fn started<'a>(
    name: &str,
    process: &Process,
    argument_types: impl Iterator<Item = &'a str>,
) {
    tracing::debug!(
        target: "lpc_rs::applies",
        apply = name,
        object = %process.filename(),
        argument_types = ?argument_types.collect::<Vec<_>>(),
        "Applying function"
    );
}

/// Report the resolved receiver and outcome, including failures caught by LPC.
pub(crate) fn finished(name: &str, process: &Process, result: Result<Option<&LpcRef>, &LpcError>) {
    match result {
        Err(error) => tracing::error!(
            target: "lpc_rs::applies",
            apply = name,
            object = %process.filename(),
            location = %in_game_location(error.span()),
            error = %error,
            "Apply failed"
        ),
        Ok(None) => missing(name, Some(process)),
        Ok(Some(value)) => tracing::debug!(
            target: "lpc_rs::applies",
            apply = name,
            object = %process.filename(),
            result_type = value.type_name(),
            integer = match value { LpcRef::Int(value) => Some(value.0), _ => None },
            "Apply returned"
        ),
    }
}
