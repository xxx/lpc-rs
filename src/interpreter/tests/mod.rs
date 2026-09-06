//! End-to-end tests that load LPC into a VM and read `create()`'s result.

mod array_ops;
mod call_other;
mod calling;
mod catch_result;
mod entry_args;
mod loading;
mod parse_command;
mod parse_string;
mod parser;
mod pointers;
mod previous_object;
mod references;
mod security;
mod simul_efuns;
mod spreading;
mod virtual_objects;

use lpc_rs_utils::config::Config;
use lpc_rs_utils::lpc_string::LpcString;

use crate::{
    interpreter::{lpc_ref::LpcRef, stm::TxnHandle, vm::Vm},
    test_support::test_config,
};

pub(crate) fn s(text: &str) -> LpcRef {
    LpcString::from(text).into()
}

/// Loads `master` as the master and each of `objects` at its path, then
/// `/main.c` from `main`, whose `create()` returns an array; the array's
/// members.
pub(crate) async fn run(master: &str, objects: &[(&str, &str)], main: &str) -> Vec<LpcRef> {
    run_with(test_config(), master, objects, main).await
}

/// [`run`] under `config`.
pub(crate) async fn run_with(
    config: Config,
    master: &str,
    objects: &[(&str, &str)],
    main: &str,
) -> Vec<LpcRef> {
    let (result, txn) = run_result(config, master, objects, main).await;
    result
        .with_array(&txn, |a| a.iter().cloned().collect())
        .unwrap()
}

/// [`run`], but returning the raw result array with its transaction so a
/// member that is itself an array can be resolved with a further
/// [`LpcRef::with_array`] call.
pub(crate) async fn run_nested(
    master: &str,
    objects: &[(&str, &str)],
    main: &str,
) -> (LpcRef, TxnHandle) {
    run_result(test_config(), master, objects, main).await
}

/// Loads `master`, `objects` and `/main.c` as in [`run`], and returns
/// `create()`'s raw result with the transaction it was read through.
async fn run_result(
    config: Config,
    master: &str,
    objects: &[(&str, &str)],
    main: &str,
) -> (LpcRef, TxnHandle) {
    let vm = Vm::new(config);
    vm.initialize_process_from_code("/secure/master.c", master)
        .await
        .unwrap();
    for (path, code) in objects {
        vm.initialize_process_from_code(path, code).await.unwrap();
    }
    let proc = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));
    let result = proc.result().expect("create() returns an array");
    let txn = proc.context.txn().clone();
    (result, txn)
}

/// The runtime error `/main.c`'s `create()` raises.
pub(crate) async fn fails(master: &str, objects: &[(&str, &str)], main: &str) -> String {
    fails_with(test_config(), master, objects, main).await
}

/// [`fails`] under `config`.
pub(crate) async fn fails_with(
    config: Config,
    master: &str,
    objects: &[(&str, &str)],
    main: &str,
) -> String {
    let vm = Vm::new(config);
    vm.initialize_process_from_code("/secure/master.c", master)
        .await
        .unwrap();
    for (path, code) in objects {
        vm.initialize_process_from_code(path, code).await.unwrap();
    }
    vm.initialize_process_from_code("/main.c", main)
        .await
        .unwrap_err()
        .to_string()
}
