//! End-to-end tests that load LPC into a VM and read `create()`'s result.

mod entry_args;
mod parse_command;
mod parse_string;
mod references;

use crate::{
    interpreter::{lpc_ref::LpcRef, lpc_string::LpcString, vm::Vm},
    test_support::test_config,
};

pub(crate) fn s(text: &str) -> LpcRef {
    LpcString::from(text).into()
}

/// Loads `master` as the master and each of `objects` at its path, then
/// `/main.c` from `main`, whose `create()` returns an array; the array's
/// members.
pub(crate) async fn run(master: &str, objects: &[(&str, &str)], main: &str) -> Vec<LpcRef> {
    let vm = Vm::new(test_config());
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
    result
        .with_array(proc.context.txn(), |a| a.iter().cloned().collect())
        .unwrap()
}

/// The runtime error `/main.c`'s `create()` raises.
pub(crate) async fn fails(master: &str, objects: &[(&str, &str)], main: &str) -> String {
    let vm = Vm::new(test_config());
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
