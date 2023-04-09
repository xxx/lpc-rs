pub mod keyable;

use std::{fmt::Formatter};
use std::sync::Arc;

use lpc_rs_utils::config::Config;
use qcell::QCell;

use crate::interpreter::{object_space::ObjectSpace, process::Process};

pub fn get_simul_efuns(config: &Config, object_space: &ObjectSpace) -> Option<Arc<QCell<Process>>> {
    config.simul_efun_file.as_deref().and_then(|f| {
        let file = f.strip_suffix(".c").unwrap_or(f);
        object_space.lookup(file).cloned()
    })
}

/// A shared target to call during debug for [`QCell`]-contained data
pub fn qcell_debug<T>(_cell: T, f: &mut Formatter) -> std::fmt::Result {
    f.write_str("<QCell'ed Data>")
}

pub fn qcell_process_debug<T>(_cell: T, f: &mut Formatter) -> std::fmt::Result {
    write!(f, "<QCell'ed Process>")
}

pub fn qcell_process_option_debug<T>(cell: &Option<T>, f: &mut Formatter) -> std::fmt::Result {
    if let Some(cell) = cell {
        f.write_str("Some(")?;
        qcell_process_debug(cell, f)?;
        f.write_str(")")
    } else {
        write!(f, "None")
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;
    use qcell::QCellOwner;

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    #[test]
    fn test_get_simul_efuns() {
        let mut cell_key = QCellOwner::new();
        let config = Config::default();
        let object_space = ObjectSpace::default();
        let simul_efuns = get_simul_efuns(&config, &object_space);
        if simul_efuns.is_some() {
            panic!("Expected None, got something");
        }

        let config = ConfigBuilder::default()
            .simul_efun_file("/secure/simul_efuns")
            .build()
            .unwrap();
        let prog = ProgramBuilder::default()
            .filename(LpcPath::new_in_game(
                "/secure/simul_efuns",
                "/",
                &*config.lib_dir,
            ))
            .build()
            .unwrap();
        let proc = Process::new(prog);
        let space_cell: Arc<QCell<ObjectSpace>> = cell_key.cell(object_space).into();
        ObjectSpace::insert_process(&space_cell, cell_key.cell(proc), &mut cell_key);

        let object_space = space_cell.ro(&cell_key);
        let simul_efuns = get_simul_efuns(&config, object_space).unwrap();
        let borrowed = simul_efuns.ro(&cell_key);
        assert_eq!(
            borrowed.as_ref().filename.to_string(),
            "/secure/simul_efuns"
        );
    }
}
