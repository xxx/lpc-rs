use std::sync::Arc;

use lpc_rs_utils::config::Config;

use crate::interpreter::{object_space::ObjectSpace, process::Process};

pub fn get_simul_efuns(config: &Config, object_space: &ObjectSpace) -> Option<Arc<Process>> {
    config.simul_efun_file.as_deref().and_then(|f| {
        let file = f.strip_suffix(".c").unwrap_or(f);
        object_space.lookup(file).map(|p| p.clone())
    })
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    #[test]
    fn test_get_simul_efuns() {
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
        let space_cell: Arc<ObjectSpace> = object_space.into();
        ObjectSpace::insert_process(&space_cell, proc);

        let simul_efuns = get_simul_efuns(&config, &space_cell).unwrap();
        assert_eq!(
            simul_efuns.program.filename.to_string(),
            "/secure/simul_efuns"
        );
    }
}
