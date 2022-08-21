use std::{cell::RefCell, rc::Rc};

use lpc_rs_utils::config::Config;

use crate::interpreter::{object_space::ObjectSpace, process::Process};

pub fn get_simul_efuns(
    config: &Config,
    object_space: &ObjectSpace,
) -> Option<Rc<RefCell<Process>>> {
    config.simul_efun_file().and_then(|f| {
        let file = f.strip_suffix(".c").unwrap_or(f);
        object_space.lookup(file).cloned()
    })
}

#[cfg(test)]
mod tests {
    use claim::assert_none;
    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;
    use crate::interpreter::program::Program;

    #[test]
    fn test_get_simul_efuns() {
        let config = Config::default();
        let mut object_space = ObjectSpace::default();
        let simul_efuns = get_simul_efuns(&config, &object_space);
        assert_none!(simul_efuns);

        let config = Config::default().with_simul_efun_file(Some("/secure/simul_efuns"));
        let mut prog = Program::default();
        prog.filename = LpcPath::new_in_game("/secure/simul_efuns", "/", config.lib_dir());
        let proc = Process::new(prog);
        object_space.insert_process(proc);

        let simul_efuns = get_simul_efuns(&config, &object_space).unwrap();
        let borrowed = simul_efuns.borrow();
        assert_eq!(borrowed.filename.to_string(), "/secure/simul_efuns");
    }
}
