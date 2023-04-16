use std::{borrow::Cow, collections::HashMap, path::Path};

use derive_builder::Builder;
use fs_err as fs;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{span::Span, LpcError, Result};
use tracing::{info, warn};
use ustr::{ustr, Ustr};

const DEFAULT_MAX_INHERIT_DEPTH: usize = 10;

/// The main struct that handles runtime use configurations.
#[derive(Debug, Builder)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
#[readonly::make]
pub struct Config {
    #[builder(setter(into, strip_option), default = "None")]
    pub auto_include_file: Option<Ustr>,

    #[builder(setter(into, strip_option), default = "None")]
    pub auto_inherit_file: Option<Ustr>,

    #[builder(setter(into), default = "ustr(\"0.0.0.0\")")]
    pub bind_address: Ustr,

    #[builder(setter(into, strip_option), default = "Some(ustr(\"STDOUT\"))")]
    pub log_file: Option<Ustr>,

    #[builder(setter(strip_option), default = "None")]
    pub log_level: Option<tracing::Level>,

    #[builder(setter(custom), default = "ustr(\"\")")]
    pub lib_dir: Ustr,

    #[builder(default = "ustr(\"/secure/master.c\")")]
    pub master_object: Ustr,

    #[builder(default = "DEFAULT_MAX_INHERIT_DEPTH")]
    pub max_inherit_depth: usize,

    #[builder(setter(into, strip_option), default = "Some(100000)")]
    pub max_task_instructions: Option<usize>,

    #[builder(setter(into, strip_option), default = "None")]
    pub simul_efun_file: Option<Ustr>,

    #[builder(setter(custom), default = "vec![ustr(\"/sys\")]")]
    pub system_include_dirs: Vec<Ustr>,

    #[builder(default = "2496")]
    pub port: u16,
}

impl ConfigBuilder {
    /// Set config values from a `dotenv` file. If `env_path` is `None`, the default `.env` is used.
    pub fn load_env<P>(&mut self, env_path: Option<P>) -> &mut Self
    where
        P: AsRef<Path>,
    {
        let _ = match env_path {
            Some(p) => {
                dotenvy::from_filename(p).map_err(|e| info!(".env not loaded: {}", e.to_string()))
            }
            None => dotenvy::dotenv().map_err(|e| info!(".env not loaded: {}", e.to_string())),
        };

        let env = std::env::vars()
            .map(|(k, v)| {
                let key = k.to_uppercase();

                (key, v)
            })
            .collect::<HashMap<_, _>>();

        let s = Self {
            auto_include_file: env
                .get("LPC_AUTO_INCLUDE_FILE")
                .or_else(|| env.get("AUTO_INCLUDE_FILE"))
                .map(|x| Some(ustr(x)))
                .or(self.auto_include_file),
            auto_inherit_file: env
                .get("LPC_AUTO_INHERIT_FILE")
                .or_else(|| env.get("AUTO_INHERIT_FILE"))
                .map(|x| Some(ustr(x)))
                .or(self.auto_inherit_file),
            bind_address: env
                .get("LPC_BIND_ADDRESS")
                .or_else(|| env.get("BIND_ADDRESS"))
                .map(|x| ustr(x))
                .or(self.bind_address),
            log_file: env
                .get("LPC_LOG_FILE")
                .or_else(|| env.get("LOG_FILE"))
                .map(|x| Some(ustr(x)))
                .or(self.log_file),
            log_level: env
                .get("LPC_LOG_LEVEL")
                .or_else(|| env.get("LOG_LEVEL"))
                .map(|x| Some(x.parse::<tracing::Level>().unwrap()))
                .or(self.log_level),
            lib_dir: env
                .get("LPC_LIB_DIR")
                .or_else(|| env.get("LIB_DIR"))
                .and_then(|x| canonicalized_path(x).ok())
                .or(self.lib_dir),
            master_object: env
                .get("LPC_MASTER_OBJECT")
                .or_else(|| env.get("MASTER_OBJECT"))
                .map(|x| ustr(x))
                .or(self.master_object),
            max_inherit_depth: env
                .get("LPC_MAX_INHERIT_DEPTH")
                .or_else(|| env.get("MAX_INHERIT_DEPTH"))
                .map(|x| x.parse::<usize>().unwrap())
                .or(self.max_inherit_depth),
            max_task_instructions: env
                .get("LPC_MAX_TASK_INSTRUCTIONS")
                .or_else(|| env.get("MAX_TASK_INSTRUCTIONS"))
                .map(|x| Some(x.parse::<usize>().unwrap()))
                .or(self.max_task_instructions),
            port: env
                .get("LPC_PORT")
                .or_else(|| env.get("PORT"))
                .map(|x| x.parse::<u16>().unwrap())
                .or(self.port),
            simul_efun_file: env
                .get("LPC_SIMUL_EFUN_FILE")
                .or_else(|| env.get("SIMUL_EFUN_FILE"))
                .map(|x| Some(ustr(x)))
                .or(self.simul_efun_file),
            system_include_dirs: env
                .get("LPC_SYSTEM_INCLUDE_DIRS")
                .or_else(|| env.get("SYSTEM_INCLUDE_DIRS"))
                .map(|x| x.split(':').map(|x| x.into()).collect::<Vec<_>>())
                .or_else(|| self.system_include_dirs.clone()),
        };

        let new = self;

        *new = s;

        new
    }

    pub fn lib_dir<S>(&mut self, lib_dir: S) -> &mut Self
    where
        S: Into<String>,
    {
        let mut new = self;
        let dir = match canonicalized_path(lib_dir.into()) {
            Ok(x) => x,
            Err(e) => {
                let path = canonicalized_path(".").unwrap();
                warn!("Unable to get canonical path for `lib_dir`: {e}. Using `{path}` instead.");
                path
            }
        };

        new.lib_dir = Some(ustr(&dir));

        new
    }

    /// Set the system include directories. These are in-game directories.
    pub fn system_include_dirs<T>(&mut self, dirs: Vec<T>) -> &mut Self
    where
        T: Into<Ustr>,
    {
        let mut new = self;
        new.system_include_dirs = Some(dirs.into_iter().map(Into::into).collect());
        new
    }
}

impl Config {
    /// Validate the passed-in path, and return a canonical, full version of it
    pub fn validate_in_game_path<'a>(
        &self,
        path: &'a LpcPath,
        span: Option<Span>,
    ) -> Result<Cow<'a, Path>> {
        let true_path = path.as_server(self.lib_dir.as_str());

        if path.as_os_str().is_empty() || !true_path.starts_with(self.lib_dir.as_str()) {
            return Err(LpcError::new(format!(
                "attempt to access a file outside of lib_dir: `{}` (expanded to `{}`) (lib_dir: `{}`)",
                path,
                true_path.display(),
                &self.lib_dir
            )).with_span(span));
        }

        Ok(true_path)
    }
}

impl Default for Config {
    fn default() -> Self {
        ConfigBuilder::default().build().unwrap()
    }
}

fn canonicalized_path<P>(path: P) -> Result<Ustr>
where
    P: AsRef<Path>,
{
    match fs::canonicalize(path) {
        Ok(y) => Ok(ustr(&y.to_string_lossy())),
        Err(e) => Err(LpcError::new(e.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_validate_in_game_path {
        use super::*;

        #[test]
        fn test_validate_in_game_path() {
            let config = Config::default();
            let path = LpcPath::new_in_game("/foo/bar.c", "/", &*config.lib_dir);
            let result = config.validate_in_game_path(&path, None);
            assert!(result.is_ok());
        }

        // note: this doesn't actually fail, because the path is canonicalized before it checks.
        // #[test]
        // fn test_validate_in_game_path_outside_lib_dir() {
        //     let config = Config::default();
        //     let path = LpcPath::from("/../baz/../../foo/bar.c");
        //     println!("path: {:?}", path);
        //     let result = config.validate_in_game_path(&path, None);
        //     println!("result: {:?}", result);
        //     assert_eq!(result.unwrap_err().to_string(), "foo");
        // }
    }
}
