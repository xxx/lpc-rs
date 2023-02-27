use std::{path::Path, str::FromStr};

use derive_builder::Builder;
use fs_err as fs;
use if_chain::if_chain;
use lpc_rs_errors::{LpcError, Result};
use toml::{value::Index, Value};

const DEFAULT_CONFIG_FILE: &str = "./config.toml";
const DEFAULT_MAX_INHERIT_DEPTH: usize = 10;

const AUTO_INCLUDE_FILE: &[&str] = &["lpc-rs", "auto_include_file"];
const AUTO_INHERIT_FILE: &[&str] = &["lpc-rs", "auto_inherit_file"];
const LIB_DIR: &[&str] = &["lpc-rs", "lib_dir"];
const MAX_INHERIT_DEPTH: &[&str] = &["lpc-rs", "max_inherit_depth"];
const MAX_TASK_INSTRUCTIONS: &[&str] = &["lpc-rs", "max_task_instructions"];
const SIMUL_EFUN_FILE: &[&str] = &["lpc-rs", "simul_efun_file"];
const SYSTEM_INCLUDE_DIRS: &[&str] = &["lpc-rs", "system_include_dirs"];

const MASTER_OBJECT: &[&str] = &["driver", "master_object"];
const DRIVER_LOG_LEVEL: &[&str] = &["driver", "log_level"];
const DRIVER_LOG_FILE: &[&str] = &["driver", "log_file"];

/// The main struct that handles runtime use configurations.
#[derive(Debug, Builder)]
#[builder(build_fn(name = "real_build", error = "lpc_rs_errors::LpcError"))]
#[readonly::make]
pub struct Config {
    #[builder(default = "None")]
    #[allow(dead_code)]
    config: Option<Value>,

    #[builder(setter(into), default = "None")]
    #[allow(dead_code)]
    pub path: Option<String>,

    #[builder(setter(into, strip_option), default = "self.get_auto_include_file()")]
    pub auto_include_file: Option<String>,

    #[builder(setter(into, strip_option), default = "self.get_auto_inherit_file()")]
    pub auto_inherit_file: Option<String>,

    #[builder(setter(into, strip_option), default = "self.get_driver_log_file()")]
    pub driver_log_file: Option<String>,

    #[builder(setter(strip_option), default = "self.get_driver_log_level()")]
    pub driver_log_level: Option<tracing::Level>,

    #[builder(setter(custom), default = "self.get_lib_dir()?")]
    pub lib_dir: String,

    #[builder(default = "self.get_master_object()?")]
    pub master_object: String,

    #[builder(default = "self.get_max_inherit_depth()?")]
    pub max_inherit_depth: usize,

    #[builder(
        setter(into, strip_option),
        default = "self.get_max_task_instructions()"
    )]
    pub max_task_instructions: Option<usize>,

    #[builder(setter(into, strip_option), default = "self.get_simul_efun_file()")]
    pub simul_efun_file: Option<String>,

    #[builder(setter(custom), default = "self.get_system_include_dirs()?")]
    pub system_include_dirs: Vec<String>,
}

impl ConfigBuilder {
    pub fn build(&mut self) -> Result<Config> {
        let config_str = match &self.path {
            Some(Some(path)) => fs::read_to_string(path)?,
            Some(None) => fs::read_to_string(DEFAULT_CONFIG_FILE)?,
            _ => "".into(),
        };

        // If there's no config at all, we'll fall back to the fields populated by Config::default()
        if !config_str.is_empty() {
            match config_str.parse::<Value>() {
                Ok(config) => {
                    self.config(Some(config));
                }
                Err(e) => return Err(LpcError::new(e.to_string())),
            }
        }

        self.real_build()
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
                eprintln!(
                    "Unable to get canonical path for `lib_dir`: {e}. Using `{path}` instead."
                );
                path
            }
        };

        new.lib_dir = Some(dir);

        new
    }

    pub fn system_include_dirs<T>(&mut self, dirs: Vec<T>) -> &mut Self
    where
        T: Into<String>,
    {
        let mut new = self;
        new.system_include_dirs = Some(dirs.into_iter().map(Into::into).collect());
        new
    }

    fn get_simul_efun_file(&self) -> Option<String> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().simul_efun_file;
        };

        let dug = dig(binding, SIMUL_EFUN_FILE);
        dug.and_then(|x| x.as_str()).map(String::from)
    }

    fn get_driver_log_file(&self) -> Option<String> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().driver_log_file;
        };

        let dug = dig(binding, DRIVER_LOG_FILE);
        dug.and_then(|x| x.as_str()).map(String::from)
    }

    fn get_driver_log_level(&self) -> Option<tracing::Level> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().driver_log_level;
        };

        let dug = dig(binding, DRIVER_LOG_LEVEL);
        dug.and_then(|x| x.as_str())
            .and_then(|x| tracing::Level::from_str(x).ok())
    }

    fn get_system_include_dirs(&self) -> Result<Vec<String>> {
        let Some(Some(binding)) = &self.config else {
            return Ok(Config::default().system_include_dirs);
        };

        let system_include_dirs = match dig(binding, SYSTEM_INCLUDE_DIRS) {
            Some(v) => match v.as_array() {
                Some(arr) => arr
                    .iter()
                    .filter_map(|x| x.as_str())
                    .map(String::from)
                    .collect(),
                None => {
                    return Err(LpcError::new(format!(
                        "Expected array for system_include_dirs, found {v}"
                    )))
                }
            },
            None => vec![],
        };

        Ok(system_include_dirs)
    }

    fn get_lib_dir(&self) -> Result<String> {
        let Some(Some(binding)) = &self.config else {
            return Ok(Config::default().lib_dir);
        };

        let dug = dig(binding, LIB_DIR);
        let non_canon = match dug {
            Some(x) => String::from(x.as_str().unwrap_or(".")),
            None => {
                eprintln!("No configuration for `lib_dir` found. Using \".\" instead.");
                String::from(".")
            }
        };
        let lib_dir = canonicalized_path(non_canon)?;

        Ok(lib_dir)
    }

    fn get_master_object(&self) -> Result<String> {
        let Some(Some(binding)) = &self.config else {
            return Ok(Config::default().master_object);
        };

        let dug = dig(binding, MASTER_OBJECT);
        let master_object = match dug {
            Some(x) => match x.as_str() {
                Some(s) => String::from(s),
                None => {
                    return Err(LpcError::new(
                        "Invalid configuration for `master_object` found. Cannot continue.",
                    ));
                }
            },
            None => {
                return Err(LpcError::new(
                    "No configuration for `master_object` found. Cannot continue.",
                ));
            }
        };

        Ok(master_object)
    }

    fn get_max_inherit_depth(&self) -> Result<usize> {
        let Some(Some(binding)) = &self.config else {
            return Ok(Config::default().max_inherit_depth);
        };

        let dug = dig(binding, MAX_INHERIT_DEPTH);
        let max_inherit_depth = match dug {
            Some(x) => match x.as_integer() {
                Some(0) => DEFAULT_MAX_INHERIT_DEPTH,
                Some(y) => {
                    if y < 1 {
                        return Err(LpcError::new("max_inherit_depth must be greater than 0"));
                    } else {
                        y as usize
                    }
                }
                None => {
                    return Err(LpcError::new(
                        "max_inherit_depth must be a positive integer",
                    ))
                }
            },
            None => DEFAULT_MAX_INHERIT_DEPTH,
        };

        Ok(max_inherit_depth)
    }

    fn get_max_task_instructions(&self) -> Option<usize> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().max_task_instructions;
        };

        let dug = dig(binding, MAX_TASK_INSTRUCTIONS);
        if_chain! {
            if let Some(x) = dug;
            if let Some(y) = x.as_integer();
            if y >= 0;
            then {
                Some(y as usize)
            } else {
                None
            }
        }
    }

    fn get_auto_include_file(&self) -> Option<String> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().auto_include_file;
        };

        let dug = dig(binding, AUTO_INCLUDE_FILE);
        dug.and_then(|x| x.as_str()).map(String::from)
    }

    fn get_auto_inherit_file(&self) -> Option<String> {
        let Some(Some(binding)) = &self.config else {
            return Config::default().auto_inherit_file;
        };

        let dug = dig(binding, AUTO_INHERIT_FILE);
        dug.and_then(|x| x.as_str()).map(String::from)
    }
}

impl Default for Config {
    // The bare-bones default Config, used if there is no config file at all found.
    fn default() -> Self {
        Self {
            config: None,
            path: None,
            lib_dir: "".to_string(),
            system_include_dirs: vec!["/sys".into()],
            master_object: "/secure/master.c".to_string(),
            max_task_instructions: Some(100000),
            max_inherit_depth: DEFAULT_MAX_INHERIT_DEPTH,
            driver_log_level: None,
            driver_log_file: Some("STDOUT".into()),
            simul_efun_file: None,
            auto_include_file: None,
            auto_inherit_file: None,
        }
    }
}

fn dig<'a, I>(toml: &'a Value, path: &'_ [I]) -> Option<&'a Value>
where
    I: Index,
{
    let mut result = toml;

    for index in path {
        match result.get(index) {
            Some(x) => {
                result = x;
            }
            None => return None,
        }
    }

    Some(result)
}

fn canonicalized_path<P>(path: P) -> Result<String>
where
    P: AsRef<Path>,
{
    match fs::canonicalize(path) {
        Ok(y) => Ok(y.to_string_lossy().into_owned()),
        Err(e) => Err(LpcError::new(e.to_string())),
    }
}
