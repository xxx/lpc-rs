use fs_err as fs;
use if_chain::if_chain;
use lpc_rs_errors::{LpcError, Result};
use std::{path::Path, str::FromStr};
use toml::{value::Index, Value};

const DEFAULT_CONFIG_FILE: &str = "./config.toml";
const DEFAULT_MAX_INHERIT_DEPTH: usize = 10;

const LIB_DIR: &[&str] = &["lpc-rs", "lib_dir"];
const MAX_INHERIT_DEPTH: &[&str] = &["lpc-rs", "max_inherit_depth"];
const MAX_TASK_INSTRUCTIONS: &[&str] = &["lpc-rs", "max_task_instructions"];
const SIMUL_EFUN_FILE: &[&str] = &["lpc-rs", "simul_efun_file"];
const SYSTEM_INCLUDE_DIRS: &[&str] = &["lpc-rs", "system_include_dirs"];

const MASTER_OBJECT: &[&str] = &["driver", "master_object"];
const DRIVER_LOG_LEVEL: &[&str] = &["driver", "log_level"];
const DRIVER_LOG_FILE: &[&str] = &["driver", "log_file"];

/// The main struct that handles runtime use configurations.
#[derive(Debug)]
pub struct Config {
    lib_dir: String,
    system_include_dirs: Vec<String>,
    master_object: String,
    max_task_instructions: Option<usize>,
    max_inherit_depth: usize,
    driver_log_level: Option<tracing::Level>,
    driver_log_file: Option<String>,
    simul_efun_file: Option<String>,
}

impl Config {
    pub fn new<P>(config_override: Option<P>) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let config_str = match config_override {
            Some(path) => fs::read_to_string(path)?,
            None => fs::read_to_string(DEFAULT_CONFIG_FILE).unwrap_or_else(|_| "".into()),
        };

        let config = if config_str.is_empty() {
            return Ok(Self::default());
        } else {
            match config_str.parse::<Value>() {
                Ok(x) => x,
                Err(e) => return Err(LpcError::new(e.to_string())),
            }
        };

        let system_include_dirs = match dig(&config, SYSTEM_INCLUDE_DIRS) {
            Some(v) => match v.as_array() {
                Some(arr) => arr
                    .iter()
                    .filter_map(|x| x.as_str())
                    .map(String::from)
                    .collect(),
                None => {
                    return Err(LpcError::new(format!(
                        "Expected array for system_include_dirs, found {}",
                        v
                    )))
                }
            },
            None => vec![],
        };

        let dug = dig(&config, LIB_DIR);
        let non_canon = match dug {
            Some(x) => String::from(x.as_str().unwrap_or(".")),
            None => {
                eprintln!("No configuration for `lib_dir` found. Using \".\" instead.");
                String::from(".")
            }
        };
        let lib_dir = canonicalized_path(non_canon)?;

        let dug = dig(&config, MASTER_OBJECT);
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

        let dug = dig(&config, MAX_INHERIT_DEPTH);
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

        let dug = dig(&config, MAX_TASK_INSTRUCTIONS);
        let max_task_instructions = if_chain! {
            if let Some(x) = dug;
            if let Some(y) = x.as_integer();
            if y >= 0;
            then {
                Some(y as usize)
            } else {
                None
            }
        };

        let dug = dig(&config, DRIVER_LOG_LEVEL);
        let driver_log_level = dug
            .and_then(|x| x.as_str())
            .and_then(|x| tracing::Level::from_str(x).ok());

        let dug = dig(&config, DRIVER_LOG_FILE);
        let driver_log_file = dug.and_then(|x| x.as_str()).map(String::from);

        let dug = dig(&config, SIMUL_EFUN_FILE);
        let simul_efun_file = dug.and_then(|x| x.as_str()).map(String::from);

        Ok(Self {
            lib_dir,
            system_include_dirs,
            master_object,
            max_inherit_depth,
            max_task_instructions,
            driver_log_level,
            driver_log_file,
            simul_efun_file,
        })
    }

    pub fn with_lib_dir<S>(mut self, lib_dir: S) -> Self
    where
        S: Into<String>,
    {
        self.lib_dir = match canonicalized_path(lib_dir.into()) {
            Ok(x) => x,
            Err(e) => {
                let path = canonicalized_path(".").unwrap();
                eprintln!(
                    "Unable to get canonical path for `lib_dir`: {}. Using `{}` instead.",
                    e, path
                );
                path
            }
        };

        self
    }

    pub fn with_system_include_dirs<S>(mut self, system_include_dirs: Vec<S>) -> Self
    where
        S: Into<String>,
    {
        self.system_include_dirs = system_include_dirs.into_iter().map(Into::into).collect();

        self
    }

    pub fn with_max_task_instructions(mut self, max_task_instructions: Option<usize>) -> Self {
        self.max_task_instructions = max_task_instructions;

        self
    }

    pub fn with_simul_efun_file<T>(mut self, file: Option<T>) -> Self
    where
        T: Into<String>,
    {
        self.simul_efun_file = file.map(|t| t.into());

        self
    }

    #[inline]
    pub fn lib_dir(&self) -> &str {
        &self.lib_dir
    }

    #[inline]
    pub fn master_object(&self) -> &str {
        &self.master_object
    }

    #[inline]
    pub fn max_inherit_depth(&self) -> usize {
        self.max_inherit_depth
    }

    #[inline]
    pub fn max_task_instructions(&self) -> Option<usize> {
        self.max_task_instructions
    }

    #[inline]
    pub fn system_include_dirs(&self) -> &Vec<String> {
        &self.system_include_dirs
    }

    #[inline]
    pub fn driver_log_level(&self) -> Option<tracing::Level> {
        self.driver_log_level
    }

    #[inline]
    pub fn driver_log_file(&self) -> Option<&str> {
        self.driver_log_file.as_deref()
    }

    #[inline]
    pub fn simul_efun_file(&self) -> Option<&str> {
        self.simul_efun_file.as_deref()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            lib_dir: "".to_string(),
            system_include_dirs: vec!["/sys".into()],
            master_object: "/secure/master.c".to_string(),
            max_task_instructions: Some(100000),
            max_inherit_depth: DEFAULT_MAX_INHERIT_DEPTH,
            driver_log_level: None,
            driver_log_file: Some("STDOUT".into()),
            simul_efun_file: None,
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