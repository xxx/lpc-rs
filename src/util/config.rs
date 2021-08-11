use crate::{errors::LpcError, Result};
use fs_err as fs;
use std::path::Path;
use toml::{value::Index, Value};

const DEFAULT_CONFIG_FILE: &str = "./config.toml";

const LIB_DIR: &[&str] = &["lpc-rs", "lib_dir"];
const MAX_CALL_STACK_SIZE: &[&str] = &["lpc-rs", "max_call_stack_size"];
const MAX_TASK_INSTRUCTIONS: &[&str] = &["lpc-rs", "max_task_instructions"];
const SYSTEM_INCLUDE_DIRS: &[&str] = &["lpc-rs", "system_include_dirs"];

const MASTER_OBJECT: &[&str] = &["driver", "master_object"];

#[derive(Debug, Default)]
pub struct Config {
    lib_dir: String,
    system_include_dirs: Vec<String>,
    master_object: String,
    max_call_stack_size: Option<usize>,
    max_task_instructions: Option<usize>,
}

impl Config {
    pub fn new<P>(config_override: Option<P>) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let config_str = match config_override {
            Some(path) => fs::read_to_string(path)?,
            None => fs::read_to_string(DEFAULT_CONFIG_FILE)?,
        };

        let config = match config_str.parse::<Value>() {
            Ok(x) => x,
            Err(e) => return Err(LpcError::new(e.to_string())),
        };

        let system_include_dirs = match dig(&config, SYSTEM_INCLUDE_DIRS) {
            Some(v) => match v.as_array() {
                Some(arr) => arr
                    .iter()
                    .map(|x| x.as_str())
                    .flatten()
                    .map(String::from)
                    .collect(),
                None => {
                    return Err(LpcError::new(format!(
                        "Expected array for system_include_dirs, found {}",
                        v
                    )))
                }
            },
            None => Vec::new(), // TODO: add warning about missing config
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

        let dug = dig(&config, MAX_CALL_STACK_SIZE);
        let max_call_stack_size = match dug {
            Some(x) => {
                match x.as_integer() {
                    Some(y) => {
                        if y < 1 {
                            None
                        } else {
                            Some(y as usize)
                        }
                    }
                    None => None,
                }
            }
            None => None,
        };

        let dug = dig(&config, MAX_TASK_INSTRUCTIONS);
        let max_task_instructions = match dug {
            Some(x) => {
                match x.as_integer() {
                    Some(y) => {
                        if y < 1 {
                            None
                        } else {
                            Some(y as usize)
                        }
                    }
                    None => None,
                }
            }
            None => None,
        };

        Ok(Self {
            lib_dir,
            system_include_dirs,
            master_object,
            max_call_stack_size,
            max_task_instructions,
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

    pub fn with_max_call_stack_size(mut self, max_call_stack_size: Option<usize>) -> Self
    {
        self.max_call_stack_size = max_call_stack_size;

        self
    }

    pub fn with_max_task_instructions(mut self, max_task_instructions: Option<usize>) -> Self
    {
        self.max_task_instructions = max_task_instructions;

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
    pub fn max_call_stack_size(&self) -> Option<usize> {
        self.max_call_stack_size
    }

    #[inline]
    pub fn max_task_instructions(&self) -> Option<usize> {
        self.max_task_instructions
    }

    #[inline]
    pub fn system_include_dirs(&self) -> &Vec<String> {
        &self.system_include_dirs
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
