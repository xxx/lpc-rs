use crate::Result;
use toml::Value;
use crate::errors::LpcError;
use std::fs;
use std::path::Path;
use toml::value::Index;

const CONFIG_FILE: &str = "./config.toml";

const LIB_DIR: &[&str] = &["lpc-rs", "lib_dir"];
const SYSTEM_INCLUDE_DIRS: &[&str] = &["lpc-rs", "system_include_dirs"];

#[derive(Debug, Default)]
pub struct Config {
    lib_dir: String,
    system_include_dirs: Vec<String>,
}

impl Config {
    pub fn new<P>(config_override: Option<P>) -> Result<Self>
    where
        P: AsRef<Path>
    {
        let config_str = match config_override {
            Some(path) => fs::read_to_string(path)?,
            None => fs::read_to_string(CONFIG_FILE)?
        };

        let config = match config_str.parse::<Value>() {
            Ok(x) => x,
            Err(e) => return Err(LpcError::new(e.to_string()))
        };

        let system_include_dirs = match dig(&config, SYSTEM_INCLUDE_DIRS) {
            Some(v) => {
                match v.as_array() {
                    Some(arr) => arr.iter().map(|x| x.as_str()).flatten().map(String::from).collect(),
                    None => return Err(LpcError::new(format!("Expected array for system_include_dirs, found {}", v)))
                }
            }
            None => Vec::new() // TODO: add warning about missing config
        };

        let dug = dig(&config, LIB_DIR);
        let non_canon = match dug {
            Some(x) => {
                String::from(x.as_str().unwrap_or("."))
            },
            None => {
                eprintln!("No configuration for `lib_dir` found. Using \".\" instead.");
                String::from(".")
            }
        };

        let lib_dir = canonicalized_path(non_canon)?;

        Ok(Self {
            lib_dir,
            system_include_dirs,
        })
    }

    pub fn with_lib_dir<S>(mut self, lib_dir: S) -> Self
    where
        S: Into<String>
    {
        self.lib_dir = match canonicalized_path(lib_dir.into()) {
            Ok(x) => x,
            Err(e) => {
                let path = canonicalized_path(".").unwrap();
                eprintln!(
                    "Unable to get canonical path for `lib_dir`: {}. Using `{}` instead.",
                    e,
                    path
                );
                path
            },
        };

        self
    }

    pub fn with_system_include_dirs<S>(mut self, system_include_dirs: Vec<S>) -> Self
    where
        S: Into<String>
    {
        self.system_include_dirs = system_include_dirs.into_iter().map(Into::into).collect();

        self
    }

    #[inline]
    pub fn lib_dir(&self) -> &str {
        &self.lib_dir
    }

    #[inline]
    pub fn system_include_dirs(&self) -> &Vec<String> {
        &self.system_include_dirs
    }
}

fn dig<'a, I>(toml: &'a Value, path: &'_ [I]) -> Option<&'a Value>
where
    I: Index
{
    let mut result = toml;

    for index in path {
        match result.get(index) {
            Some(x) => {
                result = x;
            }
            None => return None
        }
    }

    Some(result)
}

fn canonicalized_path<P>(path: P) -> Result<String>
where
    P: AsRef<Path>,
{
    match fs::canonicalize(path) {
        Ok(y) => {
            Ok(y.to_string_lossy().into_owned())
        }
        Err(e) => Err(LpcError::new(e.to_string()))
    }
}