use std::{borrow::Cow, collections::HashMap, fmt::Debug, path::Path};

use derive_builder::Builder;
use fs_err as fs;
use lpc_rs_core::lpc_path::{LpcPath, canonicalize_in_game_path};
use lpc_rs_errors::{Result, lpc_error, span::Span};
use tracing::{info, warn};
use tracing_subscriber::{EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};
use ustr::{Ustr, ustr};

use crate::debug_log::DebugLog;

const DEFAULT_MAX_INHERIT_DEPTH: u8 = 10;
const DEFAULT_MAX_EXECUTION_TIME: u64 = 300;
const DEFAULT_GC_INTERVAL: u64 = 300;
const DEFAULT_MAX_PENDING_OUTPUT: usize = 64 * 1024;
const DEFAULT_MAX_IDLE_TIME: u64 = 0;

/// The main struct that handles runtime use configurations.
#[derive(Debug, Builder)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"), pattern = "owned")]
#[readonly::make]
pub struct Config {
    #[builder(setter(into, strip_option), default = "None")]
    pub auto_include_file: Option<Ustr>,

    /// The file every object inherits first, as an in-game path with or
    /// without its `.c`.
    #[builder(setter(into, strip_option), default = "None")]
    pub auto_inherit_file: Option<Ustr>,

    #[builder(setter(into), default = "ustr(\"0.0.0.0\")")]
    pub bind_address: Ustr,

    #[builder(setter(into, strip_option), default = "Some(ustr(\"STDOUT\"))")]
    pub server_log_file: Option<Ustr>,

    #[builder(setter(into, strip_option), default = "None")]
    pub debug_log: Option<DebugLog>,

    #[builder(setter(custom), default = "ustr(\"\")")]
    pub lib_dir: Ustr,

    #[builder(default = "ustr(\"/secure/master.c\")")]
    pub master_object: Ustr,

    #[builder(setter(into), default = "DEFAULT_MAX_EXECUTION_TIME")]
    pub max_execution_time: u64,

    /// Seconds a connection may go without a line of input before the
    /// driver disconnects it; `0` for no limit.
    #[builder(setter(into), default = "DEFAULT_MAX_IDLE_TIME")]
    pub max_idle_time: u64,

    #[builder(default = "DEFAULT_MAX_INHERIT_DEPTH")]
    pub max_inherit_depth: u8,

    /// Bytes pending to one client before its output is truncated; `0` for
    /// no bound.
    #[builder(setter(into), default = "DEFAULT_MAX_PENDING_OUTPUT")]
    pub max_pending_output: usize,

    /// Seconds between garbage-collection passes; `0` disables them.
    #[builder(setter(into), default = "DEFAULT_GC_INTERVAL")]
    pub gc_interval: u64,

    /// The simul-efun file, as an in-game path with or without its `.c`.
    #[builder(setter(into, strip_option), default = "None")]
    pub simul_efun_file: Option<Ustr>,

    #[builder(setter(custom), default = "vec![]")]
    pub system_include_dirs: Vec<Ustr>,

    #[builder(default = "24960")]
    pub port: u16,
}

impl ConfigBuilder {
    /// Set config values from a `dotenv` file. If `env_path` is `None`, the default `.env` is used.
    pub async fn load_env<P>(self, env_path: Option<P>) -> Self
    where
        P: AsRef<Path>,
    {
        let _ = match env_path {
            Some(p) => dotenvy::from_filename(p.as_ref())
                .map_err(|e| info!("{:?} not loaded: {}", p.as_ref(), e.to_string())),
            None => dotenvy::dotenv().map_err(|e| info!(".env not loaded: {}", e.to_string())),
        };

        let env = std::env::vars()
            .map(|(k, v)| {
                let key = k.to_uppercase();

                (key, v)
            })
            .collect::<HashMap<_, _>>();

        let debug_log = {
            let path = env
                .get("LPC_DEBUG_LOG_FILE")
                .or_else(|| env.get("DEBUG_LOG_FILE"))
                .map(|x| ustr(x))
                .unwrap_or_else(|| ustr("STDOUT"));

            DebugLog::from_str(path).await
        };

        let lib_dir = env
            .get("LPC_LIB_DIR")
            .or_else(|| env.get("LIB_DIR"))
            .and_then(|x| canonicalized_path(x).ok())
            .or(self.lib_dir);

        Self {
            auto_include_file: env
                .get("LPC_AUTO_INCLUDE_FILE")
                .or_else(|| env.get("AUTO_INCLUDE_FILE"))
                .map(|x| {
                    let canon = canonicalize_in_game_path(x, "/", lib_dir.unwrap().as_str());
                    Some(ustr(canon.to_string_lossy().as_ref()))
                })
                .or(self.auto_include_file),
            auto_inherit_file: env
                .get("LPC_AUTO_INHERIT_FILE")
                .or_else(|| env.get("AUTO_INHERIT_FILE"))
                .map(|x| {
                    let canon = canonicalize_in_game_path(x, "/", lib_dir.unwrap().as_str());
                    Some(ustr(canon.to_string_lossy().as_ref()))
                })
                .or(self.auto_inherit_file),
            bind_address: env
                .get("LPC_BIND_ADDRESS")
                .or_else(|| env.get("BIND_ADDRESS"))
                .map(|x| ustr(x))
                .or(self.bind_address),
            server_log_file: env
                .get("LPC_SERVER_LOG_FILE")
                .or_else(|| env.get("SERVER_LOG_FILE"))
                .map(|x| canonicalized_path(x).ok())
                .or(self.server_log_file),
            debug_log: Some(Some(debug_log)),
            lib_dir,
            master_object: env
                .get("LPC_MASTER_OBJECT")
                .or_else(|| env.get("MASTER_OBJECT"))
                .map(|x| {
                    let canon = canonicalize_in_game_path(x, "/", lib_dir.unwrap().as_str());
                    ustr(canon.to_string_lossy().as_ref())
                })
                .or(self.master_object),
            max_execution_time: env
                .get("LPC_MAX_EXECUTION_TIME")
                .or_else(|| env.get("MAX_EXECUTION_TIME"))
                .map(|x| x.parse::<u64>().unwrap())
                .or(self.max_execution_time),
            max_idle_time: env
                .get("LPC_MAX_IDLE_TIME")
                .or_else(|| env.get("MAX_IDLE_TIME"))
                .map(|x| x.parse::<u64>().unwrap())
                .or(self.max_idle_time),
            max_inherit_depth: env
                .get("LPC_MAX_INHERIT_DEPTH")
                .or_else(|| env.get("MAX_INHERIT_DEPTH"))
                .map(|x| x.parse::<u8>().unwrap())
                .or(self.max_inherit_depth),
            max_pending_output: env
                .get("LPC_MAX_PENDING_OUTPUT")
                .or_else(|| env.get("MAX_PENDING_OUTPUT"))
                .map(|x| x.parse::<usize>().unwrap())
                .or(self.max_pending_output),
            gc_interval: env
                .get("LPC_GC_INTERVAL")
                .or_else(|| env.get("GC_INTERVAL"))
                .map(|x| x.parse::<u64>().unwrap())
                .or(self.gc_interval),
            port: env
                .get("LPC_PORT")
                .or_else(|| env.get("PORT"))
                .map(|x| x.parse::<u16>().unwrap())
                .or(self.port),
            simul_efun_file: env
                .get("LPC_SIMUL_EFUN_FILE")
                .or_else(|| env.get("SIMUL_EFUN_FILE"))
                .map(|x| {
                    let canon = canonicalize_in_game_path(x, "/", lib_dir.unwrap().as_str());
                    Some(ustr(canon.to_string_lossy().as_ref()))
                })
                .or(self.simul_efun_file),
            system_include_dirs: env
                .get("LPC_SYSTEM_INCLUDE_DIRS")
                .or_else(|| env.get("SYSTEM_INCLUDE_DIRS"))
                .map(|x| {
                    x.split(':')
                        .map(|x| {
                            let canon =
                                canonicalize_in_game_path(x, "/", lib_dir.unwrap().as_str());
                            canon.to_string_lossy().as_ref().into()
                        })
                        .collect::<Vec<_>>()
                })
                .or_else(|| self.system_include_dirs.clone()),
        }
    }

    pub fn lib_dir<S>(mut self, lib_dir: S) -> Self
    where
        S: Into<String>,
    {
        let dir = match canonicalized_path(lib_dir.into()) {
            Ok(x) => x,
            Err(e) => {
                let path = canonicalized_path(".").unwrap();
                warn!("Unable to get canonical path for `lib_dir`: {e}. Using `{path}` instead.");
                path
            }
        };

        self.lib_dir = Some(ustr(&dir));

        self
    }

    /// Set the system include directories. These are in-game directories.
    pub fn system_include_dirs<T>(mut self, dirs: Vec<T>) -> Self
    where
        T: Into<Ustr>,
    {
        self.system_include_dirs = Some(dirs.into_iter().map(Into::into).collect());
        self
    }
}

impl Config {
    /// The source file of the configured simul efuns as one canonical
    /// in-game path — `/secure/simul_efuns`, `/secure/simul_efuns.c` and
    /// `secure/simul_efuns` all name `/secure/simul_efuns.c` — or `None`
    /// when no file is configured.
    pub fn simul_efun_source(&self) -> Option<LpcPath> {
        let file = self.simul_efun_file.as_ref()?;
        Some(LpcPath::new_in_game(file.as_str(), "/", &*self.lib_dir).source_file())
    }

    /// Validate the passed-in path, and return a canonical, absolute on-server version of it
    pub fn validate_in_game_path<'a>(
        &self,
        path: &'a LpcPath,
        span: Option<Span>,
    ) -> Result<Cow<'a, Path>> {
        let true_path = path.as_server(self.lib_dir.as_str());

        if path.as_os_str().is_empty() || !true_path.starts_with(self.lib_dir.as_str()) {
            return Err(lpc_error!(
                span,
                "attempt to access a file outside of lib_dir: `{}`",
                path
            ));
        }

        Ok(true_path)
    }

    /// Set up the global tracing subscriber for the server logs.
    /// This will panic if called multiple times.
    pub fn init_tracing_subscriber(&self) {
        let filter = EnvFilter::builder()
            .with_env_var("RUST_LOG")
            .with_default_directive(tracing::Level::INFO.into())
            .from_env_lossy();

        let registry = tracing_subscriber::registry().with(filter);

        let file = self.server_log_file.as_deref().unwrap_or("STDOUT");

        match file {
            "STDOUT" => registry
                .with(fmt::Layer::default().with_writer(std::io::stdout))
                .init(),
            "STDERR" => registry
                .with(fmt::Layer::default().with_writer(std::io::stderr))
                .init(),
            s => registry
                .with(fmt::Layer::default().with_writer(std::fs::File::create(s).unwrap()))
                .init(),
        }
    }

    /// Log a message to the debug log, if one is configured.
    pub async fn debug_log<M>(&self, msg: M)
    where
        M: Into<Cow<'static, str>> + Send,
    {
        if let Some(debug_log) = &self.debug_log {
            debug_log.log(msg).await;
        } else {
            println!("{}", msg.into());
        }
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
        Err(e) => Err(lpc_error!(e.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simul_efun_source_is_the_source_file_however_the_config_spells_it() {
        for spelling in [
            "/secure/simul_efuns",
            "/secure/simul_efuns.c",
            "secure/simul_efuns",
        ] {
            let config = ConfigBuilder::default()
                .simul_efun_file(spelling)
                .build()
                .unwrap();
            assert_eq!(
                config.simul_efun_source().unwrap().to_string(),
                "/secure/simul_efuns.c",
                "{spelling}"
            );
        }
    }

    #[test]
    fn simul_efun_source_is_none_when_no_file_is_configured() {
        assert!(Config::default().simul_efun_source().is_none());
    }

    #[test]
    fn gc_interval_defaults_to_five_minutes() {
        assert_eq!(Config::default().gc_interval, 300);
        assert_eq!(
            ConfigBuilder::default()
                .gc_interval(0u64)
                .build()
                .unwrap()
                .gc_interval,
            0
        );
    }

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
