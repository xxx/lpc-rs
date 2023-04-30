use std::{future::Future, sync::Arc};

use async_trait::async_trait;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;

use crate::{
    compiler::{Compiler, CompilerBuilder},
    interpreter::object_space::ObjectSpace,
    util::get_simul_efuns,
};

/// A convenience trait to make it easier to get access to a pre-configured [`Compiler`].
#[async_trait]
pub trait WithCompiler {
    /// Run a callback with a new, initialized [`Compiler`], that can handle callbacks that return futures.
    async fn with_async_compiler<F, U, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send;

    /// Run a callback with a new, initialized [`Compiler`], that can handle callbacks that return futures.
    async fn with_async_compiler_associated<F, U, T>(
        f: F,
        config: &Arc<Config>,
        object_space: &ObjectSpace,
    ) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send,
    {
        let compiler = CompilerBuilder::default()
            .config(config.clone())
            .simul_efuns(get_simul_efuns(config, object_space))
            .build()?;
        f(compiler).await
    }
}

/// A generic impl that anything with a `Config` and an `ObjectSpace` can use.
#[async_trait]
impl WithCompiler for (&Arc<Config>, &ObjectSpace) {
    async fn with_async_compiler<F, U, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send,
    {
        Self::with_async_compiler_associated(f, self.0, self.1).await
    }
}