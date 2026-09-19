#![forbid(unsafe_code)]
#![expect(
    clippy::mutable_key_type,
    reason = "LSP URI equality and ordering use immutable text; only parsed components are cached"
)]

use std::{path::PathBuf, process::ExitCode};

use clap::Parser;
use lsp_server::Connection;

mod check;
mod documents;
mod efuns;
mod server;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    /// Mudlib root; defaults to LIB_DIR or the first editor workspace folder.
    #[arg(long)]
    lib_dir: Option<PathBuf>,
    /// Explicit driver environment file containing compiler settings.
    #[arg(short, long)]
    config: Option<PathBuf>,
}

fn main() -> ExitCode {
    let args = Args::parse();
    let (connection, threads) = Connection::stdio();
    match server::run(connection, args) {
        Ok(()) => match threads.join() {
            Ok(()) => ExitCode::SUCCESS,
            Err(error) => {
                eprintln!("lpc-rs-lsp: {error}");
                ExitCode::FAILURE
            }
        },
        Err(error) => {
            eprintln!("lpc-rs-lsp: {error:#}");
            ExitCode::FAILURE
        }
    }
}
