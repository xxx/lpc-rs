#![forbid(unsafe_code)]

use std::{
    io::{self, Write},
    sync::Arc,
};

use clap::Parser;
use lpc_rs::{
    compiler::{Compiled, CompilerBuilder},
    interpreter::{process::Process, vm::Vm},
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_utils::config::{Config, ConfigBuilder};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The LPC file to compile
    #[clap(value_parser, value_name = "FILE", value_hint = clap::ValueHint::FilePath)]
    filename: String,

    /// Use a specific configuration file
    #[clap(short, long, value_parser)]
    config: Option<String>,

    /// Compile only: the file is not initialized, so its create() does not run
    #[clap(long)]
    check: bool,

    /// Print the assembly listing to stdout without executing LPC
    #[clap(short = 'S', long, conflicts_with = "check")]
    emit_asm: bool,
}

fn main() {
    if let Err(error) = lpc_rs::runtime::run(run()) {
        error.emit_diagnostics();
        std::process::exit(1);
    }
}

async fn run() -> Result<()> {
    let args = Args::parse();

    let config = ConfigBuilder::default()
        .load_env(args.config)
        .await
        .and_then(ConfigBuilder::build)?;

    let config = Arc::new(config);

    let lpc_path = LpcPath::new_server(&args.filename);

    if args.emit_asm {
        return emit_assembly(config, &lpc_path).await;
    }

    let vm = Vm::new(config.clone());

    // Loading the sefun file first when it is also the target redefines its
    // own nomask functions.
    let target_is_sefun_file = config.simul_efun_source().is_some_and(|sefuns| {
        config.paths().program_path(&sefuns) == config.paths().program_path(&lpc_path)
    });
    if !target_is_sefun_file && let Some(result) = vm.initialize_simul_efuns().await {
        result?;
    }

    if args.check {
        vm.compile_from_path(&lpc_path).await
    } else {
        vm.initialize_process_from_path(&lpc_path).await.map(|_| ())
    }
}

async fn emit_assembly(config: Arc<Config>, path: &LpcPath) -> Result<()> {
    let mut builder = CompilerBuilder::default();
    builder.config(config.clone());

    if let Some(sefuns) = config.simul_efun_source()
        && config.paths().program_path(&sefuns) != config.paths().program_path(path)
    {
        let compiled = builder.build()?.compile_in_game_file(&sefuns, None).await?;
        emit_warnings(&compiled);
        builder.simul_efuns(Some(Arc::new(Process::new(compiled.program))));
    }

    let compiled = builder.build()?.compile_in_game_file(path, None).await?;
    emit_warnings(&compiled);

    let mut output = io::stdout().lock();
    for line in compiled.program.listing() {
        writeln!(output, "{line}").map_err(|error| lpc_error!("cannot write assembly: {error}"))?;
    }
    Ok(())
}

fn emit_warnings(compiled: &Compiled) {
    for warning in compiled.warnings.iter().flat_map(|group| &group.warnings) {
        warning.emit_diagnostics();
    }
}
