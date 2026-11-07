#![no_main]

use std::{io, path::Path, sync::Arc};

use async_trait::async_trait;
use codespan_reporting::term::{self, termcolor::Buffer};
use libfuzzer_sys::fuzz_target;
use lpc_rs::compiler::{
    CompilerBuilder,
    source_reader::{SourceKind, SourceReader},
};
use lpc_rs_errors::{LpcError, source_map::DiagnosticSources};
use lpc_rs_utils::config::ConfigBuilder;

fn render(error: &LpcError, sources: &DiagnosticSources) {
    for diagnostic in error.to_diagnostics() {
        term::emit_to_write_style(
            &mut Buffer::no_color(),
            &term::Config::default(),
            &*sources.read(),
            &diagnostic,
        )
        .expect("compiler diagnostic must reference valid source spans");
    }
}

#[derive(Debug)]
struct Sources {
    header: Vec<u8>,
}

#[async_trait]
impl SourceReader for Sources {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        match path.file_name().and_then(|name| name.to_str()) {
            Some("fuzz.h") => Ok(self.header.clone()),
            Some("parent.c") => Ok(b"int inherited(int n) { return n + 1; }".to_vec()),
            _ => Err(io::Error::from(io::ErrorKind::NotFound)),
        }
    }

    async fn kind(&self, path: &Path) -> io::Result<SourceKind> {
        self.read(path).await.map(|_| SourceKind::File)
    }
}

fuzz_target!(|data: &[u8]| {
    let text = String::from_utf8_lossy(data);
    let (header, code) = text.split_once("\n// fuzz-source\n").unwrap_or(("", &text));
    let runtime = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    runtime.block_on(async {
        for null_mode in [
            lpc_rs_core::null_mode::NullMode::Zero,
            lpc_rs_core::null_mode::NullMode::Distinct,
        ] {
            let config = ConfigBuilder::default()
                .lib_dir("/fuzz")
                .null_mode(null_mode)
                .compiler_cache_idle_time(0)
                .max_inherit_depth(4)
                .build()
                .unwrap();
            let sources = DiagnosticSources::scoped();
            let compiler = CompilerBuilder::default()
                .config(config)
                .diagnostic_sources(sources.clone())
                .source_reader(Some(Arc::new(Sources {
                    header: header.as_bytes().to_vec(),
                }) as Arc<dyn SourceReader>))
                .build()
                .unwrap();
            match compiler.compile_string("/input.c", code).await {
                Ok(compiled) => {
                    for group in &compiled.warnings {
                        for warning in &group.warnings {
                            render(warning, &sources);
                        }
                    }
                }
                Err(error) => {
                    assert!(!error.is_bug(), "{error}");
                    render(&error, &sources);
                }
            }
        }
    });
});
