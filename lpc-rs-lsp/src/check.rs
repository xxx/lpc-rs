use std::{collections::BTreeMap, sync::Arc};

use codespan_reporting::diagnostic::{
    Diagnostic as CompilerDiagnostic, Label, LabelStyle, Severity,
};
use lpc_rs::compiler::{Compiler, CompilerBuilder};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{
    LpcError,
    source_map::{DiagnosticSources, FileId, SourceMap},
};
use lpc_rs_utils::config::Config;
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Range, Uri,
};

use crate::documents::{Documents, Overlay, file_uri, range};

pub type Reports = BTreeMap<Uri, Vec<Diagnostic>>;

#[derive(Clone)]
pub struct Check {
    pub generation: u64,
    pub documents: Documents,
}

pub async fn check(job: &Check, config: Arc<Config>) -> Reports {
    let sources = DiagnosticSources::scoped();
    let overlay = Arc::new(Overlay::new(&job.documents));
    let mut builder = CompilerBuilder::default();
    builder
        .config(config.clone())
        .source_reader(Some(overlay))
        .diagnostic_sources(sources.clone());
    let mut reports = Reports::new();
    let mut roots: Vec<_> = job
        .documents
        .iter()
        .filter(|(_, document)| document.path.extension().is_some_and(|ext| ext == "c"))
        .collect();
    roots.sort_by_key(|(uri, _)| *uri);
    if roots.is_empty() {
        return reports;
    }
    for (uri, _) in &roots {
        reports.entry((*uri).clone()).or_default();
    }

    let compiler = match builder.build() {
        Ok(compiler) => compiler,
        Err(error) => {
            record(&mut reports, &error, roots[0].0, &sources.read(), &config);
            return reports;
        }
    };
    let sefun_path = config
        .simul_efun_source()
        .map(|path| config.paths().source(&path).server().to_owned());
    if let Some(path) = &sefun_path
        && roots.iter().any(|(_, document)| document.path != *path)
    {
        match compiler.compile_file(LpcPath::new_server(path)).await {
            Ok(compiled) => {
                for group in &compiled.warnings {
                    for warning in &group.warnings {
                        record(&mut reports, warning, roots[0].0, &sources.read(), &config);
                    }
                }
                builder.simul_efuns(Some(Arc::new(compiled.program)));
            }
            Err(error) => {
                let uri = file_uri(path).unwrap_or_else(|| roots[0].0.clone());
                record(&mut reports, &error, &uri, &sources.read(), &config);
                return reports;
            }
        }
    }

    for (uri, document) in roots {
        let path = LpcPath::new_server(&document.path);
        if let Err(error) = config.validate_in_game_path(&path, None) {
            record(&mut reports, &error, uri, &sources.read(), &config);
            continue;
        }
        let compiler = if sefun_path.as_ref() == Some(&document.path) {
            builder.clone().simul_efuns(None).build()
        } else {
            builder.build()
        };
        let compiler: Compiler = match compiler {
            Ok(compiler) => compiler,
            Err(error) => {
                record(&mut reports, &error, uri, &sources.read(), &config);
                continue;
            }
        };
        match compiler.analyze_string(path, &*document.text).await {
            Ok((_, context)) => {
                for error in context.diagnostics.errors().iter().chain(
                    context
                        .inherited_warnings
                        .iter()
                        .flat_map(|group| &group.warnings),
                ) {
                    record(&mut reports, error, uri, &sources.read(), &config);
                }
            }
            Err(error) => record(&mut reports, &error, uri, &sources.read(), &config),
        }
    }
    reports
}

fn location(label: &Label<FileId>, sources: &SourceMap, config: &Config) -> Option<Location> {
    let source = sources.get(label.file_id).ok()?;
    let path = config.paths().resolve(source.name(), "/").ok()?;
    Some(Location {
        uri: file_uri(path.server())?,
        range: range(source.source(), label.range.clone()),
    })
}

fn convert(
    error: CompilerDiagnostic<FileId>,
    root: &Uri,
    sources: &SourceMap,
    config: &Config,
) -> (Uri, Diagnostic) {
    let primary = error
        .labels
        .iter()
        .position(|label| label.style == LabelStyle::Primary);
    let at = primary.and_then(|index| location(&error.labels[index], sources, config));
    let related: Vec<_> = error
        .labels
        .iter()
        .enumerate()
        .filter(|(index, _)| Some(*index) != primary)
        .filter_map(|(_, label)| {
            Some(DiagnosticRelatedInformation {
                location: location(label, sources, config)?,
                message: label.message.clone(),
            })
        })
        .collect();
    let mut message = error.message;
    for note in error.notes {
        message.push_str("\n\n");
        message.push_str(&note);
    }
    let uri = at
        .as_ref()
        .map_or_else(|| root.clone(), |at| at.uri.clone());
    let diagnostic = Diagnostic {
        range: at.map_or_else(Range::default, |at| at.range),
        severity: Some(match error.severity {
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Note | Severity::Help => DiagnosticSeverity::INFORMATION,
            Severity::Error | Severity::Bug => DiagnosticSeverity::ERROR,
        }),
        source: Some("lpc-rs".into()),
        message,
        related_information: (!related.is_empty()).then_some(related),
        ..Diagnostic::default()
    };
    (uri, diagnostic)
}

fn record(
    reports: &mut Reports,
    error: &LpcError,
    root: &Uri,
    sources: &SourceMap,
    config: &Config,
) {
    for error in error.to_diagnostics() {
        let (uri, diagnostic) = convert(error, root, sources, config);
        let diagnostics = reports.entry(uri).or_default();
        if !diagnostics.contains(&diagnostic) {
            diagnostics.push(diagnostic);
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;
    use lsp_types::Position;

    use super::*;
    use crate::documents::Document;

    fn open(job: &mut Check, path: std::path::PathBuf, text: &str) -> Uri {
        let uri = file_uri(&path).unwrap();
        job.documents.insert(
            uri.clone(),
            Document {
                path,
                version: 1,
                text: text.into(),
            },
        );
        uri
    }

    #[tokio::test]
    async fn checks_unsaved_includes_and_inherits_and_maps_related_locations() {
        let dir = tempfile::tempdir().unwrap();
        let config = Arc::new(
            ConfigBuilder::default()
                .lib_dir(dir.path().to_string_lossy())
                .build()
                .unwrap(),
        );
        let mut job = Check {
            generation: 1,
            documents: Documents::new(),
        };
        let parent = open(
            &mut job,
            dir.path().join("parent.c"),
            "int inherited() { return 1; }\n",
        );
        let header = open(
            &mut job,
            dir.path().join("header.h"),
            "int duplicate;\nint duplicate;\n",
        );
        let root = open(
            &mut job,
            dir.path().join("root.c"),
            "inherit \"/parent\";\n#include \"header.h\"\nint f() { return inherited(); }\n",
        );
        let reports = check(&job, config.clone()).await;
        let errors = &reports[&header];
        let duplicate = errors
            .iter()
            .find(|error| error.message.contains("Redefinition"))
            .unwrap();
        assert_eq!(duplicate.range.start.line, 1);
        assert!(
            duplicate
                .related_information
                .as_ref()
                .unwrap()
                .iter()
                .any(|info| info.location.uri == header && info.location.range.start.line == 0)
        );
        job.documents.get_mut(&header).unwrap().text = "int duplicate;\n".into();
        let reports = check(&job, config).await;
        assert!(
            !reports
                .values()
                .flatten()
                .any(|error| error.severity == Some(DiagnosticSeverity::ERROR)),
            "{reports:?}"
        );
        assert!(reports[&parent].is_empty());
        assert!(reports[&root].is_empty());
    }

    #[tokio::test]
    async fn syntax_errors_use_utf16_positions() {
        let dir = tempfile::tempdir().unwrap();
        let config = Arc::new(
            ConfigBuilder::default()
                .lib_dir(dir.path().to_string_lossy())
                .build()
                .unwrap(),
        );
        let mut job = Check {
            generation: 1,
            documents: Documents::new(),
        };
        let text = "string value = \"💖\"; int x = ;\n";
        let root = open(&mut job, dir.path().join("root.c"), text);
        let reports = check(&job, config).await;
        let diagnostic = &reports[&root][0];
        assert_eq!(
            diagnostic.range.start,
            Position::new(
                0,
                text[..text.rfind(';').unwrap()].encode_utf16().count() as u32
            )
        );
    }

    #[tokio::test]
    async fn configured_includes_inherits_and_simul_efuns_are_checked_without_initializing() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("auto.h"), "#define AUTO_VALUE 3\n").unwrap();
        std::fs::write(dir.path().join("base.c"), "int inherited() { return 2; }\n").unwrap();
        std::fs::write(
            dir.path().join("sefun.c"),
            "int helper() { return 1; }\nvoid create() { write_file(\"/executed\", \"bad\"); }\n",
        )
        .unwrap();
        let config = Arc::new(
            ConfigBuilder::default()
                .lib_dir(dir.path().to_string_lossy())
                .auto_include_file("/auto.h")
                .auto_inherit_file("/base")
                .simul_efun_file("/sefun")
                .build()
                .unwrap(),
        );
        let mut job = Check {
            generation: 1,
            documents: Documents::new(),
        };
        let root = open(
            &mut job,
            dir.path().join("root.c"),
            "int result() { return helper() + inherited() + AUTO_VALUE; }\n",
        );
        let reports = check(&job, config).await;
        assert!(reports[&root].is_empty(), "{reports:?}");
        assert!(
            !reports
                .values()
                .flatten()
                .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR)),
            "{reports:?}"
        );
        assert!(!dir.path().join("executed").exists());
    }
}
