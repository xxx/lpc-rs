use std::{collections::BTreeSet, sync::Arc, time::Duration};

use anyhow::{Context, Result, bail};
use lpc_rs::compile_time_config::THREAD_STACK;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{
    CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, PositionEncodingKind, PublishDiagnosticsParams,
    SaveOptions, ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Uri,
};
use parking_lot::Mutex;
use serde_json::Value;

use crate::{
    Args,
    check::{Check, Reports, check},
    documents::{Document, Documents, file_path},
    efuns,
};

#[derive(Default)]
struct State {
    documents: Documents,
    generation: u64,
    published: BTreeSet<Uri>,
}

impl State {
    fn diagnostic_notification(
        &self,
        uri: Uri,
        diagnostics: Vec<lsp_types::Diagnostic>,
    ) -> Notification {
        let version = self.documents.get(&uri).map(|doc| doc.version);
        Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version,
            },
        )
    }

    fn invalidate(&mut self) -> Vec<Notification> {
        self.generation += 1;
        std::mem::take(&mut self.published)
            .into_iter()
            .map(|uri| self.diagnostic_notification(uri, vec![]))
            .collect()
    }

    fn publish(&mut self, generation: u64, reports: Reports) -> Vec<Notification> {
        if generation != self.generation {
            return vec![];
        }
        let mut notifications = Vec::new();
        for (uri, diagnostics) in reports {
            self.published.insert(uri.clone());
            notifications.push(self.diagnostic_notification(uri, diagnostics));
        }
        notifications
    }

    fn update(&mut self, notification: Notification) -> Result<(Vec<Notification>, Option<Check>)> {
        let schedule = match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let item = params.text_document;
                let Some(path) = file_path(&item.uri) else {
                    return Ok((vec![], None));
                };
                self.documents.insert(
                    item.uri,
                    Document {
                        path,
                        version: item.version,
                        text: item.text.into(),
                    },
                );
                true
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let Some(document) = self.documents.get_mut(&params.text_document.uri) else {
                    return Ok((vec![], None));
                };
                if params.text_document.version <= document.version {
                    return Ok((vec![], None));
                }
                if params.content_changes.len() != 1 || params.content_changes[0].range.is_some() {
                    bail!("this server negotiated full document synchronization");
                }
                document.text = params
                    .content_changes
                    .into_iter()
                    .next()
                    .context("missing document text")?
                    .text
                    .into();
                document.version = params.text_document.version;
                false
            }
            "textDocument/didSave" => {
                let params: DidSaveTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                let Some(document) = self.documents.get_mut(&params.text_document.uri) else {
                    return Ok((vec![], None));
                };
                if let Some(text) = params.text {
                    document.text = text.into();
                }
                true
            }
            "textDocument/didClose" => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params)?;
                if self.documents.remove(&params.text_document.uri).is_none() {
                    return Ok((vec![], None));
                }
                true
            }
            _ => return Ok((vec![], None)),
        };
        let notifications = self.invalidate();
        let job = schedule.then(|| Check {
            generation: self.generation,
            documents: self.documents.clone(),
        });
        Ok((notifications, job))
    }

    fn request(&self, request: Request) -> Response {
        let result = match request.method.as_str() {
            "textDocument/completion" => serde_json::from_value::<CompletionParams>(request.params)
                .and_then(|params| {
                    let at = params.text_document_position;
                    let items = self
                        .documents
                        .get(&at.text_document.uri)
                        .map(|document| efuns::complete(&document.text, at.position))
                        .unwrap_or_default();
                    serde_json::to_value(items)
                }),
            "textDocument/hover" => {
                serde_json::from_value::<HoverParams>(request.params).and_then(|params| {
                    let at = params.text_document_position_params;
                    let hover = self
                        .documents
                        .get(&at.text_document.uri)
                        .and_then(|document| efuns::hover(&document.text, at.position));
                    serde_json::to_value(hover)
                })
            }
            _ => {
                return Response::new_err(
                    request.id,
                    ErrorCode::MethodNotFound as i32,
                    format!("Unsupported method: {}", request.method),
                );
            }
        };
        match result {
            Ok(value) => Response::new_ok(request.id, value),
            Err(error) => Response::new_err(
                request.id,
                ErrorCode::InvalidParams as i32,
                error.to_string(),
            ),
        }
    }
}

fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(PositionEncodingKind::UTF16),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
                ..TextDocumentSyncOptions::default()
            },
        )),
        completion_provider: Some(CompletionOptions::default()),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
    }
}

#[allow(deprecated)]
async fn configuration(args: Args, params: &InitializeParams) -> Result<Arc<Config>> {
    let root = params
        .workspace_folders
        .as_ref()
        .and_then(|folders| folders.first())
        .map(|folder| &folder.uri)
        .or(params.root_uri.as_ref())
        .and_then(file_path)
        .unwrap_or(std::env::current_dir()?);
    let mut builder = ConfigBuilder::default()
        .lib_dir(root.to_string_lossy())
        .load_env(args.config)
        .await?;
    if let Some(path) = args.lib_dir {
        let path = path
            .canonicalize()
            .with_context(|| format!("mudlib directory {}", path.display()))?;
        builder = builder.lib_dir(path.to_string_lossy());
    }
    Ok(Arc::new(builder.build()?))
}

pub fn run(connection: Connection, args: Args) -> Result<()> {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .thread_stack_size(THREAD_STACK)
        .enable_all()
        .build()?;
    let result = serve(&connection, args, &runtime);
    runtime.shutdown_timeout(Duration::from_secs(1));
    result
}

fn serve(connection: &Connection, args: Args, runtime: &tokio::runtime::Runtime) -> Result<()> {
    let (id, params) = connection.initialize_start()?;
    let initialized: Result<_> = (|| {
        let params: InitializeParams = serde_json::from_value(params)?;
        let config = runtime.block_on(configuration(args, &params))?;
        Ok(config)
    })();
    let config = match initialized {
        Ok(config) => config,
        Err(error) => {
            connection.sender.send(
                Response::new_err(id, ErrorCode::InvalidParams as i32, error.to_string()).into(),
            )?;
            return Err(error);
        }
    };
    connection.initialize_finish(
        id,
        serde_json::to_value(InitializeResult {
            capabilities: capabilities(),
            server_info: Some(ServerInfo {
                name: "lpc-rs-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })?,
    )?;

    let state = Arc::new(Mutex::new(State::default()));
    let (jobs, incoming) = flume::unbounded::<Check>();
    let worker_state = state.clone();
    let sender = connection.sender.clone();
    let worker = runtime.spawn(async move {
        while let Ok(mut job) = incoming.recv_async().await {
            while let Ok(newer) = incoming.try_recv() {
                job = newer;
            }
            if worker_state.lock().generation != job.generation {
                continue;
            }
            let generation = job.generation;
            let config = config.clone();
            let reports = match tokio::spawn(async move { check(&job, config).await }).await {
                Ok(reports) => reports,
                Err(error) => {
                    eprintln!("lpc-rs-lsp: compiler check failed: {error}");
                    continue;
                }
            };
            let mut state = worker_state.lock();
            for notification in state.publish(generation, reports) {
                if sender.send(notification.into()).is_err() {
                    return;
                }
            }
        }
    });

    let mut shutdown = false;
    for message in &connection.receiver {
        match message {
            Message::Request(request) if request.method == "shutdown" && !shutdown => {
                shutdown = true;
                state.lock().generation += 1;
                worker.abort();
                connection
                    .sender
                    .send(Response::new_ok(request.id, Value::Null).into())?;
            }
            Message::Request(request) => {
                let response = if shutdown {
                    Response::new_err(
                        request.id,
                        ErrorCode::InvalidRequest as i32,
                        "server is shutting down".into(),
                    )
                } else {
                    state.lock().request(request)
                };
                connection.sender.send(response.into())?;
            }
            Message::Notification(notification) if notification.method == "exit" => {
                worker.abort();
                if !shutdown {
                    bail!("exit received before shutdown");
                }
                return Ok(());
            }
            Message::Notification(notification) if !shutdown => {
                let mut state = state.lock();
                match state.update(notification) {
                    Ok((notifications, job)) => {
                        for notification in notifications {
                            connection.sender.send(notification.into())?;
                        }
                        if let Some(job) = job {
                            jobs.send(job)?;
                        }
                    }
                    Err(error) => eprintln!("lpc-rs-lsp: invalid notification: {error}"),
                }
            }
            _ => {}
        }
    }
    worker.abort();
    Ok(())
}

#[cfg(test)]
mod tests {
    use lsp_types::{Diagnostic, Position, Range};
    use serde_json::json;

    use super::*;

    #[test]
    fn protocol_initializes_checks_edits_answers_requests_and_shuts_down() {
        let root = tempfile::tempdir().unwrap();
        let uri = crate::documents::file_uri(&root.path().join("example.c")).unwrap();
        let (server, client) = Connection::memory();
        let path = root.path().to_owned();
        let thread = std::thread::spawn(move || {
            run(
                server,
                Args {
                    lib_dir: Some(path),
                    config: None,
                },
            )
        });
        let send = |message: Message| client.sender.send(message).unwrap();
        let receive = || {
            client
                .receiver
                .recv_timeout(Duration::from_secs(20))
                .unwrap()
        };
        send(Request::new(1.into(), "initialize".into(), json!({ "capabilities": {} })).into());
        let Message::Response(response) = receive() else {
            panic!("initialize response")
        };
        let value = response.response_result.unwrap();
        assert_eq!(value["capabilities"]["positionEncoding"], "utf-16");
        send(Notification::new("initialized".into(), json!({})).into());
        let text = "void create() { clone_object( }";
        send(
            Notification::new(
                "textDocument/didOpen".into(),
                json!({
                    "textDocument": { "uri": uri, "languageId": "lpc", "version": 1, "text": text }
                }),
            )
            .into(),
        );
        let Message::Notification(notification) = receive() else {
            panic!("diagnostics")
        };
        let diagnostics: PublishDiagnosticsParams =
            serde_json::from_value(notification.params).unwrap();
        assert_eq!(diagnostics.version, Some(1));
        assert!(!diagnostics.diagnostics.is_empty());

        for (id, method) in [(2, "textDocument/completion"), (3, "textDocument/hover")] {
            send(Request::new(id.into(), method.into(), json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 20 } })).into());
            let Message::Response(response) = receive() else {
                panic!("feature response")
            };
            assert!(
                response
                    .response_result
                    .unwrap()
                    .to_string()
                    .contains("clone_object")
            );
        }
        send(Notification::new("textDocument/didChange".into(), json!({
            "textDocument": { "uri": uri, "version": 2 }, "contentChanges": [{ "text": "void create() {}" }]
        })).into());
        let Message::Notification(clear) = receive() else {
            panic!("clear diagnostics")
        };
        assert_eq!(clear.params["diagnostics"], json!([]));
        send(
            Notification::new(
                "textDocument/didSave".into(),
                json!({ "textDocument": { "uri": uri } }),
            )
            .into(),
        );
        let Message::Notification(clean) = receive() else {
            panic!("clean diagnostics")
        };
        assert_eq!(clean.params["diagnostics"], json!([]));
        assert_eq!(clean.params["version"], 2);
        send(
            Notification::new(
                "textDocument/didClose".into(),
                json!({ "textDocument": { "uri": uri } }),
            )
            .into(),
        );
        let Message::Notification(closed) = receive() else {
            panic!("close diagnostics")
        };
        assert_eq!(closed.params["diagnostics"], json!([]));
        send(Request::new(4.into(), "shutdown".into(), Value::Null).into());
        assert!(matches!(receive(), Message::Response(_)));
        send(Notification::new("exit".into(), Value::Null).into());
        thread.join().unwrap().unwrap();
    }

    #[test]
    fn edits_clear_diagnostics_and_discard_in_flight_checks() {
        let mut state = State::default();
        let uri: Uri = "file:///mud/test.c".parse().unwrap();
        let (_, job) = state.update(Notification::new("textDocument/didOpen".into(), json!({
            "textDocument": { "uri": uri, "languageId": "lpc", "version": 1, "text": "int x = ;" }
        }))).unwrap();
        let job = job.unwrap();
        let reports = Reports::from([(
            uri.clone(),
            vec![Diagnostic::new_simple(
                Range::new(Position::new(0, 8), Position::new(0, 9)),
                "bad syntax".into(),
            )],
        )]);
        assert_eq!(state.publish(job.generation, reports.clone()).len(), 1);
        let (clears, next) = state.update(Notification::new("textDocument/didChange".into(), json!({
            "textDocument": { "uri": uri, "version": 2 }, "contentChanges": [{ "text": "int x = 1;" }]
        }))).unwrap();
        assert!(next.is_none());
        let clear: PublishDiagnosticsParams =
            serde_json::from_value(clears[0].params.clone()).unwrap();
        assert!(clear.diagnostics.is_empty());
        assert_eq!(clear.version, Some(2));
        assert!(state.publish(job.generation, reports).is_empty());
        let (_, job) = state
            .update(Notification::new(
                "textDocument/didSave".into(),
                json!({ "textDocument": { "uri": uri } }),
            ))
            .unwrap();
        assert_eq!(&*job.unwrap().documents[&uri].text, "int x = 1;");
    }
}
