use std::{borrow::Cow, fmt::Debug};

use tokio::{
    io::AsyncWriteExt,
    sync::{mpsc, oneshot},
};

pub enum LogOp {
    Message(Cow<'static, str>),
    Flush(oneshot::Sender<()>),
    Shutdown,
}

/// A struct to handle the simple requirements of the in-game debug log,
/// which is where unreceived `write`s go.
/// Dropping the sender closes the queue so its writer can drain and flush while the runtime runs.
pub struct DebugLog {
    handle: Option<tokio::task::JoinHandle<()>>,
    tx: mpsc::Sender<LogOp>,
}

impl DebugLog {
    const BUFLEN: u8 = 64;

    /// Create a new [`DebugLog`]. It will immediately start its own task.
    pub fn new<W>(mut writer: W) -> Self
    where
        W: tokio::io::AsyncWrite + Send + Sync + Unpin + 'static,
    {
        let (tx, mut rx) = mpsc::channel::<LogOp>(Self::BUFLEN as usize);

        let handle = tokio::spawn(async move {
            while let Some(op) = rx.recv().await {
                match op {
                    LogOp::Message(msg) => {
                        let _ = writer.write_all(msg.as_bytes()).await;
                    }
                    LogOp::Flush(done) => {
                        let _ = writer.flush().await;
                        let _ = done.send(());
                    }
                    LogOp::Shutdown => {
                        break;
                    }
                }
            }
            drop(rx);

            let _ = writer.flush().await;
        });

        Self {
            handle: Some(handle),
            tx,
        }
    }

    /// Create a new [`DebugLog`] that appends to the file at `path` (or STDOUT / STDERR).
    pub async fn from_str<P>(path: P) -> Self
    where
        P: AsRef<str>,
    {
        match path.as_ref() {
            "STDOUT" => Self::new(tokio::io::stdout()),
            "STDERR" => Self::new(tokio::io::stderr()),
            p => {
                if let Ok(file) = tokio::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(p)
                    .await
                {
                    Self::new(file)
                } else {
                    Self::new(tokio::io::stdout())
                }
            }
        }
    }

    /// Log a message to the debug log.
    pub async fn log<M>(&self, msg: M)
    where
        M: Into<Cow<'static, str>>,
    {
        let _ = self.tx.send(LogOp::Message(msg.into())).await;
    }

    /// Wait for previously queued messages to be written and flushed.
    pub async fn flush(&self) {
        let (done, flushed) = oneshot::channel();
        if self.tx.send(LogOp::Flush(done)).await.is_ok() {
            let _ = flushed.await;
        }
    }

    /// Shut down the task, and flush the log.
    /// Repeated calls are harmless, and a cancelled call can be awaited again.
    pub async fn shutdown(&mut self) {
        if let Some(handle) = self.handle.as_mut() {
            let _ = self.tx.send(LogOp::Shutdown).await;
            let _ = handle.await;
            self.handle = None;
        }
    }
}

impl Debug for DebugLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebugLog").finish()
    }
}

impl Default for DebugLog {
    fn default() -> Self {
        Self::new(tokio::io::stdout())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        io,
        pin::Pin,
        sync::{Arc, Mutex},
        task::{Context, Poll},
        time::Duration,
    };

    use tokio::io::{AsyncReadExt, AsyncWrite};

    use super::*;

    #[derive(Default)]
    struct Written {
        bytes: Vec<u8>,
        flushes: usize,
    }

    struct Probe(Arc<Mutex<Written>>);

    impl AsyncWrite for Probe {
        fn poll_write(
            self: Pin<&mut Self>,
            _: &mut Context<'_>,
            bytes: &[u8],
        ) -> Poll<io::Result<usize>> {
            self.0.lock().unwrap().bytes.extend_from_slice(bytes);
            Poll::Ready(Ok(bytes.len()))
        }

        fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
            self.0.lock().unwrap().flushes += 1;
            Poll::Ready(Ok(()))
        }

        fn poll_shutdown(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<io::Result<()>> {
            self.poll_flush(context)
        }
    }

    #[tokio::test]
    async fn flush_acknowledges_prior_messages_and_writer_flush() {
        let written = Arc::new(Mutex::new(Written::default()));
        let mut log = DebugLog::new(Probe(written.clone()));
        log.log("first").await;
        log.log("é".to_owned()).await;
        log.flush().await;

        {
            let written = written.lock().unwrap();
            assert_eq!(written.bytes, "firsté".as_bytes());
            assert_eq!(written.flushes, 1);
        }
        assert_eq!(format!("{log:?}"), "DebugLog");
        log.shutdown().await;
    }

    #[tokio::test]
    async fn shutdown_drains_messages_flushes_and_can_be_repeated() {
        let written = Arc::new(Mutex::new(Written::default()));
        let mut log = DebugLog::new(Probe(written.clone()));
        log.log("pending").await;
        log.shutdown().await;

        {
            let written = written.lock().unwrap();
            assert_eq!(written.bytes, b"pending");
            assert_eq!(written.flushes, 1);
        }
        log.shutdown().await;
    }

    #[tokio::test]
    async fn file_logs_append_and_flush_to_the_requested_path() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("debug.log");
        std::fs::write(&path, "first").unwrap();
        let mut log = DebugLog::from_str(path.to_str().unwrap()).await;
        log.log("second").await;
        log.flush().await;
        log.shutdown().await;

        assert_eq!(std::fs::read_to_string(path).unwrap(), "firstsecond");
    }

    #[test]
    fn dropping_outside_an_entered_runtime_still_drains_the_writer() {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let (writer, mut reader) = tokio::io::duplex(64);
        let log = runtime.block_on(async { DebugLog::new(writer) });
        runtime.block_on(log.log("pending"));

        drop(log);

        let mut bytes = Vec::new();
        runtime.block_on(async {
            tokio::time::timeout(Duration::from_secs(1), reader.read_to_end(&mut bytes))
                .await
                .unwrap()
                .unwrap();
        });
        assert_eq!(bytes, b"pending");
    }

    #[tokio::test]
    async fn cancelled_shutdown_can_be_awaited_again() {
        let (writer, mut reader) = tokio::io::duplex(1);
        let mut log = DebugLog::new(writer);
        log.log("pending").await;
        assert!(
            tokio::time::timeout(Duration::from_millis(1), log.shutdown())
                .await
                .is_err()
        );
        let received = tokio::spawn(async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await.unwrap();
            bytes
        });

        log.shutdown().await;

        assert_eq!(received.await.unwrap(), b"pending");
    }
}
