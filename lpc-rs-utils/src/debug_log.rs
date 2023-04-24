use std::{borrow::Cow, fmt::Debug};

use tokio::{io::AsyncWriteExt, sync::mpsc};

pub enum LogOp {
    Message(Cow<'static, str>),
    Shutdown,
}

/// A struct to handle the simple requirements of the in-game debug log,
/// which is where unreceived `write`s go.
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

    /// Create a new [`DebugLog`] that appends to the file at <path> (or STDOUT / STDERR).
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

    /// Shut down the task, and flush the log.
    pub async fn shutdown(&mut self) {
        let _ = self.tx.send(LogOp::Shutdown).await;
        let handle = self.handle.take().unwrap();
        let _ = handle.await;
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

impl Drop for DebugLog {
    fn drop(&mut self) {
        let tx = self.tx.clone();
        let handle = self.handle.take();

        if handle.is_some() {
            tokio::spawn(async move {
                let _ = tx.send(LogOp::Shutdown).await;
                let _ = handle.unwrap().await;
            });
        }
    }
}
