//! Capture server logging without installing a global test subscriber.

use std::{
    io,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll},
};

use parking_lot::Mutex;
use tokio::io::AsyncWrite;
use tracing::Subscriber;
use tracing_subscriber::EnvFilter;

#[derive(Clone, Default)]
pub(crate) struct LogCapture(Arc<Mutex<Vec<u8>>>);

impl LogCapture {
    pub(crate) fn subscriber(&self, filter: &str) -> impl Subscriber + Send + Sync + 'static {
        let writer = self.clone();
        tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::new(filter))
            .with_writer(move || writer.clone())
            .with_ansi(false)
            .without_time()
            .finish()
    }

    pub(crate) fn contents(&self) -> String {
        String::from_utf8(self.0.lock().clone()).unwrap()
    }
}

impl io::Write for LogCapture {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.lock().extend_from_slice(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl AsyncWrite for LogCapture {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        bytes: &[u8],
    ) -> Poll<io::Result<usize>> {
        Poll::Ready(io::Write::write(self.get_mut(), bytes))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}
