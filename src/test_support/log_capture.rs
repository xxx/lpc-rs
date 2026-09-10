//! Capture server logging without installing a global test subscriber.

use std::{io, sync::Arc};

use parking_lot::Mutex;
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
