use std::fs::File;
use std::io::Write;
use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};

/// A newtype so we can implement WriteColor and emit codespan diagnostics to files.
pub struct FileStream {
    file: File,
}

impl FileStream {
    pub fn new(file: File) -> Self {
        Self { file }
    }
}

impl Write for FileStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.file.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.file.flush()
    }
}

impl WriteColor for FileStream {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _spec: &ColorSpec) -> std::io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}