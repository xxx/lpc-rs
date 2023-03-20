use std::{fs::File, io::Write};

use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};

/// A newtype so we can implement WriteColor and emit [`codespan`](codespan_reporting)
/// diagnostics to files.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_stream() {
        use std::io::Write;

        let mut file = FileStream::new(std::fs::File::create("test.txt").unwrap());
        file.write_all(b"Hello, world!").unwrap();
        file.flush().unwrap();

        assert!(!file.supports_color());
        assert!(file.set_color(&ColorSpec::new()).is_ok());
        assert!(file.reset().is_ok());

        std::fs::remove_file("test.txt").unwrap();
    }

}