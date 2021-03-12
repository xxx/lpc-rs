use crate::errors::lazy_files::FileId;

/// A struct to store state during lexing.
#[derive(Debug)]
pub struct LexState {
    // TODO: remove the allocation for last_slice
    pub last_slice: String,
    pub current_file_id: FileId,
    pub current_line: usize,
}

impl Default for LexState {
    fn default() -> Self {
        LexState {
            last_slice: String::from("\n"),
            current_file_id: 0,
            current_line: 0,
        }
    }
}