use lpc_rs_errors::source_map::FileId;

/// A struct to store state during lexing.
#[derive(Debug)]
pub struct LexState {
    pub last_slice: String,
    pub current_file_id: FileId,
    /// Byte offset of the lexed text within `current_file_id`'s source —
    /// nonzero when lexing a fragment (a `#define` body, a `#if`
    /// operand), so spans are born in file coordinates.
    pub base_offset: usize,
}
