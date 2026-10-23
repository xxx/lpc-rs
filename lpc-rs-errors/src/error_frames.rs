//! Owned diagnostic text, independent of interpreter values and transactions.

/// Maximum retained frames, keeping those nearest the failure.
pub const MAX_ERROR_FRAMES: usize = 32;
/// Maximum arguments and locals retained together in one frame.
pub const MAX_ERROR_VARIABLES: usize = 32;
/// Maximum UTF-8 bytes in one rendered field, including a truncation marker.
pub const MAX_ERROR_TEXT_BYTES: usize = 256;
/// Maximum combined rendered field bytes in a snapshot.
pub const MAX_ERROR_FRAME_BYTES: usize = 65_536;

/// A named slot's rendered value at the time of failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorVariable {
    pub name: String,
    pub type_: String,
    pub value: String,
    pub truncated: bool,
}

/// One retained LPC frame; positions are one-based, or zero when unknown.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ErrorFrame {
    pub function: String,
    pub program: String,
    pub object: String,
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub arguments: Vec<ErrorVariable>,
    pub locals: Vec<ErrorVariable>,
    pub variables_omitted: usize,
    pub truncated: bool,
}

impl ErrorFrame {
    /// Bytes occupied by the rendered fields, excluding allocation overhead.
    pub fn text_bytes(&self) -> usize {
        self.function.len()
            + self.program.len()
            + self.object.len()
            + self.file.len()
            + self
                .arguments
                .iter()
                .chain(&self.locals)
                .map(|v| v.name.len() + v.type_.len() + v.value.len())
                .sum::<usize>()
    }
}

/// Root-to-leaf frames; omitted outer frames are counted separately.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ErrorFrames {
    pub frames: Vec<ErrorFrame>,
    pub omitted: usize,
}

impl ErrorFrames {
    /// Enforce the combined limits after attaching a calling task's frames.
    pub fn prepend(&mut self, mut outer: Self) {
        // Once inner frames are omitted, retaining callers would leave a gap.
        if self.omitted > 0 {
            self.omitted += outer.omitted + outer.frames.len();
            return;
        }
        outer.frames.append(&mut self.frames);
        self.frames = outer.frames;
        self.omitted += outer.omitted;
        let mut bytes = 0;
        let retained = self
            .frames
            .iter()
            .rev()
            .take(MAX_ERROR_FRAMES)
            .take_while(|frame| {
                bytes += frame.text_bytes();
                bytes <= MAX_ERROR_FRAME_BYTES
            })
            .count();
        let omitted = self.frames.len() - retained;
        self.frames.drain(..omitted);
        self.omitted += omitted;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_traces_keep_the_innermost_frames_in_order() {
        let frame = |n: usize| ErrorFrame {
            function: n.to_string(),
            ..Default::default()
        };
        let mut inner = ErrorFrames {
            frames: (30..40).map(frame).collect(),
            omitted: 0,
        };
        inner.prepend(ErrorFrames {
            frames: (0..30).map(frame).collect(),
            omitted: 3,
        });
        assert_eq!(inner.frames.len(), MAX_ERROR_FRAMES);
        assert_eq!(inner.frames.first().unwrap().function, "8");
        assert_eq!(inner.frames.last().unwrap().function, "39");
        assert_eq!(inner.omitted, 11);
    }

    #[test]
    fn omitted_inner_frames_keep_older_callers_out_of_the_snapshot() {
        let mut inner = ErrorFrames {
            frames: vec![ErrorFrame {
                function: "failure".into(),
                ..Default::default()
            }],
            omitted: 2,
        };
        inner.prepend(ErrorFrames {
            frames: vec![ErrorFrame::default()],
            omitted: 3,
        });
        assert_eq!(inner.frames.len(), 1);
        assert_eq!(inner.frames[0].function, "failure");
        assert_eq!(inner.omitted, 6);
    }

    #[test]
    fn merging_respects_the_combined_text_budget() {
        let large = ErrorFrame {
            function: "x".repeat(MAX_ERROR_FRAME_BYTES / 2),
            ..Default::default()
        };
        let mut inner = ErrorFrames {
            frames: vec![large.clone()],
            omitted: 0,
        };
        inner.prepend(ErrorFrames {
            frames: vec![large.clone(), large],
            omitted: 0,
        });
        assert_eq!(inner.frames.len(), 2);
        assert_eq!(inner.omitted, 1);
    }
}
