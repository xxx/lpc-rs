//! The conditional stack: the one owner of conditional-compilation state.
//! A frame per open conditional; `live()` is the only question the
//! directive handlers ask. Balanced per file — the include boundary swaps
//! the stack.

use lpc_rs_errors::{LpcError, Result, lpc_error, span::Span};

/// One open `#if`/`#ifdef`/`#ifndef`.
#[derive(Debug)]
struct Frame {
    /// The span of this frame's opening directive, reported by `finish()` if never closed.
    span: Span,
    /// Was the surrounding region live when this frame opened?
    parent_live: bool,
    /// Is this frame's current branch emitting? Flipped by `#else`.
    branch_live: bool,
    /// The span of this frame's `#else`, once seen.
    else_seen: Option<Span>,
}

/// The stack of open conditionals for one file's scan.
#[derive(Debug, Default)]
pub(super) struct Conditionals {
    frames: Vec<Frame>,
}

impl Conditionals {
    /// Open a conditional. When the stack is not live, `taken` is ignored
    /// in effect (the frame is inert) — a dead `#if`'s operand is never
    /// evaluated by its handler.
    pub(super) fn enter(&mut self, span: Span, taken: bool) {
        let parent_live = self.live();
        self.frames.push(Frame {
            span,
            parent_live,
            branch_live: taken,
            else_seen: None,
        });
    }

    /// `#else`: record it, and flip the branch iff the parent region is
    /// live. Duplicate and orphan `#else` are errors, live or dead.
    pub(super) fn flip_else(&mut self, span: Span) -> Result<()> {
        let Some(frame) = self.frames.last_mut() else {
            return Err(lpc_error!(
                Some(span),
                "found `#else` without a corresponding `#if` or `#ifdef`",
            ));
        };
        if let Some(first) = frame.else_seen {
            let err = LpcError::new("duplicate `#else` found")
                .with_span(Some(span))
                .with_label("first used here", Some(first));
            return Err(err);
        }
        frame.else_seen = Some(span);
        if frame.parent_live {
            frame.branch_live = !frame.branch_live;
        }
        Ok(())
    }

    /// `#endif`. Orphans are errors, live or dead.
    pub(super) fn leave(&mut self, span: Span) -> Result<()> {
        if self.frames.pop().is_none() {
            return Err(lpc_error!(
                Some(span),
                "found `#endif` without a corresponding `#if`"
            ));
        }
        Ok(())
    }

    /// Is the current region emitting?
    pub(super) fn live(&self) -> bool {
        self.frames
            .last()
            .is_none_or(|f| f.parent_live && f.branch_live)
    }

    /// Per-file balance: any open frame is a missing `#endif`, reported at
    /// the innermost unclosed frame.
    pub(super) fn finish(&self) -> Result<()> {
        if let Some(frame) = self.frames.last() {
            return Err(lpc_error!(
                Some(frame.span),
                "Found `#if` without a corresponding `#endif`"
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sp(l: usize) -> Span {
        Span::new(0, l..l + 1)
    }

    #[test]
    fn empty_stack_is_live() {
        assert!(Conditionals::default().live());
    }

    #[test]
    fn taken_is_live_and_untaken_is_dead() {
        let mut c = Conditionals::default();
        c.enter(sp(1), true);
        assert!(c.live());
        c.enter(sp(2), false);
        assert!(!c.live());
        c.leave(sp(3)).unwrap();
        assert!(c.live());
    }

    #[test]
    fn else_flips_a_live_parent() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false);
        assert!(!c.live());
        c.flip_else(sp(2)).unwrap();
        assert!(c.live());
    }

    #[test]
    fn else_records_but_does_not_flip_in_a_dead_parent() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false); // dead from here
        c.enter(sp(2), true); // inert: parent_live = false
        assert!(!c.live());
        c.flip_else(sp(3)).unwrap();
        assert!(!c.live()); // no flip — but a second #else still errors
        let e = c.flip_else(sp(4)).unwrap_err();
        assert!(e.message().contains("duplicate `#else`"));
    }

    #[test]
    fn nested_frame_has_its_own_else() {
        let mut c = Conditionals::default();
        c.enter(sp(1), true);
        c.flip_else(sp(2)).unwrap(); // outer else
        c.enter(sp(3), true);
        c.leave(sp(4)).unwrap(); // nested #if/#endif must not forget it
        let e = c.flip_else(sp(5)).unwrap_err();
        assert!(e.message().contains("duplicate `#else`"));
    }

    #[test]
    fn orphan_else_and_endif_error() {
        let mut c = Conditionals::default();
        assert!(c.flip_else(sp(1)).is_err());
        assert!(c.leave(sp(2)).is_err());
    }

    #[test]
    fn finish_reports_the_innermost_open_frame() {
        let mut c = Conditionals::default();
        c.enter(sp(1), true);
        c.enter(sp(9), true);
        let e = c.finish().unwrap_err();
        assert!(e.message().contains("without a corresponding `#endif`"));
        assert_eq!(e.span(), Some(sp(9)));
    }

    #[test]
    fn balanced_stack_finishes_clean() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false);
        c.leave(sp(2)).unwrap();
        assert!(c.finish().is_ok());
    }
}
