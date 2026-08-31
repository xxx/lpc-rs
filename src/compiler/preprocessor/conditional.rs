//! The conditional stack: the one owner of conditional-compilation state.
//! A frame per open conditional chain (an `#if`/`#ifdef`/`#ifndef` through
//! its `#elif`s and `#else` to its `#endif`); `live()` is the only
//! question the directive handlers ask. Balanced per file — the include
//! boundary swaps the stack.

use lpc_rs_errors::{LpcError, Result, lpc_error, span::Span};

/// One open `#if`/`#ifdef`/`#ifndef`.
#[derive(Debug)]
struct Frame {
    /// The span of this frame's opening directive, reported by `finish()` if never closed.
    span: Span,
    /// Was the surrounding region live when this frame opened?
    parent_live: bool,
    /// Is this frame's current branch emitting?
    branch_live: bool,
    /// Has any branch of this frame emitted? `#elif` and `#else` consult it.
    taken_any: bool,
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
            taken_any: taken,
            else_seen: None,
        });
    }

    /// `#elif`: end the previous branch and say whether this operand
    /// decides the chain — the parent is live and nothing is taken yet
    /// (C99 6.10p6: otherwise the operand is never read). Orphan and
    /// after-`#else` are errors, live or dead.
    pub(super) fn elif(&mut self, span: Span) -> Result<bool> {
        let Some(frame) = self.frames.last_mut() else {
            return Err(lpc_error!(
                Some(span),
                "found `#elif` without a corresponding `#if` or `#ifdef`",
            ));
        };
        if let Some(else_span) = frame.else_seen {
            let err = LpcError::new("found `#elif` after `#else`")
                .with_span(Some(span))
                .with_label("`#else` is here", Some(else_span));
            return Err(err);
        }
        if frame.parent_live {
            frame.branch_live = false;
        }
        Ok(frame.parent_live && !frame.taken_any)
    }

    /// Record an armed `#elif`'s verdict. Call only when [`elif`](Self::elif)
    /// returned `true` — otherwise the branch is already dead and stays dead.
    pub(super) fn take_elif(&mut self, taken: bool) {
        let frame = self.frames.last_mut().expect("elif() found this frame");
        frame.branch_live = taken;
        frame.taken_any |= taken;
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
            // `!branch_live` would wrongly take the else after a taken
            // `#elif`.
            frame.branch_live = !frame.taken_any;
            frame.taken_any = true;
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
        assert_eq!(e.to_string(), "duplicate `#else` found");
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
        let else_err = c.flip_else(sp(1)).unwrap_err();
        assert_eq!(
            else_err.to_string(),
            "found `#else` without a corresponding `#if` or `#ifdef`"
        );
        let endif_err = c.leave(sp(2)).unwrap_err();
        assert_eq!(
            endif_err.to_string(),
            "found `#endif` without a corresponding `#if`"
        );
    }

    #[test]
    fn finish_reports_the_innermost_open_frame() {
        let mut c = Conditionals::default();
        c.enter(sp(1), true);
        c.enter(sp(9), true);
        let e = c.finish().unwrap_err();
        assert_eq!(
            e.to_string(),
            "Found `#if` without a corresponding `#endif`"
        );
        assert_eq!(e.span(), Some(sp(9)));
    }

    #[test]
    fn balanced_stack_finishes_clean() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false);
        c.leave(sp(2)).unwrap();
        assert!(c.finish().is_ok());
    }

    #[test]
    fn an_elif_chain_takes_the_first_true_branch() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false); // #if 0
        assert!(c.elif(sp(2)).unwrap()); // undecided: operand decides
        c.take_elif(true);
        assert!(c.live());
        assert!(!c.elif(sp(3)).unwrap()); // decided: never evaluated
        assert!(!c.live());
        c.flip_else(sp(4)).unwrap();
        assert!(!c.live()); // something was taken — else stays dead
        c.leave(sp(5)).unwrap();
    }

    #[test]
    fn an_all_false_chain_takes_the_else() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false);
        assert!(c.elif(sp(2)).unwrap());
        c.take_elif(false);
        assert!(c.elif(sp(3)).unwrap()); // still undecided
        c.take_elif(false);
        c.flip_else(sp(4)).unwrap();
        assert!(c.live());
    }

    #[test]
    fn an_elif_after_a_taken_if_ends_the_branch_unread() {
        let mut c = Conditionals::default();
        c.enter(sp(1), true);
        assert!(c.live());
        assert!(!c.elif(sp(2)).unwrap());
        assert!(!c.live());
        c.flip_else(sp(3)).unwrap();
        assert!(!c.live()); // the flip_else fix: !taken_any, not !branch_live
    }

    #[test]
    fn a_dead_parents_elif_validates_but_never_decides() {
        let mut c = Conditionals::default();
        c.enter(sp(1), false); // dead from here
        c.enter(sp(2), true); // inert frame
        assert!(!c.elif(sp(3)).unwrap());
        c.flip_else(sp(4)).unwrap();
        let e = c.elif(sp(5)).unwrap_err();
        assert_eq!(e.to_string(), "found `#elif` after `#else`");
    }

    #[test]
    fn orphan_elif_errors() {
        let mut c = Conditionals::default();
        let e = c.elif(sp(1)).unwrap_err();
        assert_eq!(
            e.to_string(),
            "found `#elif` without a corresponding `#if` or `#ifdef`"
        );
    }
}
