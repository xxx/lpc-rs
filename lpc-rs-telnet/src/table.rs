//! RFC 1143 negotiation, the Q method: a state per option per side and a
//! queued opposite request, so two sides asking at once never loop.

// Until session.rs lands (B5); it removes this.
#![allow(dead_code)]

use crate::opt::{DO, DONT, WILL, WONT};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum State {
    #[default]
    No,
    Yes,
    WantNoEmpty,
    WantNoOpposite,
    WantYesEmpty,
    WantYesOpposite,
}

/// The direction of a request or reply, before it is spelled as a command.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Answer {
    Enable,
    Disable,
}

/// A negotiation to send: `(command, option)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Reply(pub u8, pub u8);

/// Both sides' state for every option.
#[derive(Debug)]
pub(crate) struct Table {
    /// What we do (WILL/WONT from us, DO/DONT from him).
    us: [State; 256],
    /// What he does (DO/DONT from us, WILL/WONT from him).
    him: [State; 256],
}

/// He asked for the option: DO when the side is us, WILL when it is him.
fn recv_enable(state: State, agree: bool) -> (State, Option<Answer>) {
    match state {
        State::No if agree => (State::Yes, Some(Answer::Enable)),
        State::No => (State::No, Some(Answer::Disable)),
        State::Yes => (State::Yes, None),
        // He answered our disable with an enable: an error, settle on off.
        State::WantNoEmpty => (State::No, None),
        State::WantNoOpposite => (State::Yes, None),
        State::WantYesEmpty => (State::Yes, None),
        State::WantYesOpposite => (State::WantNoEmpty, Some(Answer::Disable)),
    }
}

/// He refused or withdrew the option: DONT for us, WONT for him.
fn recv_disable(state: State) -> (State, Option<Answer>) {
    match state {
        State::No => (State::No, None),
        State::Yes => (State::No, Some(Answer::Disable)),
        State::WantNoEmpty => (State::No, None),
        State::WantNoOpposite => (State::WantYesEmpty, Some(Answer::Enable)),
        State::WantYesEmpty => (State::No, None),
        State::WantYesOpposite => (State::No, None),
    }
}

/// We want the option on.
fn ask_enable(state: State) -> (State, Option<Answer>) {
    match state {
        State::No => (State::WantYesEmpty, Some(Answer::Enable)),
        State::Yes => (State::Yes, None),
        State::WantNoEmpty => (State::WantNoOpposite, None),
        State::WantNoOpposite => (State::WantNoOpposite, None),
        State::WantYesEmpty => (State::WantYesEmpty, None),
        State::WantYesOpposite => (State::WantYesEmpty, None),
    }
}

/// We want the option off.
fn ask_disable(state: State) -> (State, Option<Answer>) {
    match state {
        State::No => (State::No, None),
        State::Yes => (State::WantNoEmpty, Some(Answer::Disable)),
        State::WantNoEmpty => (State::WantNoEmpty, None),
        State::WantNoOpposite => (State::WantNoEmpty, None),
        State::WantYesEmpty => (State::WantYesOpposite, None),
        State::WantYesOpposite => (State::WantYesOpposite, None),
    }
}

impl Default for Table {
    fn default() -> Self {
        Self {
            us: [State::No; 256],
            him: [State::No; 256],
        }
    }
}

impl Table {
    /// Check if we have the option enabled.
    pub(crate) fn us_on(&self, opt: u8) -> bool {
        self.us[usize::from(opt)] == State::Yes
    }

    /// Check if he has the option enabled.
    pub(crate) fn him_on(&self, opt: u8) -> bool {
        self.him[usize::from(opt)] == State::Yes
    }

    /// Handle receiving DO from the client.
    pub(crate) fn recv_do(&mut self, opt: u8, agree: bool) -> Option<Reply> {
        let (next, answer) = recv_enable(self.us[usize::from(opt)], agree);
        self.us[usize::from(opt)] = next;
        answer.map(|a| Self::ours(a, opt))
    }

    /// Handle receiving DONT from the client.
    pub(crate) fn recv_dont(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = recv_disable(self.us[usize::from(opt)]);
        self.us[usize::from(opt)] = next;
        answer.map(|a| Self::ours(a, opt))
    }

    /// Handle receiving WILL from the client.
    pub(crate) fn recv_will(&mut self, opt: u8, agree: bool) -> Option<Reply> {
        let (next, answer) = recv_enable(self.him[usize::from(opt)], agree);
        self.him[usize::from(opt)] = next;
        answer.map(|a| Self::his(a, opt))
    }

    /// Handle receiving WONT from the client.
    pub(crate) fn recv_wont(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = recv_disable(self.him[usize::from(opt)]);
        self.him[usize::from(opt)] = next;
        answer.map(|a| Self::his(a, opt))
    }

    /// Ask the client to enable the option (we WILL).
    pub(crate) fn ask_will(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = ask_enable(self.us[usize::from(opt)]);
        self.us[usize::from(opt)] = next;
        answer.map(|a| Self::ours(a, opt))
    }

    /// Ask the client to disable the option (we WONT).
    pub(crate) fn ask_wont(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = ask_disable(self.us[usize::from(opt)]);
        self.us[usize::from(opt)] = next;
        answer.map(|a| Self::ours(a, opt))
    }

    /// Ask the client to enable the option (we DO).
    pub(crate) fn ask_do(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = ask_enable(self.him[usize::from(opt)]);
        self.him[usize::from(opt)] = next;
        answer.map(|a| Self::his(a, opt))
    }

    /// Ask the client to disable the option (we DONT).
    pub(crate) fn ask_dont(&mut self, opt: u8) -> Option<Reply> {
        let (next, answer) = ask_disable(self.him[usize::from(opt)]);
        self.him[usize::from(opt)] = next;
        answer.map(|a| Self::his(a, opt))
    }

    fn ours(answer: Answer, opt: u8) -> Reply {
        match answer {
            Answer::Enable => Reply(WILL, opt),
            Answer::Disable => Reply(WONT, opt),
        }
    }

    fn his(answer: Answer, opt: u8) -> Reply {
        match answer {
            Answer::Enable => Reply(DO, opt),
            Answer::Disable => Reply(DONT, opt),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const GMCP: u8 = 201;

    #[test]
    fn do_when_off_and_agreed_enables_with_will() {
        let mut t = Table::default();
        assert_eq!(t.recv_do(GMCP, true), Some(Reply(WILL, GMCP)));
        assert!(t.us_on(GMCP));
    }

    #[test]
    fn do_when_off_and_refused_answers_wont() {
        let mut t = Table::default();
        assert_eq!(t.recv_do(GMCP, false), Some(Reply(WONT, GMCP)));
        assert!(!t.us_on(GMCP));
    }

    #[test]
    fn do_while_on_is_silent() {
        let mut t = Table::default();
        t.recv_do(GMCP, true);
        assert_eq!(t.recv_do(GMCP, true), None);
    }

    #[test]
    fn dont_while_on_disables_with_wont() {
        let mut t = Table::default();
        t.recv_do(GMCP, true);
        assert_eq!(t.recv_dont(GMCP), Some(Reply(WONT, GMCP)));
        assert!(!t.us_on(GMCP));
    }

    #[test]
    fn our_will_then_his_do_enables_silently_even_when_unsolicited_would_be_refused() {
        let mut t = Table::default();
        assert_eq!(t.ask_will(GMCP), Some(Reply(WILL, GMCP)));
        assert!(!t.us_on(GMCP), "not on until he agrees");
        assert_eq!(t.recv_do(GMCP, false), None);
        assert!(t.us_on(GMCP));
    }

    #[test]
    fn our_will_then_his_dont_lands_on_off() {
        let mut t = Table::default();
        t.ask_will(GMCP);
        assert_eq!(t.recv_dont(GMCP), None);
        assert!(!t.us_on(GMCP));
    }

    #[test]
    fn asking_twice_sends_once() {
        let mut t = Table::default();
        assert_eq!(t.ask_will(GMCP), Some(Reply(WILL, GMCP)));
        assert_eq!(t.ask_will(GMCP), None);
    }

    #[test]
    fn disabling_while_enabling_queues_the_opposite() {
        let mut t = Table::default();
        t.ask_will(GMCP);
        assert_eq!(t.ask_wont(GMCP), None, "queued behind the pending WILL");
        assert_eq!(t.recv_do(GMCP, true), Some(Reply(WONT, GMCP)));
        assert!(!t.us_on(GMCP));
        assert_eq!(t.recv_dont(GMCP), None);
    }

    #[test]
    fn enabling_while_disabling_queues_the_opposite() {
        let mut t = Table::default();
        t.recv_do(GMCP, true);
        assert_eq!(t.ask_wont(GMCP), Some(Reply(WONT, GMCP)));
        assert_eq!(t.ask_will(GMCP), None);
        assert_eq!(t.recv_dont(GMCP), Some(Reply(WILL, GMCP)));
        assert_eq!(t.recv_do(GMCP, false), None);
        assert!(t.us_on(GMCP));
    }

    #[test]
    fn his_side_mirrors_ours() {
        const NAWS: u8 = 31;
        let mut t = Table::default();
        assert_eq!(t.ask_do(NAWS), Some(Reply(DO, NAWS)));
        assert_eq!(t.recv_will(NAWS, false), None);
        assert!(t.him_on(NAWS));
        assert_eq!(t.recv_wont(NAWS), Some(Reply(DONT, NAWS)));
        assert!(!t.him_on(NAWS));
        assert_eq!(t.recv_will(NAWS, true), Some(Reply(DO, NAWS)));
        assert_eq!(t.ask_dont(NAWS), Some(Reply(DONT, NAWS)));
    }

    #[test]
    fn an_unsolicited_will_for_a_refused_option_answers_dont() {
        let mut t = Table::default();
        assert_eq!(t.recv_will(1, false), Some(Reply(DONT, 1)));
        assert!(!t.him_on(1));
    }

    #[test]
    fn sides_are_independent() {
        let mut t = Table::default();
        t.recv_do(GMCP, true);
        assert!(t.us_on(GMCP));
        assert!(!t.him_on(GMCP));
    }
}
