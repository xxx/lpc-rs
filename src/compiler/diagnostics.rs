use lpc_rs_errors::LpcError;

/// Everything one compilation has reported so far, and the one rule for
/// turning it into a result.
#[derive(Debug, Default, Clone)]
pub struct Diagnostics {
    recorded: Vec<LpcError>,
}

impl Diagnostics {
    /// Record a diagnostic of any severity.
    pub fn record(&mut self, error: LpcError) {
        self.recorded.push(error);
    }

    /// Record an error the walker is about to return, and hand it back.
    pub fn fail(&mut self, error: LpcError) -> LpcError {
        self.record(error.clone());
        error
    }

    /// Everything recorded, in recording order.
    pub fn errors(&self) -> &[LpcError] {
        &self.recorded
    }

    #[cfg(test)]
    pub fn clear(&mut self) {
        self.recorded.clear();
    }

    /// No error or bug recorded; warnings do not count.
    pub fn is_clean(&self) -> bool {
        self.recorded.iter().all(LpcError::is_warning)
    }

    /// `Ok` with the warnings when nothing fatal was recorded; otherwise the
    /// first error or bug, carrying everything else in recording order.
    pub fn finish(&mut self) -> Result<Vec<LpcError>, LpcError> {
        let recorded = std::mem::take(&mut self.recorded);
        let Some(head) = recorded.iter().position(|e| !e.is_warning()) else {
            return Ok(recorded);
        };

        let mut rest = recorded;
        let head = rest.remove(head);
        Err(head.with_additional_errors(rest))
    }

    /// `finish` for a walk that returned `error`: recorded unless it is the
    /// diagnostic `fail` just recorded.
    pub fn finish_with(&mut self, error: LpcError) -> LpcError {
        let already = self
            .recorded
            .last()
            .is_some_and(|last| last.message() == error.message() && last.span() == error.span());
        if !already {
            self.record(error);
        }
        match self.finish() {
            Err(e) => e,
            Ok(warnings) => warnings
                .into_iter()
                .next()
                .expect("a walk that failed has recorded at least its own error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_errors::{lpc_bug, lpc_error, lpc_warning, span::Span};

    use super::*;

    #[test]
    fn only_warnings_finish_clean() {
        let mut d = Diagnostics::default();
        d.record(lpc_warning!("w1"));
        d.record(lpc_warning!("w2"));
        assert!(d.is_clean());
        let warnings = d.finish().unwrap();
        assert_eq!(warnings.len(), 2);
        assert_eq!(warnings[0].message(), "w1");
    }

    #[test]
    fn the_head_is_the_first_error_and_the_rest_keep_their_order() {
        let mut d = Diagnostics::default();
        d.record(lpc_warning!("w"));
        d.record(lpc_error!("e1"));
        d.record(lpc_error!("e2"));
        assert!(!d.is_clean());
        let e = d.finish().unwrap_err();
        assert_eq!(e.message(), "e1");
        let rendered: Vec<_> = e.to_diagnostics().into_iter().map(|x| x.message).collect();
        assert_eq!(rendered, vec!["e1", "w", "e2"]);
    }

    #[test]
    fn a_bug_is_fatal() {
        let mut d = Diagnostics::default();
        d.record(lpc_bug!("b"));
        assert!(!d.is_clean());
        assert!(d.finish().unwrap_err().is_bug());
    }

    #[test]
    fn a_failed_walk_records_its_error_once() {
        let span = Some(Span::new(0, 1..2));
        let mut d = Diagnostics::default();
        let returned = d.fail(lpc_error!(span, "boom"));
        let e = d.finish_with(returned);
        assert_eq!(e.to_diagnostics().len(), 1);

        let mut d = Diagnostics::default();
        d.record(lpc_warning!("w"));
        let e = d.finish_with(lpc_error!(span, "unseen"));
        let rendered: Vec<_> = e.to_diagnostics().into_iter().map(|x| x.message).collect();
        assert_eq!(rendered, vec!["unseen", "w"]);
    }
}
