use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `wildmatch(pattern, subject)`: 1 when the whole subject matches the
/// shell-style pattern (`*`, `?`, `[set]`, `[^set]`, `\` escape), else 0;
/// a non-string subject is 0.
pub fn wildmatch<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(pattern) = context.arg(0).as_str() else {
        return Err(context.runtime_error("wildmatch: the pattern must be a string"));
    };
    let matched = context
        .arg(1)
        .as_str()
        .is_some_and(|subject| matches(pattern, subject));
    context.return_efun_result(LpcRef::from(matched));
    Ok(())
}

/// Whether `subject` matches `pattern` from its first character to its
/// last.
pub(crate) fn matches(pattern: &str, subject: &str) -> bool {
    let pattern: Vec<char> = pattern.chars().collect();
    let subject: Vec<char> = subject.chars().collect();
    let (mut p, mut s) = (&pattern[..], &subject[..]);
    // The last star's tail and the subject after the star: a later
    // mismatch retries with the star taking one more character.
    let mut star: Option<(&[char], &[char])> = None;
    loop {
        let advanced = match element(p) {
            Some((Element::Star, rest)) => {
                star = Some((rest, s));
                p = rest;
                continue;
            }
            Some((element, rest)) => match s.split_first() {
                Some((&c, tail)) if element.takes(c) => {
                    p = rest;
                    s = tail;
                    true
                }
                _ => false,
            },
            None => {
                if p.is_empty() && s.is_empty() {
                    return true;
                }
                false
            }
        };
        if advanced {
            continue;
        }
        match star {
            Some((rest, since)) if !since.is_empty() => {
                p = rest;
                s = &since[1..];
                star = Some((rest, s));
            }
            _ => return false,
        }
    }
}

/// One element of a pattern.
enum Element {
    /// A character, escaped or not.
    Char(char),
    /// `?`
    Any,
    /// `*`
    Star,
    /// `[...]`: single characters and inclusive ranges, matched or
    /// excluded.
    Set {
        negated: bool,
        members: Vec<(char, char)>,
    },
}

impl Element {
    /// Whether this one-character element takes `c`; a star is never
    /// asked.
    fn takes(&self, c: char) -> bool {
        match self {
            Element::Char(literal) => *literal == c,
            Element::Any => true,
            Element::Set { negated, members } => {
                members.iter().any(|&(lo, hi)| (lo..=hi).contains(&c)) != *negated
            }
            Element::Star => false,
        }
    }
}

/// The element at the front of `pattern` and the pattern after it; `None`
/// when the pattern is empty or malformed (a trailing `\`, an unclosed
/// set).
fn element(pattern: &[char]) -> Option<(Element, &[char])> {
    match pattern {
        [] | ['\\'] => None,
        ['\\', c, rest @ ..] => Some((Element::Char(*c), rest)),
        ['?', rest @ ..] => Some((Element::Any, rest)),
        ['*', rest @ ..] => Some((Element::Star, rest)),
        ['[', rest @ ..] => set(rest),
        [c, rest @ ..] => Some((Element::Char(*c), rest)),
    }
}

/// The set `after_bracket` opens and the pattern after its `]`: a `^`
/// first negates, a `]` or `-` first is a member, `a-z` is an inclusive
/// range, a `-` before the `]` is a member.
fn set(after_bracket: &[char]) -> Option<(Element, &[char])> {
    let (negated, mut rest) = match after_bracket {
        ['^', rest @ ..] => (true, rest),
        rest => (false, rest),
    };
    let mut members = Vec::new();
    if let [c @ (']' | '-'), tail @ ..] = rest {
        members.push((*c, *c));
        rest = tail;
    }
    loop {
        match rest {
            [] => return None,
            [']', tail @ ..] => return Some((Element::Set { negated, members }, tail)),
            [lo, '-', hi, tail @ ..] if *hi != ']' => {
                members.push((*lo, *hi));
                rest = tail;
            }
            [c, tail @ ..] => {
                members.push((*c, *c));
                rest = tail;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::matches;
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[test]
    fn the_manual_examples() {
        assert!(matches("*.foo", "bar.foo"));
        assert!(!matches("*.foo", "foo.bar"));
        assert!(matches("[abc]?*", "axy"));
        assert!(!matches("[abc]?*", "dxy"));
        assert!(!matches("[abc]?*", "a"));
    }

    #[test]
    fn the_whole_subject_must_match() {
        assert!(matches("", ""));
        assert!(!matches("", "a"));
        assert!(!matches("a", "ab"));
        assert!(matches("to *", "to x"));
        assert!(!matches("to *", "to"));
        assert!(!matches("?", ""));
    }

    #[test]
    fn a_star_takes_any_run() {
        assert!(matches("*", ""));
        assert!(matches("a*", "a"));
        assert!(matches("**a", "xxa"));
        assert!(matches("*@@*", "x@@y"));
        assert!(matches("*a*b", "xaxb"));
    }

    #[test]
    fn a_failing_star_pattern_backtracks_without_blowing_up() {
        let subject = "a".repeat(200) + "c";
        assert!(!matches("*a*a*a*a*a*b", &subject));
    }

    #[test]
    fn sets_ranges_and_negation() {
        assert!(matches("[at][to] *", "to foo"));
        assert!(matches("[^abc]x", "dx"));
        assert!(!matches("[^abc]x", "ax"));
        assert!(matches("[]a]", "]"));
        assert!(matches("[-a]", "-"));
        assert!(matches("[a-]", "-"));
        assert!(matches("[a-z]", "m"));
        assert!(!matches("[a-z]", "M"));
        assert!(!matches("[abc]", ""));
    }

    #[test]
    fn a_backslash_escapes_the_next_character() {
        assert!(matches("\\*", "*"));
        assert!(!matches("\\*", "a"));
        assert!(matches("\\[a]", "[a]"));
    }

    #[test]
    fn a_malformed_pattern_matches_nothing() {
        assert!(!matches("a\\", "a"));
        assert!(!matches("a\\", "a\\"));
        assert!(!matches("[ab", "a"));
        assert!(!matches("[ab", "[ab"));
    }

    #[test]
    fn characters_not_bytes() {
        assert!(matches("h?llo", "héllo"));
        assert!(!matches("h??llo", "héllo"));
    }

    #[tokio::test]
    async fn the_efun_answers_one_or_zero() {
        let result = run_prog(r#"int create() { return wildmatch("*.foo", "bar.foo") * 10 + wildmatch("*.foo", "foo.bar"); }"#)
            .await
            .result();
        assert_eq!(result, Some(LpcRef::from(10)));
    }

    #[tokio::test]
    async fn a_non_string_subject_is_zero() {
        let result = run_prog(r#"int create() { mixed s = 0; return wildmatch("*", s); }"#)
            .await
            .result();
        assert_eq!(result, Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn a_non_string_pattern_is_an_error() {
        let err = try_run_prog(r#"int create() { mixed p = 0; return wildmatch(p, "x"); }"#)
            .await
            .expect_err("0 is not a pattern")
            .to_string();
        assert!(
            err.contains("wildmatch: the pattern must be a string"),
            "{err}"
        );
    }
}
