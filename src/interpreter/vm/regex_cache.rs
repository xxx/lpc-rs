//! Bounded compiled regexes shared by a VM, independent of LPC values and transactions.

use std::sync::Arc;

use regex::{Regex, RegexBuilder};

use crate::{compile_time_config::REGEX_CACHE_CAPACITY, util::memo::Memo};

/// Compilation options only; matching offsets and output flags do not belong in the key.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub(crate) struct RegexOptions {
    pub case_insensitive: bool,
    pub multi_line: bool,
    pub dot_matches_new_line: bool,
    pub ignore_whitespace: bool,
    pub swap_greed: bool,
}

#[derive(Debug)]
pub(crate) struct RegexCache {
    entries: Memo<(String, RegexOptions), Arc<Regex>>,
}

impl Default for RegexCache {
    fn default() -> Self {
        Self::new(REGEX_CACHE_CAPACITY)
    }
}

impl RegexCache {
    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    fn new(capacity: usize) -> Self {
        Self {
            entries: Memo::new(capacity),
        }
    }

    /// Compile outside the cache lock; concurrent misses reuse the first resident success.
    pub(crate) fn get(
        &self,
        pattern: &str,
        options: RegexOptions,
    ) -> Result<Arc<Regex>, regex::Error> {
        self.entries
            .get_or_try_build(&(pattern.to_owned(), options), || {
                RegexBuilder::new(pattern)
                    .case_insensitive(options.case_insensitive)
                    .multi_line(options.multi_line)
                    .dot_matches_new_line(options.dot_matches_new_line)
                    .ignore_whitespace(options.ignore_whitespace)
                    .swap_greed(options.swap_greed)
                    .build()
                    .map(Arc::new)
            })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Barrier;

    use super::*;

    #[test]
    fn equal_pattern_text_reuses_the_compiled_regex() {
        let cache = RegexCache::new(2);
        let first = cache.get("猫+", RegexOptions::default()).unwrap();
        let separate_string = String::from("猫+");
        let second = cache
            .get(&separate_string, RegexOptions::default())
            .unwrap();
        assert!(Arc::ptr_eq(&first, &second));
        assert_eq!(second.find("é猫猫").unwrap().as_str(), "猫猫");
    }

    #[test]
    fn every_compilation_option_has_its_own_entry_and_behavior() {
        let cache = RegexCache::new(16);
        for (pattern, text, options, expected) in [
            (
                "a",
                "A",
                RegexOptions {
                    case_insensitive: true,
                    ..Default::default()
                },
                "A",
            ),
            (
                "^a",
                "b\na",
                RegexOptions {
                    multi_line: true,
                    ..Default::default()
                },
                "a",
            ),
            (
                "a.b",
                "a\nb",
                RegexOptions {
                    dot_matches_new_line: true,
                    ..Default::default()
                },
                "a\nb",
            ),
            (
                "a b",
                "ab",
                RegexOptions {
                    ignore_whitespace: true,
                    ..Default::default()
                },
                "ab",
            ),
            (
                "a+",
                "aaa",
                RegexOptions {
                    swap_greed: true,
                    ..Default::default()
                },
                "a",
            ),
        ] {
            let plain = cache.get(pattern, RegexOptions::default()).unwrap();
            let changed = cache.get(pattern, options).unwrap();
            assert!(!Arc::ptr_eq(&plain, &changed));
            assert_eq!(changed.find(text).unwrap().as_str(), expected);
            assert_ne!(plain.find(text).map(|m| m.as_str()), Some(expected));
            assert!(Arc::ptr_eq(
                &plain,
                &cache.get(pattern, RegexOptions::default()).unwrap()
            ));
            assert!(Arc::ptr_eq(&changed, &cache.get(pattern, options).unwrap()));
        }
    }

    #[test]
    fn eviction_recompiles_without_invalidating_active_matches() {
        let cache = RegexCache::new(2);
        let a = cache.get("a", RegexOptions::default()).unwrap();
        let b = cache.get("b", RegexOptions::default()).unwrap();
        cache.get("a", RegexOptions::default()).unwrap();
        cache.get("c", RegexOptions::default()).unwrap();
        assert!(Arc::ptr_eq(
            &a,
            &cache.get("a", RegexOptions::default()).unwrap()
        ));
        let rebuilt = cache.get("b", RegexOptions::default()).unwrap();
        assert!(!Arc::ptr_eq(&b, &rebuilt));
        assert!(b.is_match("b"));
        assert!(rebuilt.is_match("b"));
    }

    #[test]
    fn invalid_patterns_do_not_evict_successful_entries() {
        let cache = RegexCache::new(1);
        let valid = cache.get("a", RegexOptions::default()).unwrap();
        for _ in 0..2 {
            assert!(cache.get("(", RegexOptions::default()).is_err());
        }
        assert!(Arc::ptr_eq(
            &valid,
            &cache.get("a", RegexOptions::default()).unwrap()
        ));
    }

    #[test]
    fn concurrent_callers_share_the_resident_regex_and_match_independently() {
        let cache = RegexCache::new(2);
        let ready = Barrier::new(8);
        std::thread::scope(|scope| {
            let threads: Vec<_> = (0..8)
                .map(|i| {
                    let cache = &cache;
                    let ready = &ready;
                    scope.spawn(move || {
                        ready.wait();
                        let regex = cache.get(r"(猫)(\d+)", RegexOptions::default()).unwrap();
                        let text = format!("é猫{i}");
                        for _ in 0..20 {
                            let captures = regex.captures(&text).unwrap();
                            assert_eq!(&captures[1], "猫");
                            assert_eq!(&captures[2], i.to_string());
                        }
                        regex
                    })
                })
                .collect();
            let regexes: Vec<_> = threads
                .into_iter()
                .map(|thread| thread.join().unwrap())
                .collect();
            assert!(regexes.iter().all(|regex| Arc::ptr_eq(regex, &regexes[0])));
        });
    }
}
