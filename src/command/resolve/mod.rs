//! The noun resolver: a captured phrase and a scope of candidates to the
//! candidates it names, through the `parse_command_*` identification
//! protocol of CD's `parse.c`.

#[cfg(test)]
pub(crate) mod fake;
mod lpc;
mod numeral;
mod phrase;
mod vocabulary;

use lpc_rs_errors::Result;

pub use lpc::LpcVocabulary;
pub use phrase::{Lists, Match, match_phrase};
pub use vocabulary::{Defaults, Lexicon, Vocabulary};

/// What kind of thing a capture names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// `%o`: the first candidate in scope order that matches.
    Object,
    /// `%l`: as `Items`, over the livings in scope.
    Living,
    /// `%i`: every matching candidate, with a numeral.
    Items,
    /// `%p`: one entry of the preposition list in force.
    Preposition,
    /// `%L`: the first living candidate that matches.
    Liv,
}

/// A resolved capture; a candidate is an index into the scope.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Resolved {
    /// The candidate `%o` named.
    Object(usize),
    /// The candidates `%i`/`%l` named; `numeral > 0` a count, `< 0` an
    /// ordinal, `0` the all word or a plural.
    Items {
        /// The count, ordinal, or 0.
        numeral: i64,
        /// Every matching candidate, in scope order.
        candidates: Vec<usize>,
    },
    /// The index of the matched entry in the preposition list in force.
    Preposition(usize),
}

/// Resolves phrases against one scope for one efun call or one dispatch;
/// each candidate is asked its lists at most once, and the master its
/// defaults only once the first phrase arrives.
pub struct Resolver<V: Vocabulary> {
    vocabulary: V,
    defaults: Defaults,
    fetched: bool,
    lexicons: Vec<Option<Lexicon>>,
    prepositions: Option<Vec<String>>,
}

impl<V: Vocabulary> Resolver<V> {
    /// Over `vocabulary`; `prepositions` is the caller's `%p` list, when it
    /// gave one. Nothing is asked of anyone yet.
    pub fn new(vocabulary: V, prepositions: Option<Vec<String>>) -> Self {
        let lexicons = (0..vocabulary.candidates()).map(|_| None).collect();
        Resolver {
            vocabulary,
            defaults: Defaults::default(),
            fetched: false,
            lexicons,
            prepositions,
        }
    }

    /// The vocabulary, for the scope behind the candidate indices.
    pub fn vocabulary(&self) -> &V {
        &self.vocabulary
    }

    /// The preposition list `%p` matches against: the caller's, else the
    /// master's.
    pub async fn prepositions(&mut self) -> Result<&[String]> {
        self.fetch_defaults().await?;
        Ok(self.in_force())
    }

    fn in_force(&self) -> &[String] {
        self.prepositions
            .as_deref()
            .unwrap_or(&self.defaults.prepositions)
    }

    async fn fetch_defaults(&mut self) -> Result<()> {
        if !self.fetched {
            self.defaults = self.vocabulary.defaults().await?;
            self.fetched = true;
        }
        Ok(())
    }

    /// What `phrase` names as a `kind`, or `None` when it names nothing.
    pub async fn resolve(&mut self, kind: Kind, phrase: &str) -> Result<Option<Resolved>> {
        self.fetch_defaults().await?;
        let words: Vec<&str> = phrase.split_whitespace().collect();
        match kind {
            Kind::Preposition => Ok(self.preposition(&words)),
            Kind::Object => self.object(&words).await,
            Kind::Items => self.items(&words, false).await,
            Kind::Living => self.items(&words, true).await,
            Kind::Liv => self.object_living(&words).await,
        }
    }

    fn preposition(&self, words: &[&str]) -> Option<Resolved> {
        self.in_force()
            .iter()
            .position(|entry| entry.split_whitespace().eq(words.iter().copied()))
            .map(Resolved::Preposition)
    }

    async fn object(&mut self, words: &[&str]) -> Result<Option<Resolved>> {
        for candidate in 0..self.lexicons.len() {
            if self.vocabulary.is_remote(candidate) {
                continue;
            }
            if self.matches(candidate, words, false).await?.is_some() {
                return Ok(Some(Resolved::Object(candidate)));
            }
        }
        Ok(None)
    }

    async fn object_living(&mut self, words: &[&str]) -> Result<Option<Resolved>> {
        for candidate in 0..self.lexicons.len() {
            if self.vocabulary.is_living(candidate)
                && self.matches(candidate, words, false).await?.is_some()
            {
                return Ok(Some(Resolved::Object(candidate)));
            }
        }
        Ok(None)
    }

    async fn items(&mut self, words: &[&str], living_only: bool) -> Result<Option<Resolved>> {
        let Some(&first) = words.first() else {
            return Ok(None);
        };
        let numeral = numeral::numeral(
            first,
            self.defaults.all_word.as_deref(),
            &mut self.vocabulary,
        )
        .await?;
        let rest = if numeral.is_some() {
            &words[1..]
        } else {
            words
        };
        let plural_expected = numeral.is_some_and(|n| n > 1 || n == 0);
        let match_all = numeral == Some(0);
        let mut candidates = Vec::new();
        let mut any_plural = false;
        for candidate in 0..self.lexicons.len() {
            if living_only && !self.vocabulary.is_living(candidate) {
                continue;
            }
            if !living_only && self.vocabulary.is_remote(candidate) {
                continue;
            }
            if let Some(plural) = self
                .named(candidate, rest, plural_expected, match_all)
                .await?
            {
                any_plural |= plural;
                candidates.push(candidate);
            }
        }
        if candidates.is_empty()
            && (!living_only
                || !self
                    .names_a_non_living(rest, plural_expected, match_all)
                    .await?)
        {
            return Ok(None);
        }
        let numeral = numeral.unwrap_or(if any_plural { 0 } else { 1 });
        Ok(Some(Resolved::Items {
            numeral,
            candidates,
        }))
    }

    /// Whether `candidate` is named: `Some(plural)` when it is. `rest`
    /// empty means the phrase was only a numeral, which names every live
    /// candidate under `match_all` and nothing otherwise.
    async fn named(
        &mut self,
        candidate: usize,
        rest: &[&str],
        plural_expected: bool,
        match_all: bool,
    ) -> Result<Option<bool>> {
        if rest.is_empty() {
            return Ok((match_all && self.vocabulary.is_live(candidate)).then_some(false));
        }
        Ok(self
            .matches(candidate, rest, plural_expected)
            .await?
            .map(|found| found.plural))
    }

    /// Whether a non-living, non-remote candidate is named; asked only once
    /// no living was, so a living phrase costs nothing extra when it resolves.
    async fn names_a_non_living(
        &mut self,
        rest: &[&str],
        plural_expected: bool,
        match_all: bool,
    ) -> Result<bool> {
        for candidate in 0..self.lexicons.len() {
            if self.vocabulary.is_living(candidate) || self.vocabulary.is_remote(candidate) {
                continue;
            }
            if self
                .named(candidate, rest, plural_expected, match_all)
                .await?
                .is_some()
            {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Whether `candidate` is named by `words`, fetching its lexicon on
    /// first use.
    async fn matches(
        &mut self,
        candidate: usize,
        words: &[&str],
        plural_expected: bool,
    ) -> Result<Option<Match>> {
        if words.is_empty() || !self.vocabulary.is_live(candidate) {
            return Ok(None);
        }
        let lexicon = match self.lexicons[candidate].take() {
            Some(lexicon) => lexicon,
            None => self.vocabulary.lexicon(candidate).await?,
        };
        let lexicon = self.lexicons[candidate].insert(lexicon);
        match lexicon {
            Lexicon::Lists(lists) => {
                Ok(match_phrase(words, lists, &self.defaults, plural_expected))
            }
            Lexicon::IdFunction => {
                let named = self.vocabulary.id(candidate, &words.join(" ")).await?;
                Ok(named.then_some(Match { plural: false }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{
        fake::{Fake, lists, scene, strings},
        *,
    };

    async fn resolve(fake: Fake, kind: Kind, phrase: &str) -> Option<Resolved> {
        Resolver::new(fake, None)
            .resolve(kind, phrase)
            .await
            .unwrap()
    }

    fn items(numeral: i64, candidates: &[usize]) -> Option<Resolved> {
        Some(Resolved::Items {
            numeral,
            candidates: candidates.to_vec(),
        })
    }

    #[tokio::test]
    async fn an_object_capture_takes_the_first_candidate_in_scope_order() {
        assert_eq!(
            resolve(scene(), Kind::Object, "sword").await,
            Some(Resolved::Object(0))
        );
        assert_eq!(
            resolve(scene(), Kind::Object, "red sword").await,
            Some(Resolved::Object(1))
        );
        assert_eq!(resolve(scene(), Kind::Object, "axe").await, None);
    }

    #[tokio::test]
    async fn an_object_capture_takes_no_numeral_but_accepts_a_plural_id() {
        assert_eq!(resolve(scene(), Kind::Object, "two swords").await, None);
        assert_eq!(
            resolve(scene(), Kind::Object, "swords").await,
            Some(Resolved::Object(0))
        );
    }

    #[tokio::test]
    async fn an_items_capture_collects_every_match_with_numeral_one() {
        assert_eq!(
            resolve(scene(), Kind::Items, "sword").await,
            items(1, &[0, 1])
        );
        assert_eq!(
            resolve(scene(), Kind::Items, "red sword").await,
            items(1, &[1])
        );
    }

    #[tokio::test]
    async fn a_plural_noun_reports_numeral_zero() {
        assert_eq!(
            resolve(scene(), Kind::Items, "swords").await,
            items(0, &[0, 1])
        );
    }

    #[tokio::test]
    async fn digits_are_a_count_and_expect_a_plural() {
        assert_eq!(
            resolve(scene(), Kind::Items, "2 swords").await,
            items(2, &[0, 1])
        );
        assert_eq!(resolve(scene(), Kind::Items, "2 sword").await, None);
        assert_eq!(
            resolve(scene(), Kind::Items, "1 sword").await,
            items(1, &[0, 1])
        );
    }

    #[tokio::test]
    async fn the_masters_numerals_pass_through() {
        assert_eq!(
            resolve(scene(), Kind::Items, "two swords").await,
            items(2, &[0, 1])
        );
        assert_eq!(
            resolve(scene(), Kind::Items, "second sword").await,
            items(-2, &[0, 1])
        );
    }

    #[tokio::test]
    async fn the_master_is_asked_only_for_words_that_are_neither_digits_nor_the_all_word() {
        let mut resolver = Resolver::new(scene(), None);
        resolver.resolve(Kind::Items, "3 swords").await.unwrap();
        resolver.resolve(Kind::Items, "all").await.unwrap();
        resolver.resolve(Kind::Items, "0 swords").await.unwrap();
        resolver.resolve(Kind::Items, "two swords").await.unwrap();
        assert_eq!(resolver.vocabulary().asked, vec!["two"]);
    }

    #[tokio::test]
    async fn zero_and_an_unknown_word_are_not_numerals() {
        assert_eq!(resolve(scene(), Kind::Items, "0 swords").await, None);
        assert_eq!(resolve(scene(), Kind::Items, "some swords").await, None);
    }

    #[tokio::test]
    async fn the_all_word_alone_names_every_live_candidate() {
        let mut fake = scene();
        fake.live[2] = false;
        assert_eq!(
            resolve(fake, Kind::Items, "all").await,
            items(0, &[0, 1, 3])
        );
    }

    #[tokio::test]
    async fn the_all_word_before_a_noun_names_the_matches() {
        assert_eq!(
            resolve(scene(), Kind::Items, "all swords").await,
            items(0, &[0, 1])
        );
    }

    #[tokio::test]
    async fn a_count_alone_names_nothing() {
        assert_eq!(resolve(scene(), Kind::Items, "two").await, None);
        assert_eq!(resolve(scene(), Kind::Items, "3").await, None);
    }

    #[tokio::test]
    async fn without_an_all_word_the_word_is_just_a_word() {
        let mut fake = scene();
        fake.defaults.all_word = None;
        assert_eq!(resolve(fake, Kind::Items, "all").await, None);
    }

    #[tokio::test]
    async fn living_filters_to_livings() {
        assert_eq!(
            resolve(scene(), Kind::Living, "guard").await,
            items(1, &[3])
        );
        assert_eq!(
            resolve(scene(), Kind::Living, "sword").await,
            Some(Resolved::Items {
                numeral: 1,
                candidates: vec![]
            })
        );
        assert_eq!(resolve(scene(), Kind::Living, "all").await, items(0, &[3]));
    }

    #[tokio::test]
    async fn a_living_phrase_naming_nothing_is_none() {
        assert_eq!(resolve(scene(), Kind::Living, "unicorn").await, None);
    }

    /// The guard (3), marked remote — a `parse_command_users()` living.
    fn remote_scene() -> Fake {
        let mut fake = scene();
        fake.remote[3] = true;
        fake
    }

    #[tokio::test]
    async fn a_remote_candidate_is_named_only_by_a_living_slot() {
        assert_eq!(
            resolve(remote_scene(), Kind::Living, "guard").await,
            items(1, &[3])
        );
        assert_eq!(
            resolve(remote_scene(), Kind::Liv, "guard").await,
            Some(Resolved::Object(3))
        );
        assert_eq!(resolve(remote_scene(), Kind::Items, "guard").await, None);
        assert_eq!(resolve(remote_scene(), Kind::Object, "guard").await, None);
    }

    #[tokio::test]
    async fn a_destructed_candidate_never_matches() {
        let mut fake = scene();
        fake.live[0] = false;
        assert_eq!(resolve(fake, Kind::Items, "sword").await, items(1, &[1]));
    }

    #[tokio::test]
    async fn the_id_function_is_asked_the_phrase_after_the_numeral() {
        let mut fake = Fake::new(vec![Lexicon::IdFunction]);
        fake.id_true = vec!["rock", "big rock"];
        fake.numerals = HashMap::from([("second", -2)]);
        assert_eq!(resolve(fake, Kind::Items, "big rock").await, items(1, &[0]));
        let mut fake = Fake::new(vec![Lexicon::IdFunction]);
        fake.id_true = vec!["rock"];
        fake.numerals = HashMap::from([("second", -2)]);
        assert_eq!(
            resolve(fake, Kind::Items, "second rock").await,
            items(-2, &[0])
        );
        let mut fake = Fake::new(vec![Lexicon::IdFunction]);
        fake.id_true = vec!["rock"];
        assert_eq!(resolve(fake, Kind::Items, "small rock").await, None);
    }

    #[tokio::test]
    async fn a_preposition_is_an_index_into_the_list_in_force() {
        assert_eq!(
            resolve(scene(), Kind::Preposition, "in").await,
            Some(Resolved::Preposition(0))
        );
        assert_eq!(
            resolve(scene(), Kind::Preposition, "in front of").await,
            Some(Resolved::Preposition(1))
        );
        assert_eq!(resolve(scene(), Kind::Preposition, "in front").await, None);
        assert_eq!(resolve(scene(), Kind::Preposition, "on").await, None);
    }

    #[tokio::test]
    async fn the_callers_preposition_list_replaces_the_masters() {
        let mut resolver = Resolver::new(scene(), Some(strings(&["on", "under"])));
        assert_eq!(
            resolver.resolve(Kind::Preposition, "under").await.unwrap(),
            Some(Resolved::Preposition(1))
        );
        assert_eq!(
            resolver.resolve(Kind::Preposition, "in").await.unwrap(),
            None
        );
        assert_eq!(
            resolver.prepositions().await.unwrap(),
            &strings(&["on", "under"])[..]
        );
    }

    #[tokio::test]
    async fn liv_takes_the_first_living_match() {
        let mut fake = Fake::new(vec![lists(&["bob"], &[], &[]), lists(&["bob"], &[], &[])]);
        fake.living = vec![false, true];
        let mut r = Resolver::new(fake, None);
        assert_eq!(
            r.resolve(Kind::Liv, "bob").await.unwrap(),
            Some(Resolved::Object(1))
        );
        assert_eq!(r.resolve(Kind::Liv, "nobody").await.unwrap(), None);
    }

    #[tokio::test]
    async fn each_candidate_is_asked_its_lexicon_once() {
        struct Counting(Fake, std::cell::Cell<usize>);
        impl Vocabulary for Counting {
            fn candidates(&self) -> usize {
                self.0.candidates()
            }
            fn is_live(&self, c: usize) -> bool {
                self.0.is_live(c)
            }
            fn is_living(&self, c: usize) -> bool {
                self.0.is_living(c)
            }
            async fn defaults(&mut self) -> Result<Defaults> {
                self.0.defaults().await
            }
            async fn numeral(&mut self, w: &str) -> Result<i64> {
                self.0.numeral(w).await
            }
            async fn lexicon(&mut self, c: usize) -> Result<Lexicon> {
                self.1.set(self.1.get() + 1);
                self.0.lexicon(c).await
            }
            async fn id(&mut self, c: usize, p: &str) -> Result<bool> {
                self.0.id(c, p).await
            }
        }
        let mut resolver = Resolver::new(Counting(scene(), std::cell::Cell::new(0)), None);
        resolver.resolve(Kind::Items, "sword").await.unwrap();
        resolver.resolve(Kind::Items, "bag").await.unwrap();
        resolver.resolve(Kind::Object, "guard").await.unwrap();
        assert_eq!(resolver.vocabulary().1.get(), 4);
    }

    #[tokio::test]
    async fn nothing_is_asked_of_the_master_until_the_first_phrase() {
        let mut r = Resolver::new(scene(), None);
        assert_eq!(r.vocabulary().defaults_asked, 0);
        r.resolve(Kind::Object, "sword").await.unwrap();
        assert_eq!(r.vocabulary().defaults_asked, 1);
        r.resolve(Kind::Items, "all").await.unwrap();
        assert_eq!(r.vocabulary().defaults_asked, 1);
    }

    #[tokio::test]
    async fn prepositions_force_the_masters_defaults_once() {
        let mut r = Resolver::new(scene(), None);
        assert_eq!(r.prepositions().await.unwrap(), &["in", "in front of"]);
        assert_eq!(r.vocabulary().defaults_asked, 1);
        r.prepositions().await.unwrap();
        assert_eq!(r.vocabulary().defaults_asked, 1);
    }
}
