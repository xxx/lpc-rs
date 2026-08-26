//! Where the resolver's words come from: the master's defaults and each
//! candidate's own lists, behind a trait so the algorithm is tested without
//! LPC.

use lpc_rs_errors::Result;

use super::phrase::Lists;

/// The master's shared vocabulary; every list may be empty and the all word
/// absent.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Defaults {
    /// Singular ids any object answers to (`"it"`, `"thing"`).
    pub ids: Vec<String>,
    /// Plural ids any object answers to (`"them"`).
    pub plurals: Vec<String>,
    /// Adjectives any object accepts (`"that"`).
    pub adjectives: Vec<String>,
    /// The preposition list `%p` uses when the caller gives none.
    pub prepositions: Vec<String>,
    /// The word meaning every candidate, when the master defines one.
    pub all_word: Option<String>,
}

/// What one candidate answers about itself.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lexicon {
    /// Its `parse_command_*_list()` lists.
    Lists(Lists),
    /// It defines no `parse_command_id_list`; it is asked `id()` per phrase.
    IdFunction,
}

/// The source of words for one scope: the LPC-backed implementation applies
/// `parse_command_*` and `id`; tests use an in-memory one.
pub trait Vocabulary {
    /// How many candidates the scope holds.
    fn candidates(&self) -> usize;
    /// Whether `candidate` is not destructed.
    fn is_live(&self, candidate: usize) -> bool;
    /// Whether `candidate` has commands enabled.
    fn is_living(&self, candidate: usize) -> bool;
    /// The master's lists and all word.
    async fn defaults(&mut self) -> Result<Defaults>;
    /// `master->parse_command_numeral(word)`: `> 0` a count, `< 0` an
    /// ordinal, `0` not a numeral.
    async fn numeral(&mut self, word: &str) -> Result<i64>;
    /// `candidate`'s lists, its plurals derived through the master when it
    /// supplies none.
    async fn lexicon(&mut self, candidate: usize) -> Result<Lexicon>;
    /// `candidate->id(phrase)`.
    async fn id(&mut self, candidate: usize, phrase: &str) -> Result<bool>;
}
