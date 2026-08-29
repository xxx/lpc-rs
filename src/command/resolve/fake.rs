//! In-memory vocabularies for tests: the master's table and each candidate's
//! lists, with no LPC.

use std::collections::HashMap;

use lpc_rs_errors::Result;

use super::{Defaults, Lexicon, Lists, Vocabulary};

/// An in-memory vocabulary standing in for the master and a scope of
/// candidates.
pub(crate) struct Fake {
    /// The master's shared lists and all word.
    pub(crate) defaults: Defaults,
    /// Each candidate's source of words; its length is the scope's size.
    pub(crate) lexicons: Vec<Lexicon>,
    /// Which candidates have commands enabled.
    pub(crate) living: Vec<bool>,
    /// Which candidates are undestructed.
    pub(crate) live: Vec<bool>,
    /// Which candidates sit outside the scope proper
    /// (`parse_command_users()`).
    pub(crate) remote: Vec<bool>,
    /// The master's numeral table.
    pub(crate) numerals: HashMap<&'static str, i64>,
    /// The phrases `IdFunction` candidates answer yes to.
    pub(crate) id_true: Vec<&'static str>,
    /// Every word the master was asked as a numeral.
    pub(crate) asked: Vec<String>,
    /// How many times the master's defaults were fetched.
    pub(crate) defaults_asked: usize,
}

impl Fake {
    /// A scope of `lexicons.len()` live, non-living, local candidates.
    pub(crate) fn new(lexicons: Vec<Lexicon>) -> Fake {
        let n = lexicons.len();
        Fake {
            defaults: Defaults::default(),
            lexicons,
            living: vec![false; n],
            live: vec![true; n],
            remote: vec![false; n],
            numerals: HashMap::new(),
            id_true: vec![],
            asked: vec![],
            defaults_asked: 0,
        }
    }
}

impl Vocabulary for Fake {
    fn candidates(&self) -> usize {
        self.lexicons.len()
    }
    fn is_live(&self, candidate: usize) -> bool {
        self.live[candidate]
    }
    fn is_living(&self, candidate: usize) -> bool {
        self.living[candidate]
    }
    fn is_remote(&self, candidate: usize) -> bool {
        self.remote[candidate]
    }
    async fn defaults(&mut self) -> Result<Defaults> {
        self.defaults_asked += 1;
        Ok(self.defaults.clone())
    }
    async fn numeral(&mut self, word: &str) -> Result<i64> {
        self.asked.push(word.to_owned());
        Ok(self.numerals.get(word).copied().unwrap_or(0))
    }
    async fn lexicon(&mut self, candidate: usize) -> Result<Lexicon> {
        Ok(self.lexicons[candidate].clone())
    }
    async fn id(&mut self, _candidate: usize, phrase: &str) -> Result<bool> {
        Ok(self.id_true.contains(&phrase))
    }
}

/// One candidate's lists.
pub(crate) fn lists(ids: &[&str], plurals: &[&str], adjectives: &[&str]) -> Lexicon {
    Lexicon::Lists(Lists {
        ids: strings(ids),
        plurals: strings(plurals),
        adjectives: strings(adjectives),
    })
}

/// `items` as owned strings.
pub(crate) fn strings(items: &[&str]) -> Vec<String> {
    items.iter().map(|s| (*s).to_owned()).collect()
}

/// A sword (0), a red sword (1), a bag (2), and a living guard (3).
pub(crate) fn scene() -> Fake {
    let mut fake = Fake::new(vec![
        lists(&["sword"], &["swords"], &[]),
        lists(&["sword"], &["swords"], &["red"]),
        lists(&["bag"], &["bags"], &["old"]),
        lists(&["guard"], &["guards"], &[]),
    ]);
    fake.living[3] = true;
    fake.defaults.all_word = Some("all".to_owned());
    fake.defaults.prepositions = strings(&["in", "in front of"]);
    fake.numerals = HashMap::from([("two", 2), ("second", -2)]);
    fake
}
