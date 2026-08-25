//! The productions the pattern frontends share, injected into a builder on
//! demand: `%w`, `%s`, `%i`, `[word]`, and `a / b` all reduce to these.

use super::model::{Element, GrammarBuilder, NtId, TokenClass, nt, tok};

/// The plain-words token rules; `number` precedes `word` so a digit run ties
/// to `number`.
#[derive(Clone, Copy, Debug)]
pub struct Words {
    /// Runs of whitespace, skipped from the token stream.
    pub whitespace: TokenClass,
    /// A run of digits.
    pub number: TokenClass,
    /// A run of non-whitespace.
    pub word: TokenClass,
}

impl GrammarBuilder {
    /// Register the `whitespace`/`number`/`word` token rules a plain-words grammar needs.
    pub fn words_tokens(&mut self) -> Words {
        Words {
            whitespace: self.skip_token("whitespace", r"\s+"),
            number: self.token("number", "[0-9]+"),
            word: self.token("word", r"\S+"),
        }
    }

    /// `word_like → word | number`; a grammar that calls this must set its
    /// start explicitly, since these injected productions come first.
    pub fn word_like(&mut self, words: &Words) -> NtId {
        let id = self.nonterminal("%word_like");
        if !self.is_defined(id) {
            self.production(id, [tok(words.word)]);
            self.production(id, [tok(words.number)]);
        }
        id
    }

    /// `words_plus → word_like | words_plus word_like`; a grammar that calls
    /// this must set its start explicitly, since these injected productions
    /// come first.
    pub fn words_plus(&mut self, words: &Words) -> NtId {
        let word_like = self.word_like(words);
        let id = self.nonterminal("%words_plus");
        if !self.is_defined(id) {
            self.production(id, [nt(word_like)]);
            self.production(id, [nt(id), nt(word_like)]);
        }
        id
    }

    /// `words_star → ε | words_star word_like`; a grammar that calls this
    /// must set its start explicitly, since these injected productions come
    /// first.
    pub fn words_star(&mut self, words: &Words) -> NtId {
        let word_like = self.word_like(words);
        let id = self.nonterminal("%words_star");
        if !self.is_defined(id) {
            self.production(id, []);
            self.production(id, [nt(id), nt(word_like)]);
        }
        id
    }

    /// A fresh `opt → x | ε`; greediness comes from longest-span-first
    /// enumeration, not production order, which matters only when `element`
    /// is itself nullable. A grammar that calls this must set its start
    /// explicitly, since these injected productions come first.
    pub fn optional(&mut self, element: Element) -> NtId {
        let id = self.fresh("%opt");
        self.production(id, [element]);
        self.production(id, []);
        id
    }

    /// A fresh `alt → a | b | …`, one production per alternative; a grammar
    /// that calls this must set its start explicitly, since these injected
    /// productions come first.
    pub fn alternatives(&mut self, elements: impl IntoIterator<Item = Element>) -> NtId {
        let id = self.fresh("%alt");
        for element in elements {
            self.production(id, [element]);
        }
        id
    }
}
