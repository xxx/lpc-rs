//! The numeral that may open an item phrase.

use lpc_rs_errors::Result;

use super::vocabulary::Vocabulary;

/// The numeral `word` is: a digit run of one or more is itself, the master's
/// all word is 0, anything else is what `parse_command_numeral` says (0 is
/// none). `> 0` a count, `< 0` an ordinal.
pub(super) async fn numeral<V: Vocabulary>(
    word: &str,
    all_word: Option<&str>,
    vocabulary: &mut V,
) -> Result<Option<i64>> {
    if !word.is_empty() && word.bytes().all(|b| b.is_ascii_digit()) {
        return Ok(word.parse::<i64>().ok().filter(|n| *n >= 1));
    }
    if all_word == Some(word) {
        return Ok(Some(0));
    }
    let n = vocabulary.numeral(word).await?;
    Ok((n != 0).then_some(n))
}
