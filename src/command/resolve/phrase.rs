//! Whether a phrase names a candidate: `adjective* noun`, the noun from the
//! id lists and every word before it an adjective.

use super::vocabulary::Defaults;

/// One candidate's own word lists.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Lists {
    /// Singular ids; an entry may be several words.
    pub ids: Vec<String>,
    /// Plural ids.
    pub plurals: Vec<String>,
    /// Adjectives; an entry may be several words.
    pub adjectives: Vec<String>,
}

/// A successful match.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Match {
    /// The noun came from a plural list.
    pub plural: bool,
}

/// Whether `words` names a candidate with `own` lists: some suffix equals an
/// id — a singular unless `plural_expected`, else a plural; the candidate's
/// own list and the master's shared one both count — and the words before it
/// are adjectives. Singulars are tried first, so the reported `plural` is
/// false whenever a singular fits.
pub fn match_phrase(
    words: &[&str],
    own: &Lists,
    defaults: &Defaults,
    plural_expected: bool,
) -> Option<Match> {
    let singulars = [&own.ids, &defaults.ids];
    let plurals = [&own.plurals, &defaults.plurals];
    let nouns = |lists: &[&Vec<String>; 2], plural: bool| {
        lists
            .iter()
            .flat_map(|list| list.iter())
            .any(|entry| ends_with_noun(words, entry, own, defaults))
            .then_some(Match { plural })
    };
    (!plural_expected)
        .then(|| nouns(&singulars, false))
        .flatten()
        .or_else(|| nouns(&plurals, true))
}

/// Whether `words` ends with the (possibly multi-word) noun `entry` and the
/// words before it are adjectives.
fn ends_with_noun(words: &[&str], entry: &str, own: &Lists, defaults: &Defaults) -> bool {
    let noun: Vec<&str> = entry.split_whitespace().collect();
    if noun.is_empty() || noun.len() > words.len() {
        return false;
    }
    let split = words.len() - noun.len();
    words[split..] == noun[..] && adjectives_cover(&words[..split], own, defaults)
}

/// Whether `words` is a sequence of adjectives: from the left, the longest
/// listed adjective (single or multi-word) is taken, repeatedly.
fn adjectives_cover(mut words: &[&str], own: &Lists, defaults: &Defaults) -> bool {
    while !words.is_empty() {
        let longest = own
            .adjectives
            .iter()
            .chain(&defaults.adjectives)
            .map(|entry| entry.split_whitespace().collect::<Vec<_>>())
            .filter(|adj| {
                !adj.is_empty() && adj.len() <= words.len() && words[..adj.len()] == adj[..]
            })
            .map(|adj| adj.len())
            .max();
        match longest {
            Some(taken) => words = &words[taken..],
            None => return false,
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    fn strings(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| (*s).to_owned()).collect()
    }

    fn sword() -> Lists {
        Lists {
            ids: strings(&["sword", "long sword"]),
            plurals: strings(&["swords"]),
            adjectives: strings(&["red", "very old"]),
        }
    }

    fn defaults() -> Defaults {
        Defaults {
            ids: strings(&["it", "thing"]),
            plurals: strings(&["them"]),
            adjectives: strings(&["that"]),
            prepositions: vec![],
            all_word: None,
        }
    }

    #[test]
    fn a_bare_id_matches_as_singular() {
        assert_eq!(
            match_phrase(&["sword"], &sword(), &defaults(), false),
            Some(Match { plural: false })
        );
    }

    #[test]
    fn adjectives_before_the_id_are_accepted() {
        assert_eq!(
            match_phrase(&["red", "sword"], &sword(), &defaults(), false),
            Some(Match { plural: false })
        );
        assert_eq!(
            match_phrase(&["that", "red", "sword"], &sword(), &defaults(), false),
            Some(Match { plural: false })
        );
    }

    #[test]
    fn a_multi_word_id_matches_the_whole_suffix() {
        assert_eq!(
            match_phrase(&["red", "long", "sword"], &sword(), &defaults(), false),
            Some(Match { plural: false })
        );
    }

    #[test]
    fn a_multi_word_adjective_is_one_adjective() {
        assert_eq!(
            match_phrase(&["very", "old", "sword"], &sword(), &defaults(), false),
            Some(Match { plural: false })
        );
        assert_eq!(
            match_phrase(&["very", "sword"], &sword(), &defaults(), false),
            None
        );
    }

    #[test]
    fn an_uncovered_word_fails() {
        assert_eq!(
            match_phrase(&["blue", "sword"], &sword(), &defaults(), false),
            None
        );
        assert_eq!(
            match_phrase(&["sword", "blue"], &sword(), &defaults(), false),
            None
        );
    }

    #[test]
    fn a_plural_id_reports_plural() {
        assert_eq!(
            match_phrase(&["swords"], &sword(), &defaults(), false),
            Some(Match { plural: true })
        );
        assert_eq!(
            match_phrase(&["them"], &sword(), &defaults(), false),
            Some(Match { plural: true })
        );
    }

    #[test]
    fn plural_expected_tries_only_the_plural_lists() {
        assert_eq!(match_phrase(&["sword"], &sword(), &defaults(), true), None);
        assert_eq!(
            match_phrase(&["red", "swords"], &sword(), &defaults(), true),
            Some(Match { plural: true })
        );
    }

    #[test]
    fn the_masters_ids_name_any_candidate() {
        assert_eq!(
            match_phrase(&["it"], &Lists::default(), &defaults(), false),
            Some(Match { plural: false })
        );
    }

    #[test]
    fn an_empty_phrase_names_nothing() {
        assert_eq!(match_phrase(&[], &sword(), &defaults(), false), None);
    }
}
