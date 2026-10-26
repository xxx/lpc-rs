//! The vocabulary behind real objects: `parse_command_*` and `id` applies
//! nested in the caller's transaction.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::{Defaults, Lexicon, Lists, Vocabulary};
use crate::interpreter::{
    ID, PARSE_COMMAND_ADJECTIV_ID_LIST, PARSE_COMMAND_ALL_WORD, PARSE_COMMAND_ID_LIST,
    PARSE_COMMAND_NUMERAL, PARSE_COMMAND_PLURAL_ID_LIST, PARSE_COMMAND_PLURALIZE,
    PARSE_COMMAND_PREPOS_LIST,
    apply::{apply_nested, diagnostics},
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
    process::{Process, shadow::ShadowEntry},
    task_context::{Callers, TaskContext},
};

/// A scope of objects asked through applies in `ctx`'s transaction.
pub struct LpcVocabulary<'a> {
    ctx: &'a TaskContext,
    /// The chain the applies are entered through: whoever is parsing.
    callers: Callers,
    scope: Vec<Arc<Process>>,
    /// Extra ids per candidate (the parser package's nicknames).
    extras: Vec<Vec<String>>,
    /// Candidates at or after this index are remote (`parse_command_users()`
    /// additions), nameable only through a living slot.
    remote_from: usize,
}

impl<'a> LpcVocabulary<'a> {
    /// Over `scope`, in the order `%o` prefers, asked through `callers`.
    pub fn new(ctx: &'a TaskContext, callers: Callers, scope: Vec<Arc<Process>>) -> Self {
        let extras = vec![Vec::new(); scope.len()];
        let remote_from = scope.len();
        LpcVocabulary {
            ctx,
            callers,
            scope,
            extras,
            remote_from,
        }
    }

    /// Over `scope`, each candidate answering to `extras[i]` as well as its
    /// own ids; `extras` must be `scope.len()` long. Candidates at or after
    /// `remote_from` are remote.
    pub fn with_extras(
        ctx: &'a TaskContext,
        callers: Callers,
        scope: Vec<Arc<Process>>,
        extras: Vec<Vec<String>>,
        remote_from: usize,
    ) -> Self {
        debug_assert_eq!(extras.len(), scope.len());
        LpcVocabulary {
            ctx,
            callers,
            scope,
            extras,
            remote_from,
        }
    }

    /// The candidates behind the resolver's indices.
    pub fn scope(&self) -> &[Arc<Process>] {
        &self.scope
    }

    /// The context the applies run in.
    pub fn ctx(&self) -> &'a TaskContext {
        self.ctx
    }

    /// Apply `name` on `target` nested in the caller's transaction, through
    /// `target`'s shadow chain, with `this_player` unchanged; `None` when
    /// nothing in the chain defines it.
    async fn apply(
        &self,
        target: &Arc<Process>,
        name: &str,
        args: &[LpcRef],
    ) -> Result<Option<LpcRef>> {
        let (target, function) = match Process::shadow_entry(self.ctx.txn(), target, name, target) {
            ShadowEntry::Unshadowed => {
                let Some(function) = target
                    .program(self.ctx.txn())
                    .unmangled_functions
                    .get(name)
                    .cloned()
                else {
                    diagnostics::missing(name, Some(target));
                    return Ok(None);
                };
                (target.clone(), function)
            }
            ShadowEntry::Found(process, function) => (process, function),
            ShadowEntry::Fallback(real) => {
                let Some(function) = real
                    .program(self.ctx.txn())
                    .unmangled_functions
                    .get(name)
                    .cloned()
                else {
                    diagnostics::missing(name, Some(&real));
                    return Ok(None);
                };
                (real, function)
            }
        };
        apply_nested(self.ctx, self.callers.clone(), &target, function, args)
            .await
            .map(Some)
    }

    /// The string members of an array result; anything else is nothing. Do
    /// not swallow the array arm's error — its only cause is a cell the world
    /// just handed us going missing, a driver bug.
    fn strings(&self, value: Option<LpcRef>) -> Result<Vec<String>> {
        match value {
            Some(array @ LpcRef::Array(_)) => array.with_array(self.ctx.txn(), |a| {
                a.iter()
                    .filter_map(|item| item.as_str().map(str::to_owned))
                    .collect()
            }),
            _ => Ok(Vec::new()),
        }
    }

    async fn master_strings(&self, name: &str, args: &[LpcRef]) -> Result<Vec<String>> {
        let Some(master) = self.ctx.master_object() else {
            diagnostics::missing(name, None);
            return Ok(Vec::new());
        };
        let value = self.apply(&master, name, args).await?;
        self.strings(value)
    }
}

impl Vocabulary for LpcVocabulary<'_> {
    fn candidates(&self) -> usize {
        self.scope.len()
    }

    fn is_live(&self, candidate: usize) -> bool {
        self.scope[candidate].is_live(self.ctx.txn())
    }

    fn is_living(&self, candidate: usize) -> bool {
        self.scope[candidate].commands_enabled(self.ctx.txn())
    }

    fn is_remote(&self, candidate: usize) -> bool {
        candidate >= self.remote_from
    }

    async fn defaults(&mut self) -> Result<Defaults> {
        let all_word = match self.ctx.master_object() {
            Some(master) => match self.apply(&master, PARSE_COMMAND_ALL_WORD, &[]).await? {
                Some(LpcRef::String(word)) => Some(word.to_string()),
                _ => None,
            },
            None => None,
        };
        Ok(Defaults {
            ids: self.master_strings(PARSE_COMMAND_ID_LIST, &[]).await?,
            plurals: self
                .master_strings(PARSE_COMMAND_PLURAL_ID_LIST, &[])
                .await?,
            adjectives: self
                .master_strings(PARSE_COMMAND_ADJECTIV_ID_LIST, &[])
                .await?,
            prepositions: self.master_strings(PARSE_COMMAND_PREPOS_LIST, &[]).await?,
            all_word,
        })
    }

    async fn numeral(&mut self, word: &str) -> Result<i64> {
        let Some(master) = self.ctx.master_object() else {
            return Ok(0);
        };
        Ok(
            match self
                .apply(&master, PARSE_COMMAND_NUMERAL, &[LpcRef::from(word)])
                .await?
            {
                Some(LpcRef::Int(LpcInt(n))) => n,
                _ => 0,
            },
        )
    }

    async fn lexicon(&mut self, candidate: usize) -> Result<Lexicon> {
        let object = self.scope[candidate].clone();
        let Some(ids) = self.apply(&object, PARSE_COMMAND_ID_LIST, &[]).await? else {
            return Ok(Lexicon::IdFunction);
        };
        let mut ids = self.strings(Some(ids))?;
        ids.extend(self.extras[candidate].iter().cloned());
        let plurals = match self
            .apply(&object, PARSE_COMMAND_PLURAL_ID_LIST, &[])
            .await?
        {
            Some(array @ LpcRef::Array(_)) => self.strings(Some(array))?,
            _ => {
                let singulars = ids
                    .iter()
                    .map(|id| LpcRef::from(id.as_str()))
                    .collect::<LpcArray>();
                let singulars = LpcRef::Array(self.ctx.txn().with(|t| t.mint_array(singulars)));
                self.master_strings(PARSE_COMMAND_PLURALIZE, &[singulars])
                    .await?
            }
        };
        let adjectives = self
            .apply(&object, PARSE_COMMAND_ADJECTIV_ID_LIST, &[])
            .await?;
        Ok(Lexicon::Lists(Lists {
            ids,
            plurals,
            adjectives: self.strings(adjectives)?,
        }))
    }

    async fn id(&mut self, candidate: usize, phrase: &str) -> Result<bool> {
        if self.extras[candidate].iter().any(|extra| extra == phrase) {
            return Ok(true);
        }
        let object = self.scope[candidate].clone();
        let answer = self.apply(&object, ID, &[LpcRef::from(phrase)]).await?;
        Ok(matches!(answer, Some(value) if value.is_truthy(self.ctx.txn())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::vm::Vm,
        test_support::{permissive_master, run_prog, test_config},
    };

    #[tokio::test]
    async fn candidate_liveness_and_remote_boundaries_follow_the_scope() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let dead = vm
            .initialize_process_from_code("/gone.c", "")
            .await
            .unwrap()
            .context
            .process;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                "void create() { destruct(find_object(\"/gone\")); }",
            )
            .await
            .unwrap();
        let scope = vec![dead, task.context.process().clone()];
        let vocabulary = LpcVocabulary::new(&task.context, None, scope.clone());
        assert_eq!(vocabulary.candidates(), 2);
        assert!(!vocabulary.is_live(0));
        assert!(vocabulary.is_live(1));
        assert!(!vocabulary.is_remote(0));
        assert!(!vocabulary.is_remote(1));
        for (remote_from, expected) in [(0, [true, true]), (1, [false, true]), (2, [false, false])]
        {
            let vocabulary = LpcVocabulary::with_extras(
                &task.context,
                None,
                scope.clone(),
                vec![vec![], vec![]],
                remote_from,
            );
            for (index, expected) in expected.into_iter().enumerate() {
                assert_eq!(vocabulary.is_remote(index), expected);
            }
        }
    }

    #[tokio::test]
    async fn explicit_plural_lists_work_without_a_master_pluralizer() {
        let task = run_prog(
            r#"
            string *parse_command_id_list() { return ({ "goose" }); }
            string *parse_command_plural_id_list() { return ({ "geese" }); }
            string *parse_command_adjectiv_id_list() { return ({ "wild" }); }
            int create() {
                mixed *matches;
                if (!parse_command("wild geese", ({ this_object() }), "%i", matches)) return 0;
                return sizeof(matches) == 2 && matches[1] == this_object();
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn nickname_fallback_matches_exact_aliases_for_id_function_objects() {
        let task = run_prog(
            r#"
            int handled;
            int id(string phrase) { return phrase == "native"; }
            mixed can_touch_obj(object ob, string word) { return 1; }
            mixed direct_touch_obj(object ob, string word) { return 1; }
            void do_touch_obj(object ob, string word) { handled++; }
            int create() {
                enable_commands();
                set_this_player(this_object());
                parse_init();
                parse_add_rule("touch", "OBJ");
                object *scope = ({ this_object() });
                mapping names = ([ "alias": this_object() ]);
                int alias = parse_sentence("touch alias", 0, scope, names);
                int ordinary = parse_sentence("touch native", 0, scope, names);
                int other = parse_sentence("touch unrelated", 0, scope, names);
                return alias == 1 && ordinary == 1 && other < 1 && handled == 2;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
