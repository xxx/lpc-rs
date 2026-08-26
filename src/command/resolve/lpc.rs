//! The vocabulary behind real objects — `parse_command_*` and `id` applies
//! nested in the caller's transaction — and captures to LPC values.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::{Defaults, Lexicon, Lists, Resolved, Resolver, Vocabulary};
use crate::{
    command::frontend::native::{Capture, plain_value},
    interpreter::{
        ID, PARSE_COMMAND_ADJECTIV_ID_LIST, PARSE_COMMAND_ALL_WORD, PARSE_COMMAND_ID_LIST,
        PARSE_COMMAND_NUMERAL, PARSE_COMMAND_PLURAL_ID_LIST, PARSE_COMMAND_PLURALIZE,
        PARSE_COMMAND_PREPOS_LIST, lpc_array::LpcArray, lpc_int::LpcInt, lpc_ref::LpcRef,
        process::Process, task::apply_function::apply_function, task_context::TaskContext,
    },
};

/// A scope of objects asked through applies in `ctx`'s transaction.
pub struct LpcVocabulary<'a> {
    ctx: &'a TaskContext,
    scope: Vec<Arc<Process>>,
    /// Extra ids per candidate (the parser package's nicknames).
    extras: Vec<Vec<String>>,
}

impl<'a> LpcVocabulary<'a> {
    /// Over `scope`, in the order `%o` prefers.
    pub fn new(ctx: &'a TaskContext, scope: Vec<Arc<Process>>) -> Self {
        let extras = vec![Vec::new(); scope.len()];
        LpcVocabulary { ctx, scope, extras }
    }

    /// Over `scope`, each candidate answering to `extras[i]` as well as its
    /// own ids; `extras` must be `scope.len()` long.
    pub fn with_extras(
        ctx: &'a TaskContext,
        scope: Vec<Arc<Process>>,
        extras: Vec<Vec<String>>,
    ) -> Self {
        debug_assert_eq!(extras.len(), scope.len());
        LpcVocabulary { ctx, scope, extras }
    }

    /// The candidates behind the resolver's indices.
    pub fn scope(&self) -> &[Arc<Process>] {
        &self.scope
    }

    /// The context the applies run in.
    pub fn ctx(&self) -> &'a TaskContext {
        self.ctx
    }

    /// Apply `name` on `target` nested in the caller's transaction, with
    /// `this_player` unchanged; `None` when `target` does not define it.
    async fn apply(
        &self,
        target: &Arc<Process>,
        name: &str,
        args: &[LpcRef],
    ) -> Result<Option<LpcRef>> {
        let Some(function) = target.program.unmangled_functions.get(name).cloned() else {
            return Ok(None);
        };
        let nested = self.ctx.clone().with_process(target.clone());
        let timeout = self.ctx.config().max_execution_time;
        apply_function(function, args, nested, Some(timeout))
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
        let Some(master) = self.ctx.object_space().master_object() else {
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

    async fn defaults(&mut self) -> Result<Defaults> {
        let all_word = match self.ctx.object_space().master_object() {
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
        let Some(master) = self.ctx.object_space().master_object() else {
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

/// The LPC values of `captures` in slot order — `%o` an object, `%i`/`%l`
/// `({ numeral, ob... })`, `%p` the matched entry as a string, the rest by
/// [`plain_value`] — or `None` at the first phrase that names nothing.
pub async fn values(
    captures: &[Capture],
    resolver: &mut Resolver<LpcVocabulary<'_>>,
) -> Result<Option<Vec<LpcRef>>> {
    let mut resolved: Vec<Option<Resolved>> = Vec::with_capacity(captures.len());
    for capture in captures {
        match capture.kind.resolver_kind() {
            None => resolved.push(None),
            Some(kind) => match resolver.resolve(kind, &capture.text).await? {
                Some(found) => resolved.push(Some(found)),
                None => return Ok(None),
            },
        }
    }
    let scope = resolver.vocabulary().scope();
    let txn = resolver.vocabulary().ctx().txn();
    let object = |candidate: usize| LpcRef::from(Arc::downgrade(&scope[candidate]));
    let mut out = Vec::with_capacity(captures.len());
    for (capture, found) in captures.iter().zip(resolved) {
        let value = match found {
            None => match plain_value(capture) {
                Some(value) => value,
                None => return Ok(None),
            },
            Some(Resolved::Object(candidate)) => object(candidate),
            Some(Resolved::Items {
                numeral,
                candidates,
            }) => {
                let items = std::iter::once(LpcRef::from(numeral))
                    .chain(candidates.iter().map(|&candidate| object(candidate)))
                    .collect::<LpcArray>();
                LpcRef::Array(txn.with(|t| t.mint_array(items)))
            }
            Some(Resolved::Preposition(index)) => {
                LpcRef::from(resolver.prepositions()[index].as_str())
            }
        };
        out.push(value);
    }
    Ok(Some(out))
}
