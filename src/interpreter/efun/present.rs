//! `present(id | object [, env])`: the `n`th object answering `id("name")`
//! in `env`'s inventory (default: the caller's own, then its
//! environment's), or whether `object` is there.

use std::{sync::Arc, vec};

use lpc_rs_errors::Result;
use smallvec::SmallVec;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::efun_context::EfunContext,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::TxnHandle,
};

/// `present(id | object [, env])`: the `n`th object answering `id("name")`
/// in `env`'s inventory (default: the caller's own, then its
/// environment's), or whether `object` is there.
pub fn present<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let txn = context.txn();
    let this = context.process().clone();
    let env = match context.arg(1) {
        LpcRef::Int(LpcInt(0)) => None,
        arg @ LpcRef::Object(_) => match arg.live_object(txn) {
            Some(env) => Some(env),
            None => {
                context.return_efun_result(NULL);
                return Ok(());
            }
        },
        other => {
            return Err(
                context.runtime_error(format!("present: {} is not an object", other.type_name()))
            );
        }
    };
    match context.arg(0) {
        LpcRef::String(s) => {
            let (id, n) = numbered(s.to_str());
            let candidates = match &env {
                Some(env) => Process::inventory_of(txn, env),
                None => {
                    let mut own = Process::inventory_of(txn, &this);
                    if let Some(around) = Process::environment_of(txn, &this) {
                        own.extend(Process::inventory_of(txn, &around));
                    }
                    own
                }
            };
            if n == 0 || candidates.is_empty() {
                context.return_efun_result(NULL);
                return Ok(());
            }
            context.continue_with(Box::new(Present {
                candidates: candidates.into_iter(),
                id: LpcRef::from(id),
                n,
                seen: 0,
                asked: None,
            }));
            Ok(())
        }
        arg @ LpcRef::Object(_) => {
            let result = match arg.live_object(txn) {
                Some(ob) => {
                    let holder = Process::environment_of(txn, &ob);
                    let here = match &env {
                        Some(env) => holder.as_ref().is_some_and(|h| Arc::ptr_eq(h, env)),
                        None => holder.as_ref().is_some_and(|h| {
                            Arc::ptr_eq(h, &this)
                                || Process::environment_of(txn, &this)
                                    .is_some_and(|around| Arc::ptr_eq(h, &around))
                        }),
                    };
                    if here { arg.clone() } else { NULL }
                }
                None => NULL,
            };
            context.return_efun_result(result);
            Ok(())
        }
        other => Err(context.runtime_error(format!(
            "present: {} is not a string or object",
            other.type_name()
        ))),
    }
}

/// `"sword 2"` as `("sword", 2)`; a string with no trailing number is
/// the first.
fn numbered(s: &str) -> (&str, usize) {
    match s.rsplit_once(' ') {
        Some((id, n)) => match n.parse() {
            Ok(n) => (id, n),
            Err(_) => (s, 1),
        },
        None => (s, 1),
    }
}

/// The walk: each candidate defining `id` is asked in turn; the `n`th to
/// answer true is the result.
#[derive(Debug, Clone)]
struct Present {
    candidates: vec::IntoIter<Arc<Process>>,
    id: LpcRef,
    n: usize,
    seen: usize,
    asked: Option<Arc<Process>>,
}

impl Continuation for Present {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let (Some(verdict), Some(asked)) = (result, self.asked.take())
            && verdict.is_truthy(txn)
        {
            self.seen += 1;
            if self.seen == self.n {
                return Ok(Next::Done(LpcRef::from(Arc::downgrade(&asked))));
            }
        }
        for candidate in self.candidates.by_ref() {
            let Some(function) = candidate.program.unmangled_functions.get("id").cloned() else {
                continue;
            };
            self.asked = Some(candidate.clone());
            return Ok(Next::Call(Callee::Function {
                process: candidate,
                function,
                args: SmallVec::from_vec(vec![self.id.clone()]),
            }));
        }
        Ok(Next::Done(NULL))
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, process::Process, vm::Vm},
        test_support::test_config,
    };

    /// A room holding the finder, a sword and a rock, with a second sword
    /// inside the finder.
    struct World {
        vm: Vm,
        room: Arc<Process>,
        room_sword: Arc<Process>,
        hand_sword: Arc<Process>,
    }

    const FINDER: &str = r#"
        void create() { move_object("/room"); }
        object find(string s) { return present(s); }
        object find_in(string s, object env) { return present(s, env); }
        object here(object ob) { return present(ob); }
        object here_in(object ob, object env) { return present(ob, env); }
    "#;

    async fn world() -> World {
        let vm = Vm::new(test_config());
        let init = |path: &'static str, code: &'static str| {
            let vm = &vm;
            async move {
                vm.initialize_process_from_code(path, code)
                    .await
                    .unwrap()
                    .context
                    .process
            }
        };
        let room = init("/room.c", "").await;
        init("/finder.c", FINDER).await;
        let room_sword = init(
            "/room_sword.c",
            r#"int id(string s) { return s == "sword"; } void create() { move_object("/room"); }"#,
        )
        .await;
        let hand_sword = init(
            "/hand_sword.c",
            r#"int id(string s) { return s == "sword" || (s == "asker" && previous_object() == find_object("/finder")); } void create() { move_object("/finder"); }"#,
        )
        .await;
        init("/rock.c", r#"void create() { move_object("/room"); }"#).await;
        World {
            vm,
            room,
            room_sword,
            hand_sword,
        }
    }

    /// `expr`, evaluated by a fresh caller object, as its committed global.
    async fn found(world: &World, expr: &str) -> LpcRef {
        let code = format!("object found; void create() {{ found = {expr}; }}");
        let caller = world
            .vm
            .initialize_process_from_code("/caller.c", &code)
            .await
            .unwrap()
            .context
            .process;
        world.vm.global_state.committed_global(&caller, 0u16)
    }

    fn object(process: &Arc<Process>) -> LpcRef {
        LpcRef::from(Arc::downgrade(process))
    }

    #[tokio::test]
    async fn present_searches_the_callers_own_inventory_first() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("sword")"#).await;
        assert_eq!(r, object(&w.hand_sword));
    }

    #[tokio::test]
    async fn a_numbered_id_counts_on_across_the_environment() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("sword 2")"#).await;
        assert_eq!(r, object(&w.room_sword));
    }

    #[tokio::test]
    async fn a_number_past_the_matches_is_zero() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("sword 3")"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn an_environment_argument_limits_the_search_to_it() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find_in("sword", find_object("/room"))"#).await;
        assert_eq!(r, object(&w.room_sword));
    }

    #[tokio::test]
    async fn an_absent_id_is_zero() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("shield")"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn an_object_without_id_never_matches() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("rock")"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn present_of_an_object_answers_it_when_it_is_beside_the_caller() {
        let w = world().await;
        let r = found(&w, r#""/finder"->here(find_object("/room_sword"))"#).await;
        assert_eq!(r, object(&w.room_sword));
    }

    #[tokio::test]
    async fn present_of_an_object_elsewhere_is_zero() {
        let w = world().await;
        let r = found(
            &w,
            r#""/finder"->here_in(find_object("/hand_sword"), find_object("/room"))"#,
        )
        .await;
        assert_eq!(r, LpcRef::from(0));
        let _ = &w.room;
    }

    #[tokio::test]
    async fn present_of_an_object_in_the_environment_given_answers_it() {
        let w = world().await;
        let r = found(
            &w,
            r#""/finder"->here_in(find_object("/room_sword"), find_object("/room"))"#,
        )
        .await;
        assert_eq!(r, object(&w.room_sword));
    }

    #[tokio::test]
    async fn id_is_asked_with_the_caller_of_present_as_previous_object() {
        let w = world().await;
        let r = found(&w, r#""/finder"->find("asker")"#).await;
        assert_eq!(r, object(&w.hand_sword));
    }

    #[tokio::test]
    async fn a_first_argument_that_is_neither_is_an_error() {
        let w = world().await;
        let err = w
            .vm
            .initialize_process_from_code("/caller.c", "void create() { mixed x = 1; present(x); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("present: int is not a string or object"),
            "{err}"
        );
    }
}
