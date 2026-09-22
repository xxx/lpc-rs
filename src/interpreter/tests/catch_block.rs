use super::{fails, run, s};
use crate::{compiler::Compiler, interpreter::lpc_ref::LpcRef, test_support::test_config};

#[tokio::test]
async fn block_catch_returns_zero_or_the_error_and_keeps_prior_writes() {
    let result = run(
        "",
        &[],
        r#"
        int changed;
        mixed *create() {
            int local;
            mixed empty = catch {};
            mixed success = catch { local = 4; changed = 5; };
            mixed failure = catch {
                local += 2;
                changed += 3;
                throw("failed");
                changed = -1;
            };
            return ({ empty, success, failure, local, changed, catch { throw(17); } });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(0),
            LpcRef::from(0),
            s("failed"),
            LpcRef::from(6),
            LpcRef::from(8),
            s("17")
        ]
    );
}

#[tokio::test]
async fn block_catch_handles_runtime_errors_and_remote_calls() {
    let result = run(
        "",
        &[("/other.c", "void fail() { throw(\"remote\"); }")],
        r#"
        mixed *create() {
            int zero;
            mixed arithmetic = catch { int value = 10 / zero; };
            mixed remote = catch { find_object("/other")->fail(); };
            mixed after = catch {};
            return ({ stringp(arithmetic), remote, after });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(1), s("remote"), LpcRef::from(0)]);
}

#[tokio::test]
async fn block_and_expression_catches_nest_and_rethrow() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            mixed first;
            mixed second;
            mixed outer = catch {
                first = catch(throw("expression"));
                second = catch { throw("block"); };
                throw(second);
            };
            mixed wrapped = catch(catch { throw("wrapped"); });
            return ({ first, second, outer, wrapped, catch {} });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            s("expression"),
            s("block"),
            s("block"),
            LpcRef::from(0),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn block_catch_locals_have_block_scope_and_can_be_captured() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int value = 10;
            function read;
            function snapshot;
            catch {
                int value = 2;
                read = (: ++value :);
                snapshot = (: $(value) :);
            };
            return ({ value, read(), read(), snapshot() });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(10),
            LpcRef::from(3),
            LpcRef::from(4),
            LpcRef::from(2)
        ]
    );
}

#[tokio::test]
async fn return_leaves_all_handlers_in_its_function_but_keeps_the_callers() {
    let result = run(
        "",
        &[],
        r#"
        int count;
        int value() {
            catch { catch(catch { return 42; }); };
            count = -100;
            return -1;
        }
        void quiet() { catch { return; }; count = -200; }
        mixed *create() {
            int got;
            mixed error = catch {
                got = value();
                quiet();
                count++;
                throw("caller");
            };
            return ({ got, count, error });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(42), LpcRef::from(1), s("caller")]);
}

#[tokio::test]
async fn errors_in_return_expressions_are_still_caught() {
    let result = run(
        "",
        &[],
        r#"
        int value() {
            int zero;
            catch { return 10 / zero; };
            return 7;
        }
        mixed *create() { return ({ value() }); }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(7)]);
}

#[tokio::test]
async fn returning_from_a_closure_does_not_remove_its_creators_handler() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int value;
            function later;
            mixed error = catch {
                function f = (: return 6; :);
                later = (: catch { return 8; }; return -1; :);
                value = f();
                throw("creator");
            };
            return ({ value, error, later() });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(6), s("creator"), LpcRef::from(8)]);
}

#[tokio::test]
async fn break_and_continue_leave_only_the_catches_they_cross() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int visits;
            int after;
            mixed outer = catch {
                for (int i = 0; i < 4; i++) {
                    catch {
                        catch(catch {
                            visits++;
                            if (i == 0) continue;
                            if (i == 2) break;
                        });
                    };
                    after++;
                }
                throw("outer");
            };
            return ({ visits, after, outer });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(3), LpcRef::from(1), s("outer")]);
}

#[tokio::test]
async fn jumps_within_a_catch_keep_its_handler() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int total;
            mixed error = catch {
                foreach (int n in ({ 1, 2, 3 })) {
                    if (n == 1) continue;
                    total += n;
                    if (n == 2) break;
                }
                int i;
                while (++i < 4) { if (i == 1) continue; break; }
                do { total++; continue; } while (0);
                throw("still caught");
            };
            return ({ total, error });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(3), s("still caught")]);
}

#[tokio::test]
async fn switch_break_and_loop_continue_use_different_handler_depths() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int visits;
            int after;
            mixed outer = catch {
                for (int i = 0; i < 3; i++) {
                    catch {
                        switch (i) {
                            case 0: catch { continue; }; break;
                            case 1: catch { break; }; break;
                            default: break;
                        }
                        visits++;
                        throw("inner");
                    };
                    after++;
                }
                throw("outer");
            };
            return ({ visits, after, outer });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(2), LpcRef::from(2), s("outer")]);
}

#[tokio::test]
async fn a_departed_handler_does_not_swallow_a_later_error() {
    for body in [
        "catch {};",
        "while (1) { catch { break; }; }",
        "for (int i = 0; i < 2; i++) { catch { continue; }; }",
        "leave();",
    ] {
        let code = format!(
            r#"
            void leave() {{ catch {{ return; }}; }}
            void create() {{ {body} throw("uncaught"); }}
        "#
        );
        assert_eq!(fails("", &[], &code).await, "uncaught", "{body}");
    }
}

#[tokio::test]
async fn block_catch_works_in_conditions_arguments_defaults_and_global_initializers() {
    let result = run(
        "",
        &[],
        r#"
        #define TRY catch
        int count;
        mixed initialized = TRY { int local = 2; count = local; };
        int f(mixed error = catch { count += 3; }) { return !error; }
        int pair(mixed left, mixed right) { return left == 0 && right == 0; }
        mixed *create() {
            if (catch { count++; }) count = -100;
            else count += 4;
            int a = pair(catch { count++; }, catch {});
            int b = f();
            return ({ initialized, a, b, count });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(0),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(11)
        ]
    );
}

#[tokio::test]
async fn repeated_blocks_reinitialize_locals_and_catch_results() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int total;
            int clean;
            for (int i = 0; i < 4; i++) {
                mixed error = catch {
                    int local;
                    total += local;
                    local = 10;
                    if (i == 0) throw("first");
                };
                clean += !error;
            }
            return ({ total, clean });
        }
    "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(0), LpcRef::from(3)]);
}

#[tokio::test]
async fn lima_style_handler_cleanup_continues_after_a_callback_error() {
    let result = run(
        "",
        &[],
        r#"
        class input_info { function input_func; }
        class input_info *stack;
        int calls;
        int failures;
        void good(int value) { calls += value; }
        void bad(int value) { calls += value; throw("callback"); }
        void clear_input_stack() {
            class input_info top;
            while (sizeof(stack)) {
                if (catch {
                    top = stack[0];
                    stack = stack[1..];
                    top.input_func(-1);
                }) failures++;
            }
        }
        mixed *create() {
            stack = ({ new(class input_info, input_func: &bad()),
                       new(class input_info, input_func: &good()) });
            clear_input_stack();
            return ({ calls, failures, sizeof(stack) });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![LpcRef::from(-2), LpcRef::from(1), LpcRef::from(0)]
    );
}

#[tokio::test]
async fn jumps_in_initializers_and_collections_belong_to_the_enclosing_loop() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int visited;
            int missed;
            for (int i = 0; i < 2; i++) {
                for (catch { visited++; continue; }; 0;) {}
                missed++;
            }
            for (int i = 0; i < 2; i++) {
                foreach (int item in (catch { visited++; continue; }, ({ 1 }))) {}
                missed++;
            }
            for (int i = 0; i < 2; i++) {
                switch (catch { visited++; continue; }) { default: break; }
                missed++;
            }
            int checks;
            while (catch { checks++; if (checks == 1) continue; break; }) missed++;
            return ({ visited, missed, checks, catch {} });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(6),
            LpcRef::from(0),
            LpcRef::from(2),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn catch_blocks_in_explicit_captures_run_when_the_closure_is_created() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int count;
            function value = (: $(catch { count++; throw("captured"); }) :);
            int before = count;
            mixed left = value();
            mixed right = value();
            0 && catch { count += 100; };
            1 || catch { count += 100; };
            return ({ before, count, left, right });
        }
    "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(1),
            LpcRef::from(1),
            s("captured"),
            s("captured")
        ]
    );
}

#[tokio::test]
async fn block_catch_keeps_type_scope_and_control_flow_diagnostics() {
    for (code, message) in [
        (
            "void f() { catch { int hidden; }; hidden = 1; }",
            "undefined variable `hidden`",
        ),
        (
            "void f() { catch { int n = \"wrong\"; }; }",
            "mismatched types",
        ),
        (
            "int f() { catch { return \"wrong\"; }; return 0; }",
            "invalid return type",
        ),
        ("void f() { catch { break; }; }", "invalid `break`"),
        ("void f() { catch { continue; }; }", "invalid `continue`"),
        (
            "void f() { while (1) { catch { function f = (: break; :); }; } }",
            "invalid `break`",
        ),
        (
            "void f() { switch (1) { catch { case 1: break; }; } }",
            "invalid `case`",
        ),
        (
            "mixed x = catch { return; };",
            "`return` outside a function",
        ),
        (
            "void f() {} mixed x = catch { return; };",
            "`return` outside a function",
        ),
        ("void f() { other {}; }", "expected `catch`"),
        (
            "void f() { for (catch { break; }; 0;) {} }",
            "invalid `break`",
        ),
        (
            "void f() { foreach (int n in (catch { continue; }, ({ 1 }))) {} }",
            "invalid `continue`",
        ),
        (
            "void f() { switch (catch { break; }) { default: break; } }",
            "invalid `break`",
        ),
    ] {
        let error = Compiler::new(test_config())
            .compile_string("/invalid.c", code)
            .await
            .expect_err(code)
            .diagnostic_string();
        assert!(
            error.to_lowercase().contains(&message.to_lowercase()),
            "{code}: {error}"
        );
    }
}
