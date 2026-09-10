//! Function pointer calls through variables, indexed values, and partial application.

use indoc::indoc;

use super::{fails, run};
use crate::interpreter::lpc_ref::LpcRef;

const ADD3: &str = "int add3(int a, int b, int c) { return a + 10 * b + 100 * c; }\n";

#[tokio::test]
async fn an_indexed_array_function_can_be_called() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function *callbacks = ({ &add3() });
                    return ({ callbacks[0](1, 2, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn an_indexed_mapping_function_can_be_called() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    mapping callbacks = ([ "add": &add3(1) ]);
                    return ({ callbacks["add"](2, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn indexed_calls_compose_with_nested_indexes_and_call_chains() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function make() { return (: $1 + 1 :); }
            mapping callbacks() { return ([ "make": ({ &make() }) ]); }
            mixed *create() {
                return ({ callbacks()["make"][0]()(41) });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn an_indexed_callee_is_evaluated_once_before_its_arguments() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            int order;
            int add(int a) { return a + 1; }
            function *callbacks() { order = order * 10 + 1; return ({ &add() }); }
            int index() { order = order * 10 + 2; return 0; }
            int argument() { order = order * 10 + 3; return 41; }
            mixed *create() {
                int result = callbacks()[index()](argument());
                return ({ result, order });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42), LpcRef::from(123)]);
}

#[tokio::test]
async fn an_indexed_call_captures_its_collection_in_a_closure() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function make() {
                mapping callbacks = ([ "add": (: $1 + 1 :) ]);
                return (: callbacks["add"]($1) :);
            }
            mixed *create() { return ({ make()(41) }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn indexed_calls_compose_with_object_receivers() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function *callbacks() { return ({ &this_object() }); }
            int answer() { return 42; }
            mixed *create() {
                return ({ this_object()->callbacks()[0]()->answer() });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn an_indexed_call_spreads_after_bound_arguments() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function *callbacks = ({ &add3(1) });
                    int *args = ({ 2, 3 });
                    return ({ callbacks[0](args...) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn an_indexed_call_rejects_reference_arguments() {
    let e = fails(
        "",
        &[],
        "mixed *create() { function *fs = ({ (: $1 :) }); int x; return ({ fs[0](ref x) }); }",
    )
    .await;
    assert!(
        e.contains("a function pointer cannot take an argument by reference"),
        "{e}"
    );
}

#[tokio::test]
async fn an_indexed_non_function_is_a_runtime_error() {
    let e = fails(
        "",
        &[],
        "mixed *create() { mapping callbacks = ([ \"value\": 5 ]); return ({ callbacks[\"value\"]() }); }",
    )
    .await;
    assert!(e.contains("callfp instruction on non-function: 5"), "{e}");
}

#[tokio::test]
async fn a_pointer_over_a_function_value_appends_its_partial_list() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3();
                    function g = &f(, 2);
                    function h = &g();
                    return ({ g(1, 3), h(1, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321), LpcRef::from(321)]);
}

#[tokio::test]
async fn a_pointer_over_a_bound_function_value_keeps_its_bindings() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3(1);
                    function g = &f(, 3);
                    return ({ g(2) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn a_pointer_over_a_captured_function_value_works_inside_a_closure() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3();
                    function mk = (: &f(, 2) :);
                    function g = mk();
                    return ({ g(1, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn applying_arguments_to_a_non_function_is_a_runtime_error() {
    let e = fails(
        "",
        &[],
        "mixed *create() { mixed f = 5; function g = &f(1); return ({ g }); }",
    )
    .await;
    assert!(
        e.contains("cannot apply arguments to int"),
        "unexpected error: {e}"
    );
}

mod callbacks {
    use super::*;
    use crate::{
        interpreter::tests::run_with,
        test_support::{lib_holding, temp_lib_config},
    };

    const OBJECTS: &[(&str, &str)] = &[
        ("/wearable.c", "int query_wearable_item() { return 1; }"),
        ("/unwearable.c", "int query_wearable_item() { return 0; }"),
        ("/other.c", "void unrelated() {}"),
    ];

    #[tokio::test]
    async fn filter_with_a_dynamic_pointer_treats_missing_methods_like_arrow_calls() {
        for callback in [
            "&->query_wearable_item()",
            "(: $1->query_wearable_item() :)",
        ] {
            let result = run(
                "",
                OBJECTS,
                &format!(
                    r#"
                        mixed *create() {{
                            object yes = find_object("/wearable");
                            object no = find_object("/unwearable");
                            object other = find_object("/other");
                            object *obs = ({{ other, yes, other, other, no, yes, other }});
                            return map(filter((obs), {callback}), &file_name());
                        }}
                    "#
                ),
            )
            .await;
            assert_eq!(result, vec![LpcRef::from("/wearable"); 2], "{callback}");
        }
    }

    #[tokio::test]
    async fn collection_callbacks_handle_missing_methods_after_loading() {
        let root = lib_holding(
            "filter-dynamic-missing",
            &[
                ("wearable.c", "int query_wearable_item() { return 1; }"),
                ("other.c", "void unrelated() {}"),
                ("another.c", "void unrelated() {}"),
            ],
        );
        for (efun, expected) in [
            ("filter", vec![LpcRef::from("/wearable"); 2]),
            ("map", [0, 1, 0, 1, 0].map(LpcRef::from).to_vec()),
        ] {
            let result = run_with(
                temp_lib_config(&root),
                "int valid_load(string p, string f, object c, string g) { return 1; }",
                &[],
                &format!(r#"
                    mixed *create() {{
                        string *obs = ({{ "/other", "/wearable", "/another", "/wearable", "/other" }});
                        return {efun}(obs, &->query_wearable_item());
                    }}
                "#),
            )
            .await;
            assert_eq!(result, expected, "{efun}");
        }
    }

    #[tokio::test]
    async fn map_preserves_zero_results_from_missing_methods() {
        for callback in [
            "&->query_wearable_item()",
            "&call_other(, \"query_wearable_item\")",
        ] {
            let result = run(
                "",
                OBJECTS,
                &format!(
                    r#"
                        mixed *create() {{
                            return map(({{ "/other", "/wearable", "/other", "/other", "/wearable", "/other" }}), {callback});
                        }}
                    "#
                ),
            )
            .await;
            assert_eq!(
                result,
                vec![0, 1, 0, 0, 1, 0]
                    .into_iter()
                    .map(LpcRef::from)
                    .collect::<Vec<_>>(),
                "{callback}"
            );
        }
    }

    #[tokio::test]
    async fn mapping_callbacks_use_zero_for_a_missing_method_on_a_key() {
        let result = run(
            "",
            OBJECTS,
            indoc! { r#"
                mixed *create() {
                    mapping obs = ([ "/other": 7, "/wearable": 8 ]);
                    mapping selected = filter(obs, &->query_wearable_item());
                    mapping mapped = map(obs, &->query_wearable_item());
                    return ({ sizeof(selected), selected["/wearable"], mapped["/other"], mapped["/wearable"] });
                }
            "# },
        )
        .await;
        assert_eq!(result, [1, 8, 0, 1].map(LpcRef::from));
    }

    #[tokio::test]
    async fn missing_methods_return_zero_to_other_callback_efuns() {
        let result = run(
            "",
            OBJECTS,
            indoc! { r#"
                mixed *create() {
                    function missing = papplyv(&->missing(), ({ this_object() }));
                    string *obs = ({ "/other", "/wearable", "/other" });
                    int *sorted = sort_array(({ 3, 1, 2 }), missing);
                    return filter_map(obs, &->query_wearable_item()) + sorted +
                        ({ reduce(({ 2, 3, 4 }), missing, 99) });
                }
            "# },
        )
        .await;
        assert_eq!(result, [1, 3, 1, 2, 0].map(LpcRef::from));
    }

    #[tokio::test]
    async fn consecutive_missing_callbacks_complete_without_recursive_calls() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                mixed *create() {
                    object *obs = allocate(10000, this_object());
                    return ({ sizeof(filter(obs, &->missing())), sizeof(map(obs, &->missing())) });
                }
            "# },
        )
        .await;
        assert_eq!(result, [0, 10000].map(LpcRef::from));
    }

    #[tokio::test]
    async fn dynamic_callback_errors_still_propagate_after_a_missing_method() {
        let err = fails(
            "",
            &[("/other.c", "void unrelated() {}")],
            indoc! { r#"
                int query_wearable_item() { throw("broken query"); return 0; }
                mixed *create() {
                    return filter(({ find_object("/other"), this_object() }), &->query_wearable_item());
                }
            "# },
        )
        .await;
        assert!(err.contains("broken query"), "{err}");
    }
}
