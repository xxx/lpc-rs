use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{arg_or_this_object, efun_context::EfunContext},
    lpc_ref::{LpcRef, NULL},
};

/// `function_exists(name [, ob])`: the file defining `name` in `ob` (the
/// caller when absent), as `file_name` spells it; 0 when there is none.
/// Another object's private and protected functions are hidden.
pub async fn function_exists<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "function_exists: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let Some(target) = arg_or_this_object(context.arg(1), context).await? else {
        context.return_efun_result(NULL);
        return Ok(());
    };
    let own = Arc::ptr_eq(&target, context.process());
    let lib_dir = context.config().lib_dir.as_str();
    let result = target
        .program
        .lookup_function(name)
        .filter(|function| own || function.prototype.flags.public())
        .map_or(NULL, |function| {
            let file = function.prototype.filename.as_in_game(lib_dir);
            LpcRef::from(file.with_extension("").display().to_string())
        });
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{run_prog, test_config},
    };

    async fn result_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    #[tokio::test]
    async fn function_exists_names_the_file_defining_the_function() {
        let r = result_of(r#"string create() { return function_exists("create"); }"#).await;
        assert_eq!(r.as_str(), Some("/my_file"));
    }

    #[tokio::test]
    async fn an_inherited_function_names_the_parent() {
        let code = r#"
            inherit "/grandparent";
            string create() { return function_exists("grandparent_method"); }
        "#;
        assert_eq!(result_of(code).await.as_str(), Some("/grandparent"));
    }

    #[tokio::test]
    async fn an_absent_function_is_zero() {
        let r = result_of(r#"mixed create() { return function_exists("nope"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn an_efun_is_not_a_function_of_the_object() {
        let r = result_of(r#"mixed create() { return function_exists("write"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn the_object_argument_is_searched_instead() {
        let code =
            r#"mixed create() { return function_exists("grandparent_method", "/grandparent"); }"#;
        assert_eq!(result_of(code).await.as_str(), Some("/grandparent"));
    }

    #[tokio::test]
    async fn a_private_function_of_another_object_is_hidden() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/hider.c", "private void hidden() {}")
            .await
            .unwrap();
        let code = r#"mixed create() { return function_exists("hidden", find_object("/hider")); }"#;
        let task = vm.initialize_process_from_code("/t.c", code).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn an_objects_own_private_function_is_found() {
        let code = r#"
            private void hidden() {}
            string create() { return function_exists("hidden"); }
        "#;
        assert_eq!(result_of(code).await.as_str(), Some("/my_file"));
    }

    #[tokio::test]
    async fn a_destructed_object_has_no_functions() {
        let code = r#"
            mixed create() {
                object o = clone_object("/clone_target");
                destruct(o);
                return function_exists("create", o);
            }
        "#;
        assert_eq!(result_of(code).await, LpcRef::from(0));
    }
}
