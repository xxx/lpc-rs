use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `intp`, an efun that returns true if the argument is an int.
pub async fn intp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let result = matches!(arg_ref, LpcRef::Int(_));

    context.return_efun_result(LpcRef::from(result));

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
        util::process_builder::ProcessInitializer,
    };

    #[tokio::test]
    async fn test_intp() {
        let master = indoc! { r#"
            int a = 123;
            float b = 123.456;
            string c = "hello";
            object d = this_object();
            object dd = find_object("/non-existent");
            int *e = ({ 1, 2, 3 });
            mapping f = ([ "foo": "bar" ]);
            function g = (: 1 :);

            mixed h = 123;
            mixed i = 123.456;
            mixed j = "hello";
            mixed k = this_object();
            mixed kk = find_object("/non-existent");
            mixed *l = ({ 1, 2, 3 });
            mixed m = ([ "foo": "bar" ]);
            mixed n = (: 1 :);

            int *create() {
                return ({
                    intp(a),
                    intp(b),
                    intp(c),
                    intp(d),
                    intp(dd),
                    intp(e),
                    intp(f),
                    intp(g),

                    intp(h),
                    intp(i),
                    intp(j),
                    intp(k),
                    intp(kk),
                    intp(l),
                    intp(m),
                    intp(n),
                });
            }
        "# };

        let vm = Vm::new(test_config());
        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap();

        let result = master_proc.result().unwrap();
        let LpcRef::Array(arr) = result else {
            panic!("Expected array result");
        };

        assert_eq!(
            &*arr.read(),
            [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0].as_slice()
        );
    }
}
