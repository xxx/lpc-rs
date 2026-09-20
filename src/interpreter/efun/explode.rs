use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

pub fn explode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let subject_ref = context.arg(0);
    let Some(subject) = subject_ref.as_str() else {
        return Ok(());
    };

    let delimiter_ref = context.arg(1);
    let delimiter = if delimiter_ref.is_null() {
        " "
    } else if let Some(delimiter) = delimiter_ref.as_str() {
        delimiter
    } else {
        return Ok(());
    };

    let parts: Vec<LpcRef> = if delimiter.is_empty() {
        subject
            .chars()
            .map(|character| LpcRef::from(character.to_string()))
            .collect()
    } else {
        subject.split(delimiter).map(LpcRef::from).collect()
    };
    context.return_array(parts);

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::test_support::run_prog;

    async fn result_of(expression: &str) -> Vec<String> {
        let task = run_prog(&format!("string *create() {{ return {expression}; }}")).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                arr.iter()
                    .map(|value| value.as_str().unwrap().to_owned())
                    .collect()
            })
            .unwrap()
    }

    #[tokio::test]
    async fn empty_delimiter_splits_without_boundary_fields() {
        assert_eq!(
            result_of(r#"explode("string", "")"#).await,
            ["s", "t", "r", "i", "n", "g"]
        );
    }

    #[tokio::test]
    async fn empty_delimiter_splits_unicode_scalar_values() {
        assert_eq!(
            result_of("explode(\"é中🐉e\u{301}\", \"\")").await,
            ["é", "中", "🐉", "e", "\u{301}"]
        );
    }

    #[tokio::test]
    async fn empty_delimiter_and_empty_input_return_an_empty_array() {
        assert!(result_of(r#"explode("", "")"#).await.is_empty());
    }

    #[tokio::test]
    async fn nonempty_delimiter_preserves_empty_fields() {
        assert_eq!(
            result_of(r#"explode(",a,,b,", ",")"#).await,
            ["", "a", "", "b", ""]
        );
    }

    #[tokio::test]
    async fn nonempty_delimiter_and_empty_input_return_one_empty_field() {
        assert_eq!(result_of(r#"explode("", ",")"#).await, [""]);
    }

    #[tokio::test]
    async fn omitted_delimiter_defaults_to_a_space() {
        assert_eq!(
            result_of(r#"explode("the quick brown  fox")"#).await,
            ["the", "quick", "brown", "", "fox"]
        );
    }
}
