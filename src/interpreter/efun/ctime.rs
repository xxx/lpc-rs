use chrono::{Local, TimeZone};
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// The fixed 24-character form, `Thu Sep  3 18:39:00 2026`.
const FORMAT: &str = "%a %b %e %H:%M:%S %Y";

/// `ctime([t])`: the time `t` (seconds since the epoch; now when absent)
/// in the local zone.
pub fn ctime<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let t = if context.arg_count() == 0 {
        Local::now().timestamp()
    } else {
        match context.arg(0) {
            LpcRef::Int(t) => t.0,
            other => {
                return Err(
                    context.runtime_error(format!("ctime: {} is not an int", other.type_name()))
                );
            }
        }
    };
    let Some(time) = Local.timestamp_opt(t, 0).single() else {
        return Err(context.runtime_error(format!("ctime: {t} is out of range")));
    };
    context.return_efun_result(LpcRef::from(time.format(FORMAT).to_string()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Local, NaiveDateTime, TimeZone};

    use crate::test_support::{run_prog, try_run_prog};

    const FORMAT: &str = "%a %b %e %H:%M:%S %Y";

    fn expected(t: i64) -> String {
        Local
            .timestamp_opt(t, 0)
            .unwrap()
            .format(FORMAT)
            .to_string()
    }

    async fn ctime_of(code: &str) -> String {
        let result = run_prog(code).await.result();
        result
            .as_ref()
            .and_then(|r| r.as_str())
            .unwrap_or_else(|| panic!("a string, actually {result:?}"))
            .to_owned()
    }

    #[tokio::test]
    async fn ctime_formats_a_time_in_the_local_zone() {
        let s = ctime_of("string create() { return ctime(1000000000); }").await;
        assert_eq!(s, expected(1_000_000_000));
    }

    #[tokio::test]
    async fn ctime_is_twenty_four_characters_with_the_day_space_padded() {
        // 1970-01-01, a single-digit day.
        let s = ctime_of("string create() { return ctime(0); }").await;
        assert_eq!(s.len(), 24, "{s:?}");
    }

    #[tokio::test]
    async fn ctime_of_zero_is_the_epoch_not_now() {
        let s = ctime_of("string create() { return ctime(0); }").await;
        assert_eq!(s, expected(0));
    }

    #[tokio::test]
    async fn ctime_without_an_argument_is_now() {
        let before = Local::now().timestamp();
        let s = ctime_of("string create() { return ctime(); }").await;
        let after = Local::now().timestamp();
        let parsed = NaiveDateTime::parse_from_str(&s, FORMAT).unwrap();
        let t = Local
            .from_local_datetime(&parsed)
            .earliest()
            .map(|d: DateTime<Local>| d.timestamp())
            .unwrap();
        assert!(
            (before..=after).contains(&t),
            "{s} not in {before}..={after}"
        );
    }

    #[tokio::test]
    async fn an_unrepresentable_time_is_an_error() {
        let code = "string create() { return ctime(9223372036854775807); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("ctime: 9223372036854775807 is out of range"),
            "{err}"
        );
    }
}
