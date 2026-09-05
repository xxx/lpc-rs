//! `localtime` and `strftime`: a time in the driver's local zone, as
//! LDMud's nine-int array or formatted with C's `strftime` conversions.

use std::fmt::Write;

use chrono::{DateTime, Datelike, Local, TimeZone, Timelike, format::strftime::StrftimeItems};
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Argument `i` as a local time, seconds since the epoch; now when absent.
fn local_time<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    i: usize,
) -> Result<DateTime<Local>> {
    if context.arg_count() <= i {
        return Ok(Local::now());
    }
    let LpcRef::Int(t) = context.arg(i) else {
        return Err(context.runtime_error(format!(
            "{name}: {} is not an int",
            context.arg(i).type_name()
        )));
    };
    let t = t.0;
    Local
        .timestamp_opt(t, 0)
        .single()
        .ok_or_else(|| context.runtime_error(format!("{name}: {t} is out of range")))
}

/// Whether `d` is on daylight time: its offset exceeds the zone's standard
/// offset, the smaller of the offsets on January 1 and July 1 of its year.
fn is_dst(d: &DateTime<Local>) -> bool {
    let offset = d.offset().local_minus_utc();
    let standard = [1, 7]
        .into_iter()
        .filter_map(|month| {
            Local
                .with_ymd_and_hms(d.year(), month, 1, 12, 0, 0)
                .single()
        })
        .map(|x| x.offset().local_minus_utc())
        .min()
        .unwrap_or(offset);
    offset > standard
}

/// `localtime([t])`: `({ sec, min, hour, mday, mon, year, wday, yday, isdst })`,
/// LDMud's order, month and year-day counted from 0.
pub fn localtime<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let d = local_time(context, "localtime", 0)?;
    let fields = [
        i64::from(d.second()),
        i64::from(d.minute()),
        i64::from(d.hour()),
        i64::from(d.day()),
        i64::from(d.month0()),
        i64::from(d.year()),
        i64::from(d.weekday().num_days_from_sunday()),
        i64::from(d.ordinal0()),
        i64::from(is_dst(&d)),
    ];
    context.return_array(fields.into_iter().map(LpcRef::from));
    Ok(())
}

/// `strftime(fmt [, t])`: `t` formatted with C's `%` conversions; an
/// unknown conversion is an error.
pub fn strftime<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(fmt) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "strftime: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let d = local_time(context, "strftime", 1)?;
    let Ok(items) = StrftimeItems::new(fmt).parse() else {
        return Err(context.runtime_error(format!("strftime: invalid format {fmt:?}")));
    };
    let mut result = String::new();
    if write!(result, "{}", d.format_with_items(items.iter())).is_err() {
        return Err(context.runtime_error(format!("strftime: cannot format {fmt:?}")));
    }
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

#[cfg(test)]
mod tests {
    use chrono::{Datelike, Local, TimeZone, Timelike};

    use crate::test_support::{run_prog, strings_of, try_run_prog};

    async fn string_of(expr: &str) -> String {
        let code = format!("string create() {{ return {expr}; }}");
        let result = run_prog(&code).await.result();
        result
            .as_ref()
            .and_then(|r| r.as_str())
            .unwrap_or_else(|| panic!("{expr}: a string, actually {result:?}"))
            .to_owned()
    }

    async fn error_of(expr: &str) -> String {
        let code = format!("mixed create() {{ return {expr}; }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn localtime_is_the_nine_ldmud_fields_in_the_local_zone() {
        let t = 1_000_000_000;
        let d = Local.timestamp_opt(t, 0).unwrap();
        let fields = strings_of(&format!("int *create() {{ return localtime({t}); }}")).await;
        let expected = [
            d.second(),
            d.minute(),
            d.hour(),
            d.day(),
            d.month0(),
            d.year() as u32,
            d.weekday().num_days_from_sunday(),
            d.ordinal0(),
        ]
        .map(|n| n.to_string());
        assert_eq!(&fields[..8], &expected);
        assert!(fields[8] == "0" || fields[8] == "1", "{fields:?}");
    }

    #[tokio::test]
    async fn localtime_without_an_argument_is_now() {
        let before = Local::now().year();
        let fields = strings_of("int *create() { return localtime(); }").await;
        let after = Local::now().year();
        assert_eq!(fields.len(), 9);
        let year: i32 = fields[5].parse().unwrap();
        assert!((before..=after).contains(&year), "{fields:?}");
    }

    #[tokio::test]
    async fn localtime_of_an_unrepresentable_time_is_an_error() {
        let err = error_of("localtime(9223372036854775807)").await;
        assert!(
            err.contains("localtime: 9223372036854775807 is out of range"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn localtime_of_a_non_int_is_an_error() {
        let code = r#"mixed create() { mixed s = "a"; return localtime(s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("localtime: string is not an int"), "{err}");
    }

    #[tokio::test]
    async fn strftime_formats_a_time_with_c_conversions() {
        let t = 1_000_000_000;
        let d = Local.timestamp_opt(t, 0).unwrap();
        let s = string_of(&format!(r#"strftime("%Y-%m-%d %H:%M:%S %a %j", {t})"#)).await;
        assert_eq!(s, d.format("%Y-%m-%d %H:%M:%S %a %j").to_string());
    }

    #[tokio::test]
    async fn strftime_copies_ordinary_characters_and_a_doubled_percent() {
        let s = string_of(r#"strftime("100%% done", 0)"#).await;
        assert_eq!(s, "100% done");
    }

    #[tokio::test]
    async fn strftime_without_a_time_is_now() {
        let before = Local::now().year();
        let s = string_of(r#"strftime("%Y")"#).await;
        let after = Local::now().year();
        let year: i32 = s.parse().unwrap();
        assert!((before..=after).contains(&year), "{s}");
    }

    #[tokio::test]
    async fn strftime_with_an_unknown_conversion_is_an_error() {
        let err = error_of(r#"strftime("%Q", 0)"#).await;
        assert!(err.contains(r#"strftime: invalid format "%Q""#), "{err}");
    }

    #[tokio::test]
    async fn strftime_of_an_unrepresentable_time_is_an_error() {
        let err = error_of(r#"strftime("%Y", 9223372036854775807)"#).await;
        assert!(
            err.contains("strftime: 9223372036854775807 is out of range"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn strftime_arguments_are_typed() {
        let code = r#"mixed create() { mixed n = 1; return strftime(n); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("strftime: int is not a string"), "{err}");
        let code = r#"mixed create() { mixed s = "a"; return strftime("%Y", s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("strftime: string is not an int"), "{err}");
    }

    #[tokio::test]
    async fn strftime_knows_the_zone_conversions() {
        let d = Local.timestamp_opt(0, 0).unwrap();
        let s = string_of(r#"strftime("%z %Z %c", 0)"#).await;
        assert_eq!(s, d.format("%z %Z %c").to_string());
    }
}
