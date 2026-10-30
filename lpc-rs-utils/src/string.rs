use lpc_rs_core::{LpcIntInner, RegisterSize};
use lpc_rs_errors::{Result, lpc_error};

/// The maximum length of strings, *in bytes*
// pub const MAX_STRING_LENGTH: usize = 1_073_741_824; // 1 GiB
pub const MAX_STRING_LENGTH: usize = 8192;

/// Repeat `s`, `i` times, and return a new String of it.
///
/// Return an error if there is an overflow.
pub fn repeat_string(s: &str, i: LpcIntInner) -> Result<String> {
    if i <= 0 || s.is_empty() {
        return Ok(String::new());
    }

    if let Ok(count) = usize::try_from(i)
        && let Some(capacity) = count.checked_mul(s.len())
        && capacity <= MAX_STRING_LENGTH
    {
        Ok(s.repeat(count))
    } else {
        Err(lpc_error!("overflow in string repetition"))
    }
}

/// Concatenate `s1` and `s2`, and return a new String of it.
///
/// Return an error if there is an overflow.
pub fn concatenate_strings<T, U>(s1: T, s2: U) -> Result<String>
where
    T: Into<String>,
    U: AsRef<str>,
{
    let string1 = s1.into();
    let str2 = s2.as_ref();

    let new_capacity = string1.len() + str2.len();
    if new_capacity <= MAX_STRING_LENGTH {
        Ok(string1 + str2)
    } else {
        Err(lpc_error!("overflow in string concatenation"))
    }
}

/// Pull the number of out of `$1`, etc. closure argument variable references
pub fn closure_arg_number<T>(i: T) -> Result<RegisterSize>
where
    T: AsRef<str>,
{
    i.as_ref()
        .strip_prefix('$')
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| lpc_error!("invalid closure argument number: `{}`", i.as_ref()))
}

#[cfg(test)]
mod tests {
    use claims::*;

    use super::*;

    mod test_repeat_string {
        use super::*;

        #[test]
        fn the_limit_counts_utf8_bytes_and_accepts_the_exact_cap() {
            let count = LpcIntInner::try_from(MAX_STRING_LENGTH / 2).unwrap();

            assert_eq!(repeat_string("é", count).unwrap().len(), MAX_STRING_LENGTH);
            assert!(repeat_string("é", count + 1).is_err());
        }

        #[test]
        fn empty_results_do_not_depend_on_count_width() {
            for count in [LpcIntInner::MIN, -1, 0] {
                assert_eq!(repeat_string("value", count).unwrap(), "");
            }
            assert_eq!(repeat_string("", LpcIntInner::MAX).unwrap(), "");
        }

        #[test]
        fn large_positive_counts_cannot_wrap_on_narrow_hosts() {
            for count in [1_i64 << 32, LpcIntInner::MAX] {
                assert!(repeat_string("x", count).is_err());
            }
        }

        #[test]
        fn returns_ok_with_the_string() {
            let result = repeat_string("foo", 3);
            assert_ok!(result.clone());
            assert_eq!(result.unwrap().as_str(), "foofoofoo");
        }

        #[test]
        fn returns_err_on_overflow() {
            let result = repeat_string("foo", LpcIntInner::MAX);
            assert_err!(result.clone());
            assert_eq!(
                result.unwrap_err().to_string().as_str(),
                "overflow in string repetition"
            );
        }

        #[test]
        fn returns_empty_string_on_negative_amount() {
            let result = repeat_string("foo", -3);
            assert_ok!(result.clone());
            assert_eq!(result.unwrap().as_str(), "");
        }
    }

    mod test_concatenate_strings {
        use super::*;

        #[test]
        fn returns_concatenation() {
            let result = concatenate_strings("foo", "bar");
            assert_eq!(result.unwrap().as_str(), "foobar");
        }

        #[test]
        fn returns_error_on_overflow() {
            let s1 = repeat_string("a", 8192).unwrap();
            let s2 = repeat_string("b", 1).unwrap();
            let result = concatenate_strings(s1, s2);
            assert_eq!(
                result.unwrap_err().to_string().as_str(),
                "overflow in string concatenation"
            );
        }
    }

    mod test_closure_arg_number {
        use super::*;

        #[test]
        fn argument_numbers_respect_register_width_and_reject_malformed_text() {
            assert_eq!(
                closure_arg_number(format!("${}", RegisterSize::MAX)).unwrap(),
                RegisterSize::MAX
            );
            assert!(closure_arg_number(format!("${}", u32::from(RegisterSize::MAX) + 1)).is_err());
            for text in ["", "$", "1", "$-1", "$1x"] {
                assert!(closure_arg_number(text).is_err(), "{text}");
            }
        }

        #[test]
        fn returns_number() {
            assert_eq!(closure_arg_number("$1").unwrap(), 1);
            assert_eq!(closure_arg_number("$123").unwrap(), 123);
            assert_eq!(
                closure_arg_number("foobar").unwrap_err().to_string(),
                "invalid closure argument number: `foobar`"
            );
        }
    }
}
