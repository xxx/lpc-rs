use if_chain::if_chain;
use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::{lpc_error, Result};

/// The maximum length of strings, *in bytes*
// pub const MAX_STRING_LENGTH: usize = 1_073_741_824; // 1 GiB
pub const MAX_STRING_LENGTH: usize = 8192;

/// Repeat `s`, `i` times, and return a new String of it.
///
/// Return an error if there is an overflow.
pub fn repeat_string(s: &str, i: LpcIntInner) -> Result<String> {
    if i >= 0 {
        let new_capacity = (i as usize).checked_mul(s.as_bytes().len());
        if_chain! {
            if let Some(capacity) = new_capacity;
            if capacity <= MAX_STRING_LENGTH;
            then {
                Ok(s.repeat(i as usize))
            } else {
                Err(lpc_error!("overflow in string repetition"))
            }
        }
    } else {
        Ok(String::from(""))
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

    let new_capacity = string1.as_bytes().len() + str2.len();
    if new_capacity <= MAX_STRING_LENGTH {
        Ok(string1 + str2)
    } else {
        Err(lpc_error!("overflow in string concatenation"))
    }
}

/// Pull the number of out of `$1`, etc. closure argument variable references
pub fn closure_arg_number<T>(i: T) -> Result<usize>
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
