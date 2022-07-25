use if_chain::if_chain;
use lpc_rs_core::LpcInt;
use lpc_rs_errors::{LpcError, Result};

/// The maximum length of strings, *in bytes*
pub const MAX_STRING_LENGTH: usize = 1_073_741_824; // 1 GiB

/// Repeat `s`, `i` times, and return a new String of it.
///
/// Return an error if there is an overflow.
pub fn repeat_string(s: &str, i: LpcInt) -> Result<String> {
    if i >= 0 {
        let new_capacity = (i as usize).checked_mul(s.as_bytes().len());
        if_chain! {
            if let Some(capacity) = new_capacity;
            if capacity <= MAX_STRING_LENGTH;
            then {
                Ok(s.repeat(i as usize))
            } else {
                Err(LpcError::new("overflow in string repetition"))
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
        Err(LpcError::new("overflow in string concatenation"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use claim::*;

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
            let result = repeat_string("foo", LpcInt::MAX);
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
            let s1 = repeat_string("a", 1_000_000_000).unwrap();
            let s2 = repeat_string("b", 1_000_000_000).unwrap();
            let result = concatenate_strings(s1, &s2);
            assert_eq!(
                result.unwrap_err().to_string().as_str(),
                "overflow in string concatenation"
            );
        }
    }
}
