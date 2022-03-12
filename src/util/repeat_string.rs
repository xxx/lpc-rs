use crate::{LpcError, LpcInt};

/// Repeat `s`, `i` times, and return a new String of it.
///
/// Return an error if there is an overflow.
pub fn repeat_string(s: &str, i: LpcInt) -> crate::Result<String> {
    if i >= 0 {
        let capacity = (i as usize).checked_mul(s.len());
        match capacity {
            Some(_) => Ok(s.repeat(i as usize)),
            None => Err(LpcError::new("capacity overflow in string repetition")),
        }
    } else {
        Ok(String::from(""))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use claim::*;

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
            "capacity overflow in string repetition"
        );
    }

    #[test]
    fn returns_empty_string_on_negative_amount() {
        let result = repeat_string("foo", -3);
        assert_ok!(result.clone());
        assert_eq!(result.unwrap().as_str(), "");
    }
}
