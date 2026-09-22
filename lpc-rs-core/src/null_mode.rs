use std::{fmt, str::FromStr};

/// The VM-wide representation of an absent LPC value.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NullMode {
    /// Preserve the traditional integer-zero representation.
    #[default]
    Zero,
    /// Represent absence with a distinct nil value.
    Distinct,
}

impl fmt::Display for NullMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Zero => "zero",
            Self::Distinct => "distinct",
        })
    }
}

impl FromStr for NullMode {
    type Err = &'static str;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "zero" => Ok(Self::Zero),
            "distinct" => Ok(Self::Distinct),
            _ => Err("NULL_MODE must be `zero` or `distinct`"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::NullMode;

    #[test]
    fn null_mode_names_are_explicit_and_default_to_zero() {
        assert_eq!(NullMode::default(), NullMode::Zero);
        assert_eq!("zero".parse(), Ok(NullMode::Zero));
        assert_eq!("distinct".parse(), Ok(NullMode::Distinct));
        for invalid in ["", "true", "DISTINCT", "2"] {
            assert!(invalid.parse::<NullMode>().is_err());
        }
        assert_eq!(NullMode::Distinct.to_string(), "distinct");
    }
}
