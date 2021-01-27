use std::ops::Add;

/// An actual LPC value. These are stored as constants in the ConstantPool.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LPCValue {
    Int(i64),
    String(String)
}

impl From<&String> for LPCValue {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl Add for &LPCValue {
    type Output = LPCValue;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
              match rhs {
                  LPCValue::Int(i2) => LPCValue::Int(i + i2),
                  _ => unimplemented!()
              }
            },
            LPCValue::String(s) => {
                match rhs {
                    LPCValue::String(s2) => LPCValue::String(s.clone() + s2),
                    LPCValue::Int(i) => LPCValue::String(s.clone() + &i.to_string())
                }
            },
        }
    }
}