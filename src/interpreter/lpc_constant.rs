use std::ops::Add;

#[derive(Debug, Clone, Eq, PartialEq)]
/// An actual LPC value. These are stored as constants in the ConstantPool.
pub enum LPCConstant {
    Int(i64),
    String(String)
}

impl From<&String> for LPCConstant {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl Add for &LPCConstant {
    type Output = LPCConstant;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCConstant::Int(i) => {
              match rhs {
                  LPCConstant::Int(i2) => LPCConstant::Int(i + i2),
                  _ => unimplemented!()
              }
            },
            LPCConstant::String(s) => {
                match rhs {
                    LPCConstant::String(s2) => LPCConstant::String(s.clone() + s2),
                    LPCConstant::Int(i) => LPCConstant::String(s.clone() + &i.to_string())
                }
            },
        }
    }
}