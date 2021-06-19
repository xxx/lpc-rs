use crate::{
    asm::instruction::Instruction, parser::span::Span, semantic::function_symbol::FunctionSymbol,
};
use rmp_serde::Serializer;
use serde::Serialize;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct Program {
    /// The actual program to execute
    pub instructions: Vec<Instruction>,

    /// The path to the file that this program was compiled from. Used for error messaging.
    pub filename: String,

    /// Code spans corresponding to instructions, for use in error messages
    pub debug_spans: Vec<Option<Span>>,

    /// jump labels
    pub labels: HashMap<String, usize>,

    /// function mapping of name to Symbol
    pub functions: HashMap<String, Rc<FunctionSymbol>>,

    /// How many globals does this program need storage for?
    pub num_globals: usize,
}

impl Program {
    /// Serialize the program to msgpack format, suitable for saving to disk.
    pub fn to_msgpack(&self) -> Vec<u8> {
        let mut buf = vec![];
        self.serialize(&mut Serializer::new(&mut buf)).unwrap();
        buf
    }

    /// Look up a function by its name
    pub fn lookup_function<T>(&self, name: T) -> Option<&Rc<FunctionSymbol>>
    where
        T: AsRef<str>,
    {
        self.functions.get(name.as_ref())
    }
}

impl From<Vec<u8>> for Program {
    /// Deserialize from msgpack data
    fn from(vec: Vec<u8>) -> Self {
        rmp_serde::from_slice(&vec).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::compile_string;

    #[test]
    fn test_serialization_and_deserialization() {
        let content = r#"
            int *foo = ({ 1, 2, 3, 4, 234 });
            void create() {
                foo = foo + ({ 666 });
                dump(foo);
            }
        "#;
        let prog = compile_string("foo.c", content.to_string()).unwrap();

        let msgpack = prog.to_msgpack();

        assert_eq!(Program::from(msgpack), prog);
    }
}
