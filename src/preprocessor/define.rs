use crate::{
    parser::lexer::{Spanned, Token},
    preprocessor::preprocessor_node::PreprocessorNode,
};

#[derive(Debug)]
pub struct ObjectMacro {
    pub tokens: Vec<Spanned<Token>>,
    pub expr: PreprocessorNode,
}

#[derive(Debug)]
pub struct FunctionMacro {
    pub tokens: Vec<Spanned<Token>>,
    pub args: Vec<String>,
}

#[derive(Debug)]
pub enum Define {
    Object(ObjectMacro),
    Function(FunctionMacro),
}

impl Define {
    pub fn new_object(tokens: Vec<Spanned<Token>>, expr: PreprocessorNode) -> Self {
        Define::Object(ObjectMacro { tokens, expr })
    }

    pub fn new_function(tokens: Vec<Spanned<Token>>, args: Vec<String>) -> Self {
        Define::Function(FunctionMacro { tokens, args })
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Define::Object(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Define::Function(_))
    }
}
