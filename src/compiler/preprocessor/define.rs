use crate::compiler::{
    lexer::{Spanned, Token},
    preprocessor::preprocessor_node::PreprocessorNode,
};

/// An object macro, e.g. `#define FOO 123`
#[derive(Debug)]
pub struct ObjectMacro {
    /// The vector of Tokens being defined
    pub tokens: Vec<Spanned<Token>>,
    /// The preprocessor-parsed expression, for evaluating `#if` directives
    pub expr: PreprocessorNode,
}

/// A function macro, e.g. `#define FOO(a, b) (a * b)`
#[derive(Debug)]
pub struct FunctionMacro {
    /// The vector of Tokens being defined
    pub tokens: Vec<Spanned<Token>>,
    /// The vector of defined arguments, which are matched against during expansion for replacement
    pub args: Vec<String>,
}

/// Enumeration of `#define` types
#[derive(Debug)]
pub enum Define {
    Object(ObjectMacro),
    Function(FunctionMacro),
}

impl Define {
    /// Create a new object macro
    pub fn new_object(tokens: Vec<Spanned<Token>>, expr: PreprocessorNode) -> Self {
        Define::Object(ObjectMacro { tokens, expr })
    }

    /// Create a new function macro
    pub fn new_function(tokens: Vec<Spanned<Token>>, args: Vec<String>) -> Self {
        Define::Function(FunctionMacro { tokens, args })
    }
}
