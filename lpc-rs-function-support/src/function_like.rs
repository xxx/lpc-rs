use crate::{function_prototype::FunctionPrototype, program_function::ProgramFunction};
use std::rc::Rc;

/// A facade to allow us to lookup prototypes or compiled functions
#[derive(Debug, PartialEq)]
pub enum FunctionLike<'a> {
    Prototype(&'a FunctionPrototype),
    Compiled(Rc<ProgramFunction>),
}

impl<'a> FunctionLike<'a> {
    /// Get a reference to the [`FunctionPrototype`]
    pub fn prototype(&'a self) -> &'a FunctionPrototype {
        match self {
            FunctionLike::Prototype(prototype) => prototype,
            FunctionLike::Compiled(program_function) => &program_function.prototype,
        }
    }
}

impl<'a> AsRef<FunctionPrototype> for FunctionLike<'a> {
    fn as_ref(&self) -> &FunctionPrototype {
        self.prototype()
    }
}

impl<'a> From<&'a FunctionPrototype> for FunctionLike<'a> {
    fn from(prototype: &'a FunctionPrototype) -> Self {
        FunctionLike::Prototype(prototype)
    }
}

impl<'a> From<Rc<ProgramFunction>> for FunctionLike<'a> {
    fn from(func: Rc<ProgramFunction>) -> Self {
        FunctionLike::Compiled(func)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lpc_rs_core::{
        function_arity::FunctionArity, function_flags::FunctionFlags, lpc_type::LpcType,
    };

    #[test]
    fn test_prototype() {
        let prototype = FunctionPrototype::new(
            "foo",
            LpcType::Int(false),
            FunctionArity::default(),
            FunctionFlags::default(),
            None,
            vec![],
            vec![],
        );
        let function_like = FunctionLike::from(&prototype);
        assert_eq!(function_like.prototype(), &prototype);

        let function_like = FunctionLike::from(Rc::new(ProgramFunction::new(prototype.clone(), 0)));
        assert_eq!(function_like.prototype(), &prototype);
    }
}
