use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use crate::compiler::{ast::ast_node::AstNodeTrait, codegen::tree_walker::TreeWalker};

use super::parsed_type::ParsedType;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct ClassFieldNode {
    pub name: Ustr,
    pub type_: ParsedType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct ClassDefNode {
    pub name: Ustr,
    pub fields: Vec<ClassFieldNode>,
    pub span: Span,
}

#[async_trait]
impl AstNodeTrait for ClassDefNode {
    async fn visit(&mut self, walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        walker.visit_class_def(self).await
    }
}
