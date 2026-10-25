use std::fmt::{Display, Formatter};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{ast_node::AstNodeTrait, block_node::BlockNode},
    codegen::tree_walker::TreeWalker,
};

/// A statement block evaluated for its catch result.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct CatchNode {
    pub body: BlockNode,
    pub span: Option<Span>,
}

#[async_trait]
impl AstNodeTrait for CatchNode {
    async fn visit(&mut self, walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        walker.visit_catch(self).await
    }
}

impl Display for CatchNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "catch {}", self.body)
    }
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    #[tokio::test]
    async fn catch_block_bodies_run_and_preserve_writes_before_errors() {
        for source in [
            r#"int create() {
                int value = 0;
                mixed caught = catch { value = 7; };
                return value == 7 && !caught;
            }"#,
            r#"int create() {
                int value = 0;
                mixed caught = catch { value = 7; throw("stopped"); value = 99; };
                return value == 7 && stringp(caught);
            }"#,
        ] {
            let task = run_prog(source).await;
            assert_eq!(task.context.result().unwrap(), LpcRef::from(1));
        }
    }
}
