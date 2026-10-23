use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// All possible assignment operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignmentOperation {
    /// Simple assignment - `var = 2;`
    Simple,

    /// Plus-equals assignment - `a += 1`;
    AddEq,

    /// AndAnd-equals assignment - `a &&= 12`;
    AndAndEq,

    /// bitwise And-equals assignment - `a &= 12`;
    AndEq,

    /// Divide-equals assignment - `a /= 0`;
    DivEq,

    /// Index assigment - `a[2] = "hello";`
    Index,

    /// Modulo-equals assignment - `a %= 0`;
    ModEq,

    /// Times-equals assignment - `a *= 2`;
    MulEq,

    /// bitwise Or-equals assignment - `a |= 12`;
    OrEq,

    /// OrOr-equals assignment - `a ||= 12`;
    OrOrEq,

    /// shift-left-equals assignment - `a <<= 1`;
    ShlEq,

    /// shift-right-equals assignment - `a >>= 1`;
    ShrEq,

    /// Minus-equals assignment - `a -= 1`;
    SubEq,

    /// bitwise Xor-equals assignment - `a ^= 12`;
    XorEq,
}

impl Display for AssignmentOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignmentOperation::Simple => "=",
            AssignmentOperation::AddEq => "+=",
            AssignmentOperation::AndAndEq => "&&=",
            AssignmentOperation::AndEq => "&=",
            AssignmentOperation::DivEq => "/=",
            AssignmentOperation::Index => "[]",
            AssignmentOperation::ModEq => "%=",
            AssignmentOperation::MulEq => "*=",
            AssignmentOperation::OrEq => "|=",
            AssignmentOperation::OrOrEq => "||=",
            AssignmentOperation::ShlEq => "<<=",
            AssignmentOperation::ShrEq => ">>=",
            AssignmentOperation::SubEq => "-=",
            AssignmentOperation::XorEq => "^=",
        };

        write!(f, "{s}")
    }
}

/// A node representing an assignment.
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct AssignmentNode {
    /// Whether this assignment was desugared from a compound operator.
    pub is_compound: bool,

    /// left-hand side
    pub lhs: Box<ExpressionNode>,

    /// right-hand side
    pub rhs: Box<ExpressionNode>,

    /// The text span in the original file that this node represents. Used for
    /// error messages.
    pub span: Option<Span>,
}

impl SpannedNode for AssignmentNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for AssignmentNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_assignment(self).await
    }
}

impl Display for AssignmentNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    #[tokio::test]
    async fn indexed_compound_assignment_checks_the_class_loaded_from_an_alias() {
        let task = run_prog(
            r#"
            class entry { int value; }
            class other { int value; }
            int create() {
                class entry *items = ({new(class entry, value: 1)});
                mixed *alias = items;
                alias[0] = new(class other, value: 2);
                mixed problem = catch(items[0] ||= new(class entry, value: 3));
                return stringp(problem);
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_checks_the_class_before_storing() {
        let task = run_prog(
            r#"
            class entry { int value; }
            class other { int value; }
            int create() {
                class entry *items = allocate(1);
                mixed incoming = new(class other, value: 2);
                mixed problem = catch(items[0] ||= incoming);
                return stringp(problem) && !items[0];
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_evaluates_the_index_once() {
        let task = run_prog(
            r#"
            int calls;
            int next() { return calls++; }
            int create() {
                int *items = ({10, 20, 30});
                int result = (items[next()] += 1);
                return calls == 1 && result == 11 && items[0] == 11 && items[1] == 20;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_captures_owner_and_index_before_rhs() {
        let task = run_prog(
            r#"
            int *items = ({10, 20});
            int position;
            int rhs() { items = ({90, 91}); position = 1; return 5; }
            int create() {
                int *old = items;
                int result = (items[position] += rhs());
                return result == 15 && old[0] == 15 && old[1] == 20
                    && items[0] == 90 && items[1] == 91 && position == 1;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_evaluates_nested_targets_in_order() {
        let task = run_prog(
            r#"
            mapping rows = (["row": ({10})]);
            int order;
            mapping receiver() { order = order * 10 + 1; return rows; }
            string key() { order = order * 10 + 2; return "row"; }
            int position() { order = order * 10 + 3; return 0; }
            int rhs() { order = order * 10 + 4; return 2; }
            int create() {
                receiver()[key()][position()] += rhs();
                return order == 1234 && rows["row"][0] == 12;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_short_circuits_rhs_with_one_target_evaluation() {
        let task = run_prog(
            r#"
            int calls;
            int rhs() { calls++; return 9; }
            int create() {
                int *items = ({0, 7, 1, 0});
                int position;
                items[position++] &&= rhs();
                items[position++] ||= rhs();
                items[position++] &&= rhs();
                items[position++] ||= rhs();
                return position == 4 && calls == 2 && items[0] == 0
                    && items[1] == 7 && items[2] == 9 && items[3] == 9;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_preserves_arithmetic_and_bitwise_results() {
        for (op, rhs, expected) in [
            ("+=", 3, 15),
            ("-=", 4, 8),
            ("*=", 2, 24),
            ("/=", 2, 6),
            ("%=", 5, 2),
            ("<<=", 1, 24),
            (">>=", 1, 6),
            ("&=", 3, 0),
            ("|=", 3, 15),
            ("^=", 3, 15),
        ] {
            let code = format!(
                r#"
                int calls;
                int position() {{ calls++; return 0; }}
                int create() {{
                    int *items = ({{12}});
                    int result = (items[position()] {op} {rhs});
                    return calls == 1 && result == {expected} && items[0] == {expected};
                }}
            "#
            );
            assert_eq!(
                run_prog(&code).await.result(),
                Some(LpcRef::from(1)),
                "{op}"
            );
        }
    }

    #[tokio::test]
    async fn indexed_compound_assignment_evaluates_mapping_keys_once() {
        let task = run_prog(
            r#"
            int calls;
            string key() { return calls++ ? "right" : "left"; }
            int create() {
                mapping items = (["left": 8, "right": 20]);
                items[key()] *= 2;
                return calls == 1 && items["left"] == 16 && items["right"] == 20;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_evaluates_reverse_distances_once() {
        let task = run_prog(
            r#"
            int calls;
            int distance() { calls++; return 1; }
            int create() {
                int *items = ({10, 20});
                items[<distance()] += 5;
                return calls == 1 && items[0] == 10 && items[1] == 25;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn indexed_compound_assignment_leaves_element_unchanged_when_rhs_throws() {
        let task = run_prog(
            r#"
            int calls;
            int position() { calls++; return 0; }
            int rhs() { throw("fixture failure"); return 0; }
            int create() {
                int *items = ({10});
                mixed problem = catch(items[position()] += rhs());
                return stringp(problem) && calls == 1 && items[0] == 10;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
