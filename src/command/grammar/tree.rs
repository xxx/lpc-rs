//! A parse: the derivation tree over one tokenized input, and the captures
//! frontends read out of it.

use std::{ops::Range, sync::Arc};

use super::{
    model::{Grammar, Label, ProdId},
    tokenizer::{Scan, Token},
};

/// One derivation node: which production, over which token span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node {
    /// The production this node derives from.
    pub production: ProdId,
    /// The token span this node covers.
    pub span: Range<usize>,
    /// The children, one per right-hand-side element of the production.
    pub children: Vec<Child>,
}

/// One child of a derivation node: a terminal or a nested nonterminal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Child {
    /// A terminal, by token index.
    Token(usize),
    /// A nested nonterminal derivation.
    Node(Node),
}

impl Child {
    /// The token span this child covers.
    pub fn span(&self) -> Range<usize> {
        match self {
            Child::Token(i) => *i..*i + 1,
            Child::Node(node) => node.span.clone(),
        }
    }
}

/// One derivation of an input under a grammar.
#[derive(Debug)]
pub struct Parse<'g> {
    grammar: &'g Grammar,
    scan: Arc<Scan>,
    root: Node,
}

impl<'g> Parse<'g> {
    /// Pair a derivation tree with the grammar and scan it was derived over.
    pub(crate) fn new(grammar: &'g Grammar, scan: Arc<Scan>, root: Node) -> Self {
        Parse {
            grammar,
            scan,
            root,
        }
    }

    /// The root node of this derivation.
    pub fn root(&self) -> &Node {
        &self.root
    }

    /// The grammar this parse was derived under.
    pub fn grammar(&self) -> &'g Grammar {
        self.grammar
    }

    /// The tokens of the input this parse was derived over.
    pub fn tokens(&self) -> &[Token] {
        self.scan.tokens()
    }

    /// The original text of token `i`.
    pub fn token_text(&self, i: usize) -> &str {
        self.scan.token_text(i)
    }

    /// The input under a token span with its spacing intact; an empty span is `""`.
    pub fn text(&self, span: Range<usize>) -> &str {
        if span.is_empty() {
            return "";
        }
        let tokens = self.scan.tokens();
        let start = tokens[span.start].range.start;
        let end = tokens[span.end - 1].range.end;
        &self.scan.input()[start..end]
    }

    /// Every labelled occurrence in the derivation, in pre-order, as token spans.
    pub fn capture_spans(&self) -> Vec<(Label, Range<usize>)> {
        let mut out = Vec::new();
        collect_captures(self.grammar, &self.root, &mut out);
        out
    }

    /// Every labelled occurrence in the derivation, in pre-order, as input text.
    pub fn captures(&self) -> Vec<(Label, &str)> {
        self.capture_spans()
            .into_iter()
            .map(|(label, span)| (label, self.text(span)))
            .collect()
    }
}

/// Walk `node` in pre-order, pushing every labelled element's span onto `out`.
fn collect_captures(grammar: &Grammar, node: &Node, out: &mut Vec<(Label, Range<usize>)>) {
    let rhs = &grammar.production(node.production).rhs;
    for (element, child) in rhs.iter().zip(&node.children) {
        if let Some(label) = element.label {
            out.push((label, child.span()));
        }
        if let Child::Node(inner) = child {
            collect_captures(grammar, inner, out);
        }
    }
}
