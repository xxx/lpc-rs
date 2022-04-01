use petgraph::algo::is_cyclic_directed;
use petgraph::data::Build;
use crate::{LpcError, Result};
use petgraph::graphmap::DiGraphMap;

pub struct InheritanceGraph<'a> {
    graph: DiGraphMap<&'a str, ()>,
}

impl<'a> InheritanceGraph<'a> {
    pub fn new() -> Self {
        Self {
            graph: DiGraphMap::new(),
        }
    }

    /// Add an inheritance relationship between two files
    pub fn add_inheritance(&mut self, parent: &'a str, child: &'a str) -> Result<()> {
        self.graph.update_edge(parent, child, ());

        if is_cyclic_directed(&self.graph) {
            self.graph.remove_edge(parent, child);
            return Err(
                LpcError::new(
                    format!("Cyclic inheritance detected: {} inheriting from {}", child, parent)
                )
            );
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use claim::{assert_err, assert_ok};
    use super::*;

    #[test]
    fn test_add_inheritance() {
        let mut graph = InheritanceGraph::new();

        assert_ok!(graph.add_inheritance("A", "B"));
        assert_ok!(graph.add_inheritance("B", "C"));
        assert_ok!(graph.add_inheritance("C", "D"));
        assert_ok!(graph.add_inheritance("A", "C"));
        assert_ok!(graph.add_inheritance("A", "E"));

        assert_err!(graph.add_inheritance("A", "A"));
        assert_err!(graph.add_inheritance("B", "A"));
    }
}