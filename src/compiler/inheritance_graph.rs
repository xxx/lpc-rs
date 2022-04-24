use std::path::Path;
use petgraph::algo::is_cyclic_directed;
use petgraph::data::Build;
use crate::{LpcError, Result};
use petgraph::graphmap::DiGraphMap;

pub struct InheritanceGraph<'a> {
    graph: DiGraphMap<&'a str, ()>,
}

impl<'a> InheritanceGraph<'a> {
    /// Create a new empty instance
    pub fn new() -> Self {
        Self {
            graph: DiGraphMap::new(),
        }
    }

    /// Build up a graph, starting with the root file being compiled
    pub fn build<P, Q>(&mut self, _path: P, _lib_dir: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        // parse file
        //   get list of inherits
        //   for each inherit
        //     link current file and inherit
        //     self.add(current_file, inherit);
        //     self.build(inherit_path);
        Ok(())
    }

    /// Add an inheritance relationship between two files
    pub fn add(&mut self, inheritor: &'a str, inheriting_from: &'a str) -> Result<()> {
        self.graph.update_edge(inheritor, inheriting_from, ());

        if is_cyclic_directed(&self.graph) {
            self.graph.remove_edge(inheritor, inheriting_from);
            return Err(
                LpcError::new(
                    format!("Cyclic inheritance detected: {} inheriting from {}", inheritor, inheriting_from)
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

        assert_ok!(graph.add("A", "B"));
        assert_ok!(graph.add("B", "C"));
        assert_ok!(graph.add("C", "D"));
        assert_ok!(graph.add("A", "C"));
        assert_ok!(graph.add("A", "E"));

        assert_err!(graph.add("A", "A"));
        assert_err!(graph.add("B", "A"));
    }
}