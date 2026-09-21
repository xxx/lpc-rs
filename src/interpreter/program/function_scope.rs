use std::{collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use lpc_rs_core::{EFUN, mangle::Mangle};
use ustr::{Ustr, ustr};

use super::Function;

/// Function names and inherit aliases as seen by one defining program.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct FunctionScope {
    // A private entry stops lookup without exposing the inherited definition.
    functions: HashMap<String, Option<Ustr>>,
    inherits: Box<[Arc<FunctionScope>]>,
    inherit_names: HashMap<String, usize>,
}

impl FunctionScope {
    pub(crate) fn new(
        functions: &IndexMap<String, Function, ahash::RandomState>,
        inherits: Vec<Arc<Self>>,
        inherit_names: HashMap<String, usize>,
    ) -> Self {
        Self {
            functions: functions
                .iter()
                .map(|(name, function)| {
                    (
                        name.clone(),
                        (!function.private()).then(|| ustr(&function.mangle())),
                    )
                })
                .collect(),
            inherits: inherits.into_boxed_slice(),
            inherit_names,
        }
    }

    pub(super) fn inherited_function(&self, namespace: &str, name: &str) -> Option<Ustr> {
        match namespace {
            "" => self
                .inherits
                .iter()
                .rev()
                .find_map(|parent| parent.functions.get(name).copied())
                .flatten(),
            EFUN => None,
            alias => self
                .inherit_names
                .get(alias)
                .and_then(|&index| self.inherits.get(index))
                .and_then(|parent| parent.functions.get(name).copied())
                .flatten(),
        }
    }
}
