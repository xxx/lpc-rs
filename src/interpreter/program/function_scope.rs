use std::{collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use lpc_rs_core::{EFUN, lpc_path::LpcPath, mangle::Mangle};
use ustr::{Ustr, ustr};

use super::Function;

/// Function names and inherit aliases as seen by one defining program.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct FunctionScope {
    basename: Option<Ustr>,
    // A private entry stops lookup without exposing the inherited definition.
    functions: HashMap<String, Option<Ustr>>,
    inherits: Box<[Arc<FunctionScope>]>,
    inherit_names: HashMap<String, usize>,
}

impl FunctionScope {
    pub(crate) fn new(
        filename: &LpcPath,
        functions: &IndexMap<String, Function, ahash::RandomState>,
        inherits: Vec<Arc<Self>>,
        inherit_names: HashMap<String, usize>,
    ) -> Self {
        Self {
            basename: filename
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| ustr(name.strip_suffix(".c").unwrap_or(name))),
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

    pub(crate) fn has_basename(&self, name: &str) -> bool {
        self.basename
            .is_some_and(|basename| basename.as_str() == name)
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
            alias if self.inherit_names.contains_key(alias) => self
                .inherit_names
                .get(alias)
                .and_then(|&index| self.inherits.get(index))
                .and_then(|parent| parent.functions.get(name).copied())
                .flatten(),
            basename => self
                .inherits
                .iter()
                .rev()
                .filter(|parent| parent.has_basename(basename))
                .find_map(|parent| parent.functions.get(name).copied())
                .flatten(),
        }
    }
}
