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
    pub(super) fn storage_bytes(
        &self,
        size: &mut crate::interpreter::memory::Size,
        seen: &mut std::collections::HashSet<usize>,
    ) -> lpc_rs_errors::Result<()> {
        let mut pending = vec![self];
        while let Some(scope) = pending.pop() {
            if !seen.insert(scope as *const Self as usize) {
                continue;
            }
            size.items::<usize>(2)?;
            size.items::<Self>(1)?;
            size.items::<(String, Option<Ustr>, usize)>(scope.functions.capacity())?;
            for name in scope.functions.keys() {
                size.add(name.capacity())?;
            }
            size.add(size_of_val(&*scope.inherits))?;
            size.items::<(String, usize, usize)>(scope.inherit_names.capacity())?;
            for name in scope.inherit_names.keys() {
                size.add(name.capacity())?;
            }
            pending.extend(scope.inherits.iter().map(AsRef::as_ref));
        }
        Ok(())
    }

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
