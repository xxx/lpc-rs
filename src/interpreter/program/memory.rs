use std::{
    borrow::Cow,
    collections::HashSet,
    sync::{Arc, OnceLock},
};

use lpc_rs_core::{
    class::{ClassDefinition, ClassScope},
    lpc_path::LpcPath,
};
use lpc_rs_errors::Result;

use super::Program;
use crate::interpreter::memory::Size;

pub(super) fn fresh_cache(_: &OnceLock<Option<usize>>) -> OnceLock<Option<usize>> {
    OnceLock::new()
}

impl Program {
    pub(crate) fn storage_bytes(&self) -> Option<usize> {
        *self.memory_size.get_or_init(|| self.measure_storage().ok())
    }

    fn measure_storage(&self) -> Result<usize> {
        let mut size = Size::new(size_of::<Self>());
        size.items::<usize>(2)?;
        size.add(size_of_val(&*self.functions))?;
        size.items::<(ustr::Ustr, super::Function, usize, usize)>(self.functions.capacity())?;
        size.add(size_of_val(&*self.dispatch))?;
        size.items::<(ustr::Ustr, super::Target, usize, usize)>(self.dispatch.capacity())?;
        size.add(size_of_val(&*self.unmangled_functions))?;
        size.items::<(String, super::Function, usize, usize)>(self.unmangled_functions.capacity())?;
        for name in self.unmangled_functions.keys() {
            size.add(name.capacity())?;
        }
        size.add(size_of_val(&*self.global_variables))?;
        size.items::<(String, lpc_rs_function_support::symbol::Symbol, usize)>(
            self.global_variables.capacity(),
        )?;
        for (name, symbol) in self.global_variables.iter() {
            size.add(name.capacity())?;
            size.add(symbol.name.capacity())?;
        }
        size.add(size_of_val(&*self.global_variable_info))?;
        for variable in self.global_variable_info.iter() {
            size.add(variable.name.capacity())?;
        }
        size.add(size_of_val(&*self.direct_inherits))?;
        size.add(size_of_val(&*self.global_views))?;
        size.add(size_of_val(&*self.layout))?;
        size.add(size_of_val(&*self.inherited_function_scopes))?;
        size.items::<(Arc<LpcPath>, Arc<super::FunctionScope>, usize)>(
            self.inherited_function_scopes.capacity(),
        )?;

        let mut paths = HashSet::new();
        for path in std::iter::once(&self.filename)
            .chain(self.direct_inherits.iter())
            .chain(self.layout.iter().map(|region| &region.filename))
            .chain(
                self.global_variable_info
                    .iter()
                    .map(|variable| &variable.filename),
            )
        {
            add_path(&mut size, &mut paths, path)?;
        }
        let mut scopes = HashSet::new();
        self.function_scope.storage_bytes(&mut size, &mut scopes)?;
        for scope in self.inherited_function_scopes.values() {
            scope.storage_bytes(&mut size, &mut scopes)?;
        }
        let mut class_scopes = HashSet::new();
        let mut definitions = HashSet::new();
        add_classes(
            &mut size,
            &mut class_scopes,
            &mut definitions,
            &self.class_scope,
        )?;
        let mut code = HashSet::new();
        for function in self.functions.values().chain(self.initializer.iter()) {
            if !code.insert(Arc::as_ptr(&function.code) as usize) {
                continue;
            }
            size.items::<usize>(2)?;
            size.add(size_of_val(&*function.code))?;
            add_path(&mut size, &mut paths, &function.prototype.filename)?;
            if let Cow::Owned(name) = &function.prototype.name {
                size.add(name.capacity())?;
            }
            size.items::<lpc_rs_core::lpc_type::LpcType>(function.prototype.arg_types.capacity())?;
            size.items::<bool>(function.prototype.ref_params.capacity())?;
            size.items::<lpc_rs_errors::span::Span>(function.prototype.arg_spans.capacity())?;
            size.items::<lpc_rs_asm::instruction::Instruction>(function.instructions.capacity())?;
            size.items::<Option<lpc_rs_errors::span::Span>>(function.debug_spans.capacity())?;
            size.items::<lpc_rs_core::lpc_type::LpcType>(function.types.capacity())?;
            size.items::<lpc_rs_function_support::symbol::Symbol>(
                function.local_variables.capacity(),
            )?;
            for symbol in &function.local_variables {
                size.add(symbol.name.capacity())?;
            }
            size.items::<lpc_rs_core::register::RegisterVariant>(
                function.arg_locations.capacity(),
            )?;
            size.items::<lpc_rs_function_support::constant::LpcConstant>(
                function.constants.capacity(),
            )?;
            size.items::<Vec<lpc_rs_asm::instruction::Arg>>(function.arg_lists.capacity())?;
            for list in &function.arg_lists {
                size.items::<lpc_rs_asm::instruction::Arg>(list.capacity())?;
            }
            if let Some(labels) = &function.labels {
                size.items::<(
                    lpc_rs_asm::address::Label,
                    lpc_rs_asm::address::Address,
                    usize,
                )>(labels.capacity())?;
                for label in labels.keys() {
                    size.add(label.capacity())?;
                }
            }
            add_classes(
                &mut size,
                &mut class_scopes,
                &mut definitions,
                &function.class_scope,
            )?;
        }
        Ok(size.bytes())
    }
}

fn add_path(size: &mut Size, seen: &mut HashSet<usize>, path: &Arc<LpcPath>) -> Result<()> {
    if seen.insert(Arc::as_ptr(path) as usize) {
        size.items::<usize>(2)?;
        size.items::<LpcPath>(1)?;
        size.add(path.as_os_str().len())?;
    }
    Ok(())
}

fn add_classes(
    size: &mut Size,
    scopes: &mut HashSet<usize>,
    definitions: &mut HashSet<usize>,
    scope: &Arc<ClassScope>,
) -> Result<()> {
    if !scopes.insert(Arc::as_ptr(scope) as usize) {
        return Ok(());
    }
    size.items::<usize>(2)?;
    size.items::<ClassScope>(1)?;
    size.add(size_of_val(&*scope.definitions))?;
    size.items::<(
        lpc_rs_core::class::ClassId,
        lpc_rs_core::visibility::Visibility,
        usize,
    )>(scope.visibility.capacity())?;
    for definition in scope.definitions.iter() {
        if definitions.insert(Arc::as_ptr(definition) as usize) {
            size.items::<usize>(2)?;
            size.items::<ClassDefinition>(1)?;
            size.add(size_of_val(&*definition.fields))?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cached_statistics_do_not_change_program_equality_and_cloning_invalidates_them() {
        let original = Program::default();
        let mut changed = original.clone();
        let before = original.storage_bytes().unwrap();
        assert_eq!(original, changed);
        changed = original.clone();
        assert!(changed.memory_size.get().is_none());
        changed
            .global_variables
            .insert("large-name".repeat(1024), Default::default());
        assert!(changed.storage_bytes().unwrap() > before);
    }

    #[tokio::test]
    async fn repeated_code_bindings_count_the_executable_allocation_once() {
        let task = crate::test_support::run_prog("void create() {}").await;
        let mut shared = task.context.process.initial_program().as_ref().clone();
        let function = shared.functions.values().next().unwrap().clone();
        shared
            .functions
            .insert(ustr::ustr("another-view"), function.clone());
        let bytes = shared.storage_bytes().unwrap();
        let mut independent = shared.clone();
        independent
            .functions
            .get_mut(&ustr::ustr("another-view"))
            .unwrap()
            .code = Arc::new((*function.code).clone());
        assert!(independent.storage_bytes().unwrap() > bytes);
    }
}
