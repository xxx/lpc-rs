use std::{collections::HashMap, sync::Arc};

use async_trait::async_trait;
use lpc_rs_core::{
    class::{ClassDefinition, ClassField, ClassId, ClassScope},
    lpc_type::LpcType,
};
use lpc_rs_errors::{Result, lpc_bug, lpc_error, span::Span};
use ustr::{Ustr, ustr};

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        cast_node::CastNode,
        closure_node::ClosureNode,
        decl_node::DeclNode,
        function_def_node::FunctionDefNode,
        parsed_type::ParsedType,
        program_node::ProgramNode,
        var_init_node::VarInitNode,
    },
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
};

use super::tree_walker::{
    ContextHolder, Pass, TreeWalker, walk_cast, walk_closure, walk_decl, walk_function_def,
    walk_var_init,
};

/// Resolve nominal types after inheritance and before symbols or prototypes are built.
pub struct ClassWalker {
    context: CompilationContext,
    names: HashMap<Ustr, Vec<ClassId>>,
}

impl ClassWalker {
    fn expose(&mut self, id: ClassId) {
        let ids = self.names.entry(id.name).or_default();
        if !ids.contains(&id) {
            ids.push(id);
            ids.sort_unstable_by(|left, right| left.source.as_str().cmp(right.source.as_str()));
        }
    }

    fn identity(&self, name: Ustr, span: Span) -> Result<ClassId> {
        let sources = self.context.diagnostic_sources.read();
        let file = sources
            .get(span.file_id())
            .map_err(|_| lpc_bug!(Some(span), "class declaration has no source"))?;
        let source = file.name();
        if !source.starts_with('/') {
            return Err(lpc_error!(
                Some(span),
                "class definitions require a source inside the mudlib"
            ));
        }
        Ok(ClassId {
            source: ustr(source),
            name,
        })
    }

    fn resolve(&mut self, type_: &mut ParsedType) -> Result<()> {
        let ParsedType::Class { name, array, span } = *type_ else {
            return Ok(());
        };
        let result = match self.names.get(&name).map(Vec::as_slice) {
            Some([id]) => Ok(LpcType::Class(*id, array)),
            Some(ids) => Err(
                lpc_error!(Some(span), "ambiguous class name `{name}`").with_note(format!(
                    "defined in {}",
                    ids.iter()
                        .map(|id| id.source.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
            ),
            None => Err(lpc_error!(Some(span), "unknown class `{name}`")),
        };
        match result {
            Ok(resolved) => {
                *type_ = resolved.into();
                Ok(())
            }
            Err(error) => Err(self.context.diagnostics.fail(error)),
        }
    }

    fn insert_definition(
        definitions: &mut HashMap<ClassId, Arc<ClassDefinition>>,
        definition: Arc<ClassDefinition>,
        span: Option<Span>,
    ) -> Result<()> {
        if let Some(previous) = definitions.get(&definition.id) {
            if previous != &definition {
                return Err(lpc_error!(
                    span,
                    "conflicting definitions of {}",
                    definition.id
                ));
            }
        } else {
            definitions.insert(definition.id, definition);
        }
        Ok(())
    }
}

impl ContextHolder for ClassWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for ClassWalker {
    fn new(context: CompilationContext) -> Self {
        Self {
            context,
            names: HashMap::new(),
        }
    }
    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for ClassWalker {
    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        let mut definitions = HashMap::new();
        for (parent, inherit) in self.context.inherits.iter().zip(&node.inherits) {
            for definition in &parent.class_scope.definitions {
                Self::insert_definition(&mut definitions, definition.clone(), inherit.span)?;
            }
        }
        for (parent, inherit) in self.context.inherited_interfaces.iter().zip(&node.inherits) {
            for definition in &parent.class_scope.definitions {
                Self::insert_definition(&mut definitions, definition.clone(), inherit.span)?;
            }
        }
        for id in definitions.keys() {
            self.expose(*id);
        }

        let mut own = HashMap::new();
        for declaration in &node.body {
            if let AstNode::ClassDef(class) = declaration {
                let id = self.identity(class.name, class.span)?;
                if let Some(previous) = own.insert(id, class.span) {
                    return Err(lpc_error!(Some(class.span), "duplicate definition of {id}")
                        .with_label("previous definition", Some(previous)));
                }
                self.expose(id);
            }
        }

        for declaration in &mut node.body {
            if let AstNode::ClassDef(class) = declaration {
                let id = self.identity(class.name, class.span)?;
                let mut fields = Vec::with_capacity(class.fields.len());
                let mut names = HashMap::new();
                for field in &mut class.fields {
                    if let Some(previous) = names.insert(field.name, field.span) {
                        return Err(lpc_error!(
                            Some(field.span),
                            "duplicate class field `{}`",
                            field.name
                        )
                        .with_label("previous field", Some(previous)));
                    }
                    self.resolve(&mut field.type_)?;
                    let type_ = field.type_.resolved()?;
                    if type_ == LpcType::Void {
                        return Err(lpc_error!(
                            Some(field.span),
                            "class field `{}` cannot be `void`",
                            field.name
                        ));
                    }
                    fields.push(ClassField {
                        name: field.name,
                        type_,
                    });
                }
                Self::insert_definition(
                    &mut definitions,
                    Arc::new(ClassDefinition {
                        id,
                        fields: fields.into_boxed_slice(),
                    }),
                    Some(class.span),
                )?;
            }
        }

        let mut definitions: Vec<_> = definitions.into_values().collect();
        definitions.sort_unstable_by(|left, right| {
            (left.id.source.as_str(), left.id.name.as_str())
                .cmp(&(right.id.source.as_str(), right.id.name.as_str()))
        });
        self.context.class_scope = Arc::new(ClassScope {
            definitions: definitions.into_boxed_slice(),
        });

        for declaration in &mut node.body {
            declaration.visit(self).await?;
        }
        Ok(())
    }

    async fn visit_class_init(
        &mut self,
        node: &mut crate::compiler::ast::class_expression::ClassInitNode,
    ) -> Result<()> {
        self.resolve(&mut node.type_)?;
        super::tree_walker::walk_class_init(self, node).await
    }

    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        self.resolve(&mut node.type_)?;
        walk_decl(self, node).await
    }

    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        self.resolve(&mut node.type_)?;
        walk_var_init(self, node).await
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        self.resolve(&mut node.return_type)?;
        walk_function_def(self, node).await
    }

    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        self.resolve(&mut node.return_type)?;
        walk_closure(self, node).await
    }

    async fn visit_cast(&mut self, node: &mut CastNode) -> Result<()> {
        self.resolve(&mut node.type_)?;
        walk_cast(self, node).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::Compiler,
        test_support::{lib_holding, run_prog, temp_lib_config},
    };

    #[tokio::test]
    async fn scoped_diagnostic_sources_resolve_class_identity() {
        let root = lib_holding(
            "class-scoped",
            &[("record.h", "class record { int value; }")],
        );
        let compiler = crate::compiler::CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .diagnostic_sources(lpc_rs_errors::source_map::DiagnosticSources::scoped())
            .build()
            .unwrap();
        let compiled = compiler
            .compile_string("/object.c", "#include \"record.h\"")
            .await
            .unwrap();
        assert_eq!(
            compiled.program.class_scope.definitions[0].id.source,
            ustr("/record.h")
        );
    }

    #[tokio::test]
    async fn outside_host_sources_cannot_share_a_placeholder_class_identity() {
        let root = lib_holding("class-outside", &[]);
        let compiler = Compiler::new(temp_lib_config(&root));
        let path = lpc_rs_core::lpc_path::LpcPath::new_server(root.with_extension("outside.c"));
        let error = compiler
            .compile_string(path, "class record {}")
            .await
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "class definitions require a source inside the mudlib"
        );
    }

    #[tokio::test]
    async fn class_fields_preserve_names_types_and_declaration_order() {
        let compiler = Compiler::default();
        let compiled = compiler
            .compile_string(
                "/records.c",
                "class record { int count, *counts; mixed data; function callback; };",
            )
            .await
            .unwrap();
        let definition = &compiled.program.class_scope.definitions[0];
        assert_eq!(
            definition.id,
            ClassId {
                source: ustr("/records.c"),
                name: ustr("record")
            }
        );
        let fields: Vec<_> = definition
            .fields
            .iter()
            .map(|field| (field.name.as_str(), field.type_))
            .collect();
        assert_eq!(
            fields,
            vec![
                ("count", LpcType::Int(false)),
                ("counts", LpcType::Int(true)),
                ("data", LpcType::Mixed(false)),
                ("callback", LpcType::Function(false))
            ]
        );
    }

    #[tokio::test]
    async fn recursive_and_forward_class_types_resolve_before_function_signatures() {
        let compiled = Compiler::default()
            .compile_string(
                "/records.c",
                r#"
            class first answer(class second value) { return 0; }
            class first { class first next; class second *children; }
            class second { class first parent; }
            class second *records;
        "#,
            )
            .await
            .unwrap();
        let first = ClassId {
            source: ustr("/records.c"),
            name: ustr("first"),
        };
        let second = ClassId {
            source: ustr("/records.c"),
            name: ustr("second"),
        };
        let definition = compiled.program.class_scope.get(first).unwrap();
        assert_eq!(definition.fields[0].type_, LpcType::Class(first, false));
        assert_eq!(definition.fields[1].type_, LpcType::Class(second, true));
        let function = compiled.program.lookup_function("answer").unwrap();
        assert_eq!(function.return_type(), LpcType::Class(first, false));
        assert_eq!(
            function.prototype.arg_types,
            vec![LpcType::Class(second, false)]
        );
        assert_eq!(
            compiled.program.global_variables["records"].type_,
            LpcType::Class(second, true)
        );
    }

    #[tokio::test]
    async fn includes_and_diamond_inheritance_preserve_one_nominal_identity() {
        let root = lib_holding(
            "class-diamond",
            &[
                ("records.h", "class record { int value; }"),
                (
                    "base.c",
                    "#include \"records.h\"\nclass record get() { return 0; }",
                ),
                ("left.c", "inherit \"/base\";"),
                ("right.c", "inherit \"/base\";"),
            ],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let child = compiler.compile_string("/child.c", "inherit \"/left\"; inherit \"/right\";\n#include \"records.h\"\nclass record data;").await.unwrap();
        let other = compiler
            .compile_string("/other.c", "#include \"records.h\"\nclass record data;")
            .await
            .unwrap();
        assert_eq!(child.program.class_scope.definitions.len(), 1);
        assert_eq!(child.program.class_scope, other.program.class_scope);
        assert_eq!(
            child.program.class_scope.definitions[0].id.source,
            ustr("/records.h")
        );
    }

    #[tokio::test]
    async fn source_normalization_and_line_changes_do_not_change_class_identity() {
        let root = lib_holding("class-source", &[]);
        let compiler = Compiler::new(temp_lib_config(&root));
        let first = compiler
            .compile_string("/dir/../record.c", "class record { int x; }")
            .await
            .unwrap();
        let second = compiler
            .compile_string("/record.c", "\n\nclass record { string x; }")
            .await
            .unwrap();
        assert_eq!(
            first.program.class_scope.definitions[0].id,
            second.program.class_scope.definitions[0].id
        );
        assert_ne!(first.program.class_scope, second.program.class_scope);
    }

    #[tokio::test]
    async fn same_name_and_shape_in_other_files_are_distinct() {
        let compiler = Compiler::default();
        let first = compiler
            .compile_string("/a.c", "class record { int x; }")
            .await
            .unwrap();
        let second = compiler
            .compile_string("/b.c", "class record { int x; }")
            .await
            .unwrap();
        assert_ne!(
            first.program.class_scope.definitions[0].id,
            second.program.class_scope.definitions[0].id
        );
    }

    #[tokio::test]
    async fn macro_generated_classes_belong_to_the_invocation_source() {
        let root = lib_holding(
            "class-macro",
            &[("decl.h", "#define RECORD class record { int value; }\n")],
        );
        let compiled = Compiler::new(temp_lib_config(&root))
            .compile_string(
                "/user.c",
                "#include \"decl.h\"\nRECORD\nclass record value;",
            )
            .await
            .unwrap();
        assert_eq!(
            compiled.program.class_scope.definitions[0].id.source,
            ustr("/user.c")
        );
    }

    #[tokio::test]
    async fn ambiguous_inherited_class_names_are_diagnosed_at_use() {
        let root = lib_holding(
            "class-ambiguous",
            &[
                ("left.c", "class record { int value; }"),
                ("right.c", "class record { int value; }"),
            ],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let inherits = "inherit \"/left\"; inherit \"/right\";";
        let program = compiler
            .compile_string("/unused.c", inherits)
            .await
            .unwrap();
        assert_eq!(program.program.class_scope.definitions.len(), 2);
        let error = compiler
            .compile_string("/used.c", format!("{inherits} class record value;"))
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "ambiguous class name `record`");
    }

    #[tokio::test]
    async fn incompatible_inherited_views_of_one_class_are_rejected() {
        let root = lib_holding(
            "class-conflict",
            &[
                ("record.h", "class record { FIELD value; }"),
                ("left.c", "#define FIELD int\n#include \"record.h\""),
                ("right.c", "#define FIELD string\n#include \"record.h\""),
            ],
        );
        let error = Compiler::new(temp_lib_config(&root))
            .compile_string("/child.c", "inherit \"/left\"; inherit \"/right\";")
            .await
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "conflicting definitions of class record (/record.h)"
        );
    }

    #[tokio::test]
    async fn repeated_local_definitions_are_rejected() {
        let error = Compiler::default()
            .compile_string("/record.c", "class record {} class record {}")
            .await
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "duplicate definition of class record (/record.c)"
        );
    }

    #[tokio::test]
    async fn duplicate_field_names_are_rejected_even_when_the_types_differ() {
        let error = Compiler::default()
            .compile_string("/record.c", "class record { int value; string value; }")
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "duplicate class field `value`");
    }

    #[tokio::test]
    async fn void_class_fields_are_rejected() {
        let error = Compiler::default()
            .compile_string("/record.c", "class record { void value; }")
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "class field `value` cannot be `void`");
    }

    #[tokio::test]
    async fn unknown_class_names_are_reported_in_fields_and_nested_code() {
        for code in [
            "class record { class missing child; }",
            "void f() { if (1) { class missing value; } }",
            "mixed f(mixed value) { return (class missing)value; }",
        ] {
            let error = Compiler::default()
                .compile_string("/record.c", code)
                .await
                .unwrap_err();
            assert_eq!(error.to_string(), "unknown class `missing`");
        }
    }

    #[tokio::test]
    async fn nominal_types_reject_same_shaped_assignments_arguments_and_returns() {
        let declarations = "class first { int x; } class second { int x; }";
        for body in [
            "void f(class first a, class second b) { a = b; }",
            "void take(class first a) {} void f(class second b) { take(b); }",
            "class first f(class second b) { return b; }",
            "class first f(class second b) { return (class first)b; }",
        ] {
            let error = Compiler::default()
                .compile_string("/record.c", format!("{declarations} {body}"))
                .await
                .unwrap_err();
            let message = error.to_string();
            assert!(
                message.contains("class first (/record.c)")
                    && message.contains("class second (/record.c)"),
                "{message}"
            );
        }
    }

    #[tokio::test]
    async fn incompatible_class_ternary_types_report_an_error_without_panicking() {
        let error = Compiler::default().compile_string("/record.c", "class first {} class second {} mixed choose(int c, class first a, class second b) { return c ? a : b; }").await.unwrap_err();
        assert!(
            error.to_string().contains(
                "cannot combine `class first (/record.c)` and `class second (/record.c)`"
            )
        );
    }

    #[tokio::test]
    async fn inherited_functions_and_closures_retain_their_defining_class_scope() {
        let root = lib_holding(
            "class-code-scope",
            &[(
                "base.c",
                "class record { int value; } function make() { return (: 0 :); }",
            )],
        );
        let compiled = Compiler::new(temp_lib_config(&root))
            .compile_string("/child.c", "inherit \"/base\"; class extra {} void f() {}")
            .await
            .unwrap();
        let base = compiled.program.lookup_function("make").unwrap();
        let closure = compiled
            .program
            .functions
            .values()
            .find(|f| f.is_closure())
            .unwrap();
        assert_eq!(base.class_scope.definitions.len(), 1);
        assert_eq!(base.class_scope, closure.class_scope);
        assert_eq!(
            compiled
                .program
                .lookup_function("f")
                .unwrap()
                .class_scope
                .definitions
                .len(),
            2
        );
    }

    #[tokio::test]
    async fn recompiled_definitions_do_not_replace_metadata_on_shared_old_code() {
        let compiler = Compiler::default();
        let old = compiler
            .compile_string("/record.c", "class record { int x; } int f() { return 1; }")
            .await
            .unwrap();
        let new = compiler
            .compile_string("/record.c", "class record { int y; } int f() { return 1; }")
            .await
            .unwrap();
        let old_code = &old.program.lookup_function("f").unwrap().code;
        let new_code = &new.program.lookup_function("f").unwrap().code;
        assert!(!Arc::ptr_eq(old_code, new_code));
        assert_eq!(
            old_code.class_scope.definitions[0].fields[0].name,
            ustr("x")
        );
        assert_eq!(
            new_code.class_scope.definitions[0].fields[0].name,
            ustr("y")
        );
    }

    #[tokio::test]
    async fn zero_is_accepted_by_class_arguments_returns_casts_and_array_casts() {
        let task = run_prog("class record {} class record pass(class record x) { return x; } int create() { class record x = pass(0); class record *xs = (class record *)({ 0 }); foreach (class record item : xs) { x = item; } return (class record)x == 0; }").await;
        assert_eq!(
            task.context.result().unwrap(),
            crate::interpreter::lpc_ref::LpcRef::from(1)
        );
    }
}
