//! Versioned, self-describing class records; metadata never allocates LPC arrays.

use std::{fmt::Write, sync::LazyLock};

use lpc_rs_core::{
    class::{ClassField, ClassId},
    lpc_path::LibRoot,
    lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result};
use regex::Regex;
use ustr::ustr;

use super::{Reader, write_string, write_value};
use crate::interpreter::{lpc_class::LpcClass, lpc_ref::LpcRef, stm::TxnHandle};

static IDENTIFIER: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\A[\p{Alphabetic}_]\w*\z").expect("identifier regex"));

pub(super) fn write_class(
    out: &mut String,
    instance: &LpcClass,
    txn: &TxnHandle,
    depth: usize,
) -> Result<()> {
    out.push_str("c(1,");
    write_identity(out, instance.id());
    out.push_str(",({");
    for (field, value) in instance.stored_fields() {
        out.push_str("({");
        write_string(out, field.name.as_str());
        out.push(',');
        write_type(out, field.type_)?;
        out.push(',');
        write_value(out, value, txn, depth + 1)?;
        out.push_str(",}),");
    }
    out.push_str("}),({");
    for view in instance.stored_views() {
        out.push_str("({");
        for index in view {
            write!(out, "{index},").expect("String never fails");
        }
        out.push_str("}),");
    }
    out.push_str("}))");
    Ok(())
}

fn write_identity(out: &mut String, id: ClassId) {
    write_string(out, id.source.as_str());
    out.push(',');
    write_string(out, id.name.as_str());
}

fn write_type(out: &mut String, type_: LpcType) -> Result<()> {
    let kind = match type_ {
        LpcType::Int(_) => "int",
        LpcType::Float(_) => "float",
        LpcType::String(_) => "string",
        LpcType::Bytes(_) => "bytes",
        LpcType::Object(_) => "object",
        LpcType::Mapping(_) => "mapping",
        LpcType::Function(_) => "function",
        LpcType::Mixed(_) => "mixed",
        LpcType::Class(_, _) => "class",
        LpcType::Void | LpcType::Union(_) => {
            return Err(LpcError::runtime("save file: unsupported class field type"));
        }
    };
    out.push_str("({");
    write_string(out, kind);
    out.push(',');
    out.push(if type_.is_array() { '1' } else { '0' });
    out.push(',');
    if let LpcType::Class(id, _) = type_ {
        write_identity(out, id);
        out.push(',');
    }
    out.push_str("})");
    Ok(())
}

impl Reader<'_> {
    pub(super) fn class(&mut self, depth: usize) -> Result<LpcRef> {
        self.expect("c(")?;
        if self.metadata_number()? != 1 {
            return Err(self.error("unsupported class record version"));
        }
        self.expect(",")?;
        let id = self.class_identity()?;
        self.expect(",({")?;
        let mut fields = Vec::new();
        while !self.text[self.at..].starts_with(b"})") {
            self.expect("({")?;
            let name = self.identifier()?;
            self.expect(",")?;
            let type_ = self.field_type()?;
            self.expect(",")?;
            let value = self.value(depth + 1)?;
            self.expect(",}),")?;
            fields.push((
                ClassField {
                    name: ustr(&name),
                    type_,
                },
                value,
            ));
        }
        self.expect("}),({")?;
        let mut views = Vec::new();
        while !self.text[self.at..].starts_with(b"})") {
            self.expect("({")?;
            let mut slots = Vec::new();
            while !self.text[self.at..].starts_with(b"})") {
                slots.push(self.metadata_number()?);
                self.expect(",")?;
            }
            self.expect("}),")?;
            views.push(slots.into_boxed_slice());
        }
        self.expect("}))")?;
        let instance = LpcClass::from_saved(id, fields, views, self.txn)?;
        Ok(LpcRef::Class(self.txn.with(|t| t.mint_class(instance))))
    }

    fn identifier(&mut self) -> Result<String> {
        let name = self.string()?;
        if !IDENTIFIER.is_match(&name) {
            return Err(self.error("invalid class or field name"));
        }
        Ok(name)
    }

    fn class_identity(&mut self) -> Result<ClassId> {
        let source = self.string()?;
        let canonical = LibRoot::new("/").in_game(&source, "/");
        if source.len() <= 1
            || !source.starts_with('/')
            || source.contains('\0')
            || AsRef::<str>::as_ref(&canonical) != source
        {
            return Err(self.error("class source must be a canonical in-game file path"));
        }
        self.expect(",")?;
        let name = self.identifier()?;
        Ok(ClassId {
            source: ustr(&source),
            name: ustr(&name),
        })
    }

    fn field_type(&mut self) -> Result<LpcType> {
        self.expect("({")?;
        let kind = self.string()?;
        self.expect(",")?;
        let array = match self.metadata_number()? {
            0 => false,
            1 => true,
            _ => return Err(self.error("class field array flag must be 0 or 1")),
        };
        self.expect(",")?;
        let type_ = match kind.as_str() {
            "int" => LpcType::Int(array),
            "float" => LpcType::Float(array),
            "string" => LpcType::String(array),
            "bytes" => LpcType::Bytes(array),
            "object" => LpcType::Object(array),
            "mapping" => LpcType::Mapping(array),
            "function" => LpcType::Function(array),
            "mixed" => LpcType::Mixed(array),
            "class" => {
                let id = self.class_identity()?;
                self.expect(",")?;
                LpcType::Class(id, array)
            }
            _ => return Err(self.error("unknown class field type")),
        };
        self.expect("})")?;
        Ok(type_)
    }

    fn metadata_number(&mut self) -> Result<usize> {
        let start = self.at;
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.at += 1;
        }
        std::str::from_utf8(&self.text[start..self.at])
            .ok()
            .and_then(|text| text.parse().ok())
            .ok_or_else(|| self.error("expected a nonnegative class metadata integer"))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::class::{ClassDefinition, ClassScope};

    use super::*;
    use crate::interpreter::{
        lpc_class::{read_member, write_member},
        lpc_ref::NULL,
        save_format::{MAX_DEPTH, read_value},
        stm::WorldValue,
    };

    const RECORD: &str = r#"c(1,"/record.h","record",({({"n",({"int",0,}),7,}),}),({({0,}),}))"#;

    fn read(text: &str, txn: &TxnHandle) -> LpcRef {
        read_value(text, txn, &|_| None).unwrap()
    }

    fn written(value: &LpcRef, txn: &TxnHandle) -> String {
        let mut out = String::new();
        write_value(&mut out, value, txn, 0).unwrap();
        out
    }

    fn payload(value: &LpcRef, txn: &TxnHandle) -> Arc<LpcClass> {
        let LpcRef::Class(cell) = value else {
            panic!("class instance")
        };
        txn.with(|t| t.read_class(cell.id)).unwrap()
    }

    fn rejects(text: &str, expected: &str) {
        let error = read_value(text, &TxnHandle::empty(), &|_| None).unwrap_err();
        assert!(error.to_string().contains(expected), "{text}: {error}");
    }

    fn scope(fields: &[(&str, LpcType)]) -> ClassScope {
        ClassScope {
            definitions: vec![Arc::new(ClassDefinition {
                id: ClassId {
                    source: ustr("/record.h"),
                    name: ustr("record"),
                },
                fields: fields
                    .iter()
                    .map(|(name, type_)| ClassField {
                        name: ustr(name),
                        type_: *type_,
                    })
                    .collect(),
            })]
            .into_boxed_slice(),
        }
    }

    #[test]
    fn class_records_round_trip_in_a_fresh_world_without_source_files() {
        let txn = TxnHandle::empty();
        let value = read(RECORD, &txn);
        assert_eq!(written(&value, &txn), RECORD);
        let fresh = TxnHandle::empty();
        let restored = read(&written(&value, &txn), &fresh);
        assert_eq!(payload(&value, &txn), payload(&restored, &fresh));
        assert_ne!(value, restored);
    }

    #[test]
    fn metadata_does_not_allocate_lpc_containers() {
        let txn = TxnHandle::empty();
        read(RECORD, &txn);
        let changes: Vec<_> = txn.with(|t| t.take_changeset()).into_changes().collect();
        assert_eq!(changes.len(), 1);
        assert!(matches!(changes[0].1, Some(WorldValue::Class(_))));
    }

    #[test]
    fn empty_classes_keep_their_empty_view() {
        let txn = TxnHandle::empty();
        let text = r#"c(1,"/empty.h","empty",({}),({({}),}))"#;
        assert_eq!(written(&read(text, &txn), &txn), text);
    }

    #[test]
    fn all_declared_types_and_array_flags_round_trip() {
        let txn = TxnHandle::empty();
        let id = ClassId {
            source: ustr("/record.h"),
            name: ustr("record"),
        };
        let types = [
            LpcType::Int(false),
            LpcType::Float(false),
            LpcType::String(false),
            LpcType::Bytes(false),
            LpcType::Object(false),
            LpcType::Mapping(false),
            LpcType::Function(false),
            LpcType::Mixed(false),
            LpcType::Class(id, false),
        ];
        let fields: Vec<_> = types
            .into_iter()
            .flat_map(|type_| [type_, type_.as_array(true)])
            .enumerate()
            .map(|(index, type_)| {
                (
                    ClassField {
                        name: ustr(&format!("field_{index}")),
                        type_,
                    },
                    NULL,
                )
            })
            .collect();
        let slots = (0..fields.len()).collect();
        let instance = LpcClass::from_saved(id, fields, vec![slots], &txn).unwrap();
        let value = LpcRef::Class(txn.with(|t| t.mint_class(instance.clone())));
        assert_eq!(
            *payload(&read(&written(&value, &txn), &txn), &txn),
            instance
        );
    }

    #[test]
    fn multiple_views_preserve_shared_slots_legacy_types_and_future_adaptation() {
        let txn = TxnHandle::empty();
        let text = r#"c(1,"/record.h","record",({({"n",({"int",0,}),7,}),({"changed",({"int",0,}),8,}),({"changed",({"string",0,}),"new",}),}),({({0,1,}),({2,0,}),}))"#;
        let restored = read(text, &txn);
        let old = scope(&[("n", LpcType::Int(false)), ("changed", LpcType::Int(false))]);
        let new = scope(&[
            ("changed", LpcType::String(false)),
            ("n", LpcType::Int(false)),
        ]);
        let future = scope(&[("added", LpcType::Int(false)), ("n", LpcType::Int(false))]);
        assert_eq!(written(&restored, &txn), text);
        write_member(&restored, &txn, &new, ustr("n"), 9.into()).unwrap();
        assert_eq!(
            read_member(&restored, &txn, &old, ustr("n")).unwrap(),
            LpcRef::from(9)
        );
        assert_eq!(
            read_member(&restored, &txn, &old, ustr("changed")).unwrap(),
            LpcRef::from(8)
        );
        assert_eq!(
            read_member(&restored, &txn, &new, ustr("changed")).unwrap(),
            LpcRef::from("new")
        );
        assert_eq!(
            read_member(&restored, &txn, &future, ustr("added")).unwrap(),
            NULL
        );
        assert_eq!(
            read_member(&restored, &txn, &future, ustr("n")).unwrap(),
            LpcRef::from(9)
        );
        let again = read(&written(&restored, &txn), &txn);
        assert_eq!(payload(&again, &txn), payload(&restored, &txn));
    }

    #[test]
    fn nested_class_fields_preserve_nominal_identity_and_arrays_remain_shallow() {
        let txn = TxnHandle::empty();
        for (descriptor, value) in [
            (r#"({"class",0,"/record.h","record",})"#, RECORD.to_owned()),
            (
                r#"({"class",1,"/record.h","record",})"#,
                format!("({{1,{RECORD},}})"),
            ),
        ] {
            let text = format!(
                r#"c(1,"/holder.h","holder",({{({{"child",{descriptor},{value},}}),}}),({{({{0,}}),}}))"#
            );
            assert_eq!(written(&read(&text, &txn), &txn), text);
        }
        let text = format!(
            r#"c(1,"/holder.h","holder",({{({{"child",({{"class",0,"/other.h","record",}}),{RECORD},}}),}}),({{({{0,}}),}}))"#
        );
        rejects(&text, "invalid value for");
    }

    #[test]
    fn unicode_names_and_escaped_source_paths_round_trip() {
        let txn = TxnHandle::empty();
        let text = RECORD
            .replace("/record.h", r#"/récord\"\\.h"#)
            .replace("\"record\"", "\"記録\"")
            .replace("\"n\"", "\"e\u{301}\"");
        assert_eq!(written(&read(&text, &txn), &txn), text);
    }

    #[test]
    fn class_keys_and_repeated_occurrences_restore_independently() {
        let txn = TxnHandle::empty();
        let original = read(RECORD, &txn);
        let array = crate::interpreter::lpc_array::LpcArray::new([original.clone(), original]);
        let value = LpcRef::Array(txn.with(|t| t.mint_array(array)));
        let restored = read(&written(&value, &txn), &txn);
        let items = restored
            .with_array(&txn, |a| a.iter().cloned().collect::<Vec<_>>())
            .unwrap();
        assert_ne!(items[0], items[1]);
        let scope = scope(&[("n", LpcType::Int(false))]);
        write_member(&items[0], &txn, &scope, ustr("n"), 99.into()).unwrap();
        assert_eq!(
            read_member(&items[1], &txn, &scope, ustr("n")).unwrap(),
            LpcRef::from(7)
        );
        let text = format!("([{RECORD}:{RECORD},])");
        let mapping = read(&text, &txn);
        mapping
            .with_mapping(&txn, |m| {
                let (key, value) = m.iter().next().unwrap();
                assert_ne!(key, value);
            })
            .unwrap();
        assert_eq!(written(&mapping, &txn), text);
    }

    #[test]
    fn invalid_versions_and_metadata_integers_are_rejected() {
        for version in ["0", "2", "184467440737095516160", "-1", "1.0", ""] {
            rejects(
                &RECORD.replacen("c(1,", &format!("c({version},"), 1),
                "save file:",
            );
        }
        for index in ["-1", "1.5", "184467440737095516160", "1"] {
            rejects(
                &RECORD.replace("({0,})", &format!("({{{index},}})")),
                "save file:",
            );
        }
    }

    #[test]
    fn noncanonical_sources_and_invalid_names_are_rejected() {
        for source in [
            "",
            "/",
            "record.h",
            "/a/../record.h",
            "/./record.h",
            "/a//record.h",
            "/record.h/",
            "/record\0.h",
        ] {
            rejects(&RECORD.replace("/record.h", source), "canonical in-game");
        }
        for name in ["", "1record", "a b", "a-b", "a/b"] {
            rejects(
                &RECORD.replace("\"record\"", &format!("\"{name}\"")),
                "invalid class or field name",
            );
            rejects(
                &RECORD.replace("\"n\"", &format!("\"{name}\"")),
                "invalid class or field name",
            );
        }
    }

    #[test]
    fn unknown_field_types_and_invalid_array_flags_are_rejected() {
        for kind in ["void", "union", "unknown", ""] {
            rejects(
                &RECORD.replace("\"int\"", &format!("\"{kind}\"")),
                "unknown class field type",
            );
        }
        for flag in ["2", "-1", "0.5", "184467440737095516160"] {
            rejects(
                &RECORD.replace("\"int\",0,", &format!("\"int\",{flag},")),
                "save file:",
            );
        }
        rejects(
            &RECORD.replace("({\"int\",0,})", "({\"class\",0,\"relative\",\"r\",})"),
            "canonical in-game",
        );
    }

    #[test]
    fn malformed_slots_views_and_incompatible_values_are_rejected_before_allocation() {
        for (slots, views, expected) in [
            (
                r#"({"n",({"int",0,}),7,}),({"n",({"int",0,}),8,}),"#,
                "({0,}),",
                "duplicate class slot",
            ),
            (
                r#"({"n",({"int",0,}),7,}),({"n",({"string",0,}),"x",}),"#,
                "({0,1,}),",
                "duplicate field name",
            ),
            (r#"({"n",({"int",0,}),7,}),"#, "", "at least one view"),
            (
                r#"({"n",({"int",0,}),7,}),"#,
                "({}),",
                "not used by any view",
            ),
            (
                r#"({"n",({"int",0,}),7,}),"#,
                "({0,}),({0,}),",
                "duplicate class view",
            ),
            (
                r#"({"n",({"int",0,}),7,}),"#,
                "({0,0,}),",
                "duplicate field name",
            ),
            (
                r#"({"n",({"int",0,}),"wrong",}),"#,
                "({0,}),",
                "invalid value for",
            ),
        ] {
            let text = format!(r#"c(1,"/record.h","record",({{{slots}}}),({{{views}}}))"#);
            let txn = TxnHandle::empty();
            let error = read_value(&text, &txn, &|_| None).unwrap_err();
            assert!(error.to_string().contains(expected), "{text}: {error}");
            assert!(
                txn.with(|t| t.take_changeset())
                    .into_changes()
                    .next()
                    .is_none()
            );
        }
    }

    #[test]
    fn truncated_records_and_extra_components_are_errors() {
        for end in 0..RECORD.len() {
            rejects(&RECORD[..end], "save file:");
        }
        for text in [
            format!("{RECORD}x"),
            RECORD.replace("7,})", "7,8,})"),
            RECORD.replace("\"int\",0,", "\"int\",0,1,"),
            RECORD.replace(",", ", "),
        ] {
            rejects(&text, "save file:");
        }
    }

    #[test]
    fn class_values_obey_the_shared_nesting_limit() {
        let mut text = "0".to_owned();
        let wrap = |value| {
            format!(
                r#"c(1,"/record.h","record",({{({{"n",({{"mixed",0,}}),{value},}}),}}),({{({{0,}}),}}))"#
            )
        };
        for _ in 0..MAX_DEPTH {
            text = wrap(text);
        }
        let txn = TxnHandle::empty();
        assert_eq!(written(&read(&text, &txn), &txn), text);
        rejects(&wrap(text), "nesting deeper than 128 levels");
    }

    #[test]
    fn cyclic_classes_cannot_be_saved() {
        let txn = TxnHandle::empty();
        let value = read(&RECORD.replace("\"int\"", "\"mixed\""), &txn);
        let scope = scope(&[("n", LpcType::Mixed(false))]);
        write_member(&value, &txn, &scope, ustr("n"), value.clone()).unwrap();
        let error = write_value(&mut String::new(), &value, &txn, 0).unwrap_err();
        assert!(error.to_string().contains("nesting deeper than 128 levels"));
    }

    #[test]
    fn class_object_fields_follow_live_missing_and_stale_object_resolution() {
        use crate::interpreter::{process::Process, program::Program};
        let txn = TxnHandle::empty();
        let object = Arc::new(Process::new(Program::new("/thing.c")));
        let text = RECORD
            .replace("\"int\"", "\"object\"")
            .replace("7,})", &format!("${}@/thing$,}})", object.created));
        let live = read_value(&text, &txn, &|_| Some(object.clone())).unwrap();
        let value = payload(&live, &txn)
            .stored_fields()
            .next()
            .unwrap()
            .1
            .clone();
        assert!(
            matches!(value, LpcRef::Object(ref weak) if Arc::ptr_eq(&weak.upgrade().unwrap(), &object))
        );
        let missing = read(&text, &txn);
        assert_eq!(
            payload(&missing, &txn).stored_fields().next().unwrap().1,
            &NULL
        );
        let stale = text.replace(
            &format!("${}@", object.created),
            &format!("${}@", object.created - 1),
        );
        let value = read_value(&stale, &txn, &|_| Some(object.clone())).unwrap();
        assert_eq!(
            payload(&value, &txn).stored_fields().next().unwrap().1,
            &NULL
        );
    }

    #[test]
    fn restored_instances_disappear_when_the_transaction_aborts() {
        use crate::interpreter::stm::{Committer, Transaction};
        let committer = Committer::new();
        let txn = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let value = read(RECORD, &txn);
        let LpcRef::Class(cell) = value else {
            panic!("class record")
        };
        drop(txn);
        let observer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(observer.with(|t| t.read_class(cell.id)).is_none());
    }
}
