use lpc_rs::{
    compiler::lexer::{LexWrapper, Token},
    interpreter::efun::EFUN_PROTOTYPES,
};
use lpc_rs_errors::span::HasSpan;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Documentation, Hover, HoverContents,
    MarkupContent, MarkupKind, Position, TextEdit,
};

use crate::documents::{byte_offset, range};

include!(concat!(env!("OUT_DIR"), "/efun_docs.rs"));

fn identifier(text: &str, byte: usize) -> Option<std::ops::Range<usize>> {
    let mut previous = None;
    let mut before_previous = None;
    for token in LexWrapper::new(text, 0).flatten() {
        let span = token.span();
        if span.l() > byte {
            break;
        }
        if matches!(token, Token::Id(_)) && byte <= span.r() {
            if matches!(previous, Some(Token::CallOther(_)))
                || (matches!(previous, Some(Token::ColonColon(_)))
                    && !matches!(before_previous, Some(Token::Efun(_))))
            {
                return None;
            }
            return Some(span.l()..span.r());
        }
        before_previous = previous;
        previous = Some(token);
    }
    None
}

fn help(name: &str) -> MarkupContent {
    let docs = documentation(name).unwrap_or("Documentation is not available for this efun.");
    MarkupContent {
        kind: MarkupKind::Markdown,
        value: format!("Built-in efun reference\n\n{docs}"),
    }
}

pub fn complete(text: &str, at: Position) -> Vec<CompletionItem> {
    let Some(byte) = byte_offset(text, at) else {
        return vec![];
    };
    let Some(word) = identifier(text, byte) else {
        return vec![];
    };
    let prefix = &text[word.start..byte];
    EFUN_PROTOTYPES
        .iter()
        .filter(|(name, _)| name.starts_with(prefix))
        .map(|(&name, prototype)| CompletionItem {
            label: name.to_owned(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(prototype.to_string()),
            documentation: Some(Documentation::MarkupContent(help(name))),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range: range(text, word.clone()),
                new_text: name.to_owned(),
            })),
            ..CompletionItem::default()
        })
        .collect()
}

pub fn hover(text: &str, at: Position) -> Option<Hover> {
    let word = identifier(text, byte_offset(text, at)?)?;
    let name = &text[word.clone()];
    let prototype = EFUN_PROTOTYPES.get(name)?;
    let mut content = help(name);
    content.value = format!("```lpc\n{prototype}\n```\n\n{}", content.value);
    Some(Hover {
        contents: HoverContents::Markup(content),
        range: Some(range(text, word)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn completion_works_in_an_unfinished_call_and_replaces_the_word() {
        let text = "void f() { clo";
        let items = complete(text, Position::new(0, text.len() as u32));
        let item = items
            .iter()
            .find(|item| item.label == "clone_object")
            .unwrap();
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("missing edit")
        };
        assert_eq!(edit.range, range(text, 11..14));
        assert_eq!(edit.new_text, "clone_object");
    }

    #[test]
    fn hover_contains_bundled_docs_and_respects_lexical_context() {
        let hover = hover("clone_object", Position::new(0, 3)).unwrap();
        let HoverContents::Markup(content) = hover.contents else {
            panic!("missing markup")
        };
        assert!(content.value.contains("blueprint"));
        assert!(content.value.contains("Built-in efun reference"));
        for text in [
            "// clone_object",
            "\"clone_object\"",
            "/* clone_object */",
            "obj->clone_object",
            "parent::clone_object",
        ] {
            let offset = text.find("clone_object").unwrap() + 3;
            assert!(
                super::hover(text, Position::new(0, offset as u32)).is_none(),
                "{text}"
            );
        }
        assert!(super::hover("efun::clone_object", Position::new(0, 9)).is_some());
    }
}
