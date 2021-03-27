use crate::errors::lazy_files::{add_file_to_cache, FILE_CACHE};
use std::{collections::HashMap, fs, path::Path};

use lazy_static::lazy_static;
use regex::Regex;

use crate::{
    context::Context, convert_escapes, errors::preprocessor_error::PreprocessorError,
    parser::span::Span,
};
use codespan_reporting::files::Files;
use path_absolutize::Absolutize;
use std::{ffi::OsString, path::PathBuf, result};

use crate::parser::lexer::{logos_token::StringToken, LexWrapper, Spanned, Token};
use std::ops::Range;

type Result<T> = result::Result<T, PreprocessorError>;

lazy_static! {
    static ref SYS_INCLUDE: Regex = Regex::new(r"\A\s*#\s*include\s+<([^>]+)>\s*\z").unwrap();
    static ref LOCAL_INCLUDE: Regex =
        Regex::new("\\A\\s*#\\s*include\\s+\"([^\"]+)\"[^\\S\n]*\n?\\z").unwrap();
    static ref DEFINE: Regex =
        Regex::new("\\A\\s*#\\s*define\\s+(\\S+)(?:\\s*((?:\\\\.|[^\n])*))?\n?\\z").unwrap();
    static ref UNDEF: Regex = Regex::new(r#"\A\s*#\s*undef\s+(\S+)\s*\z"#).unwrap();
    static ref IFDEF: Regex = Regex::new(r#"\A\s*#\s*ifdef\s+(\S+)\s*\z"#).unwrap();
    static ref IFDEFINED: Regex =
        Regex::new(r#"\A\s*#\s*if\s+defined\s+\(\s*(\S+?)\s*\)\s*\z"#).unwrap();
    static ref IFNDEF: Regex = Regex::new(r#"\A\s*#\s*ifndef\s+(\S+)\s*\z"#).unwrap();
    static ref IFNOTDEFINED: Regex =
        Regex::new(r#"\A\s*#\s*if\s+not\s+defined\s+\(\s*(\S+?)\s*\)\s*\z"#).unwrap();
    static ref ENDIF: Regex = Regex::new(r#"\A\s*#\s*endif\s*\z"#).unwrap();
    static ref ELSE: Regex = Regex::new(r#"\A\s*#\s*else\s*\z"#).unwrap();
}

#[derive(Debug)]
struct IfDef {
    pub code: String,
    pub span: Span,

    /// This field on the top IfDef in the stack indicates if we're currently in a
    /// section that's conditionally compiled out. We mutate this field when we see #else.
    pub skipping_lines: bool,

    /// Is this `#ifdef` itself conditionally compiled out?
    pub compiled_out: bool,
}

#[derive(Debug)]
pub struct Preprocessor {
    /// The compilation context
    context: Context,

    /// We keep track of `#define`d things here.
    defines: HashMap<String, String>,

    /// Stack of ifdefs that are in play, so we can handle `#else` and `#endif`s
    ifdefs: Vec<IfDef>,

    /// Have we seen an `#else` clause for the current `#if`?
    current_else: Option<Span>,

    /// We Track the last slice, because things like preprocessor directives need to check it.
    last_slice: String,
}

impl Preprocessor {
    /// Create a new `Preprocessor`
    ///
    /// # Arguments
    /// `context` - A context object to store data, errors, etc., generated during the compile
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::preprocessor::Preprocessor;
    /// use lpc_rs::context::Context;
    ///
    /// let context = Context::new("test.c", "/home/mud/lib", vec!["/include", "/sys"]);
    /// let preprocessor = Preprocessor::new(context);
    /// ```
    pub fn new(context: Context) -> Self {
        Self {
            context,
            ..Self::default()
        }
    }

    /// Consume this preprocessor, and return its `Context`.
    ///
    /// This is intended for use after preprocessing has completed, and
    /// you're ready to re-take ownership of the context for the next step.
    pub fn into_context(self) -> Context {
        self.context
    }

    /// Convert an in-game path, relative or absolute, to a canonical, absolute on-server path.
    /// This function is used for resolving included files.
    ///
    /// # Arguments
    /// `path` - An in-game path.
    /// `cwd` - The current working directory, needed to resolve relative paths.
    fn canonicalize_path<T, U>(&self, path: T, cwd: U) -> PathBuf
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let path_ref = path.as_ref().as_os_str();
        let sep = String::from(std::path::MAIN_SEPARATOR);
        let os_sep = OsString::from(&sep);
        let mut root_string = OsString::from(self.context.root_dir.to_str().unwrap());
        // Do this the hard way because .join/.push overwrite if the arg starts with "/"
        let localized_path = if path_ref.to_string_lossy().starts_with(&sep) {
            root_string.push(&os_sep);
            root_string.push(&path_ref);
            root_string
        } else {
            root_string.push(&os_sep);
            root_string.push(cwd.as_ref().as_os_str());
            root_string.push(&os_sep);
            root_string.push(&path_ref);
            root_string
        };

        Path::new(
            &localized_path
                .to_string_lossy()
                .replace("//", "/")
                .replace("/./", "/"),
        )
        .absolutize()
        .unwrap()
        .to_path_buf()
    }

    /// Convert an in-game path, relative or absolute, to a canonical, absolute in-game path.
    ///
    /// # Arguments
    /// `path` - An in-game path.
    /// `cwd` - The current working directory, needed to resolve relative paths.
    fn canonicalize_local_path<T, U>(&self, path: T, cwd: U) -> PathBuf
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let canon = self.canonicalize_path(path, cwd);
        let buf = canon.as_os_str();
        let root_len = self.context.root_dir.as_os_str().len();

        PathBuf::from(
            &buf.to_string_lossy()
                .chars()
                .skip(root_len)
                .collect::<String>()
                .replace("//", "/")
                .replace("/./", "/"),
        )
    }

    /// Scan a file on disk, using the `filename` stored in `Context`.
    /// This is a light wrapper around `scan()` for convenience to kick off a scan.
    ///
    /// # Arguments
    /// `cwd` - The current working directory on-server, used to resolve relative links
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::context::Context;
    /// use lpc_rs::preprocessor::Preprocessor;
    ///
    /// let context = Context::new("./foo.c", ".", vec!["/include", "/sys/include"]);
    /// let mut preprocessor = Preprocessor::new(context);
    ///
    /// let processed = preprocessor.scan_context("/");
    /// ```
    pub fn scan_context<T>(&mut self, cwd: T) -> Result<Vec<Spanned<Token>>>
    where
        T: AsRef<Path>,
    {
        let file_id = add_file_to_cache(self.context.filename.clone());
        let files = FILE_CACHE.read();
        let source = match files.source(file_id) {
            Ok(x) => x,
            Err(e) => {
                let canonical_path = self.canonicalize_path(&self.context.filename, &cwd);

                return Err(PreprocessorError {
                    message: format!("Unable to read `{}`: {}", canonical_path.display(), e),
                    span: None,
                    labels: Vec::new(),
                });
            }
        };

        self.scan(&self.context.filename.clone(), cwd, source)
    }

    /// Scan a file's contents, transforming as necessary according to the preprocessing rules.
    ///
    /// # Arguments
    /// `path` - The path + name of the file being scanned.
    /// `cwd` - The current working directory on-server, used to resolve relative links
    /// `file_content` - The actual content of the file to scan.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::preprocessor::Preprocessor;
    /// use lpc_rs::context::Context;
    ///
    /// let context = Context::new("./foo.c", ".", vec!["/include", "/sys/include"]);
    /// let mut preprocessor = Preprocessor::new(context);
    ///
    /// let content = r#"
    ///     #include "include/simple.h"
    ///
    ///     void main() {
    ///         int a = 123;
    ///     }
    /// "#;
    ///
    /// let processed = preprocessor.scan("file.c", "/", content);
    /// ```
    pub fn scan<T, U, V>(&mut self, path: T, cwd: U, file_content: V) -> Result<Vec<Spanned<Token>>>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
        V: AsRef<str>,
    {
        let mut output = Vec::new();

        // Register the file-to-be-scanned with the global file cache
        let filename = path.as_ref().file_name().unwrap();
        let file_id = add_file_to_cache(self.canonicalize_path(filename, &cwd).display());

        let mut token_stream = LexWrapper::new(file_content.as_ref());
        token_stream.set_file_id(file_id);

        for spanned_result in token_stream {
            match spanned_result {
                Ok(spanned) => {
                    let (_l, token, _r) = &spanned;

                    let token_string = token.to_string();

                    // println!("token: {:?}", token);

                    match token {
                        Token::LocalInclude(t) => {
                            self.handle_local_include(t, &cwd, &mut output)?
                        }
                        Token::SysInclude(t) => {
                            self.check_for_previous_newline(t.0)?;
                        }
                        Token::PreprocessorElse(t) => self.handle_else(t)?,
                        Token::Endif(t) => self.handle_endif(t)?,
                        Token::Define(t) => self.handle_define(t)?,
                        Token::Undef(t) => self.handle_undef(t)?,
                        Token::IfDef(t) => self.handle_ifdef(t)?,
                        Token::IfNDef(t) => self.handle_ifndef(t)?,

                        Token::NewLine(_) => { /* Ignore */ }

                        _ => self.append_spanned(&mut output, spanned),
                    }

                    self.last_slice = token_string;
                }
                Err(e) => {
                    return Err(PreprocessorError::from(e));
                }
            }
        }

        if !self.ifdefs.is_empty() {
            let ifdef = self.ifdefs.last().unwrap();

            return Err(PreprocessorError::new(
                "Found `#if` without a corresponding `#endif`",
                ifdef.span,
            ));
        }

        Ok(output)
    }

    fn handle_define(&mut self, token: &StringToken) -> Result<()> {
        if self.skipping_lines() {
            return Ok(());
        }

        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = DEFINE.captures(&token.1) {
            if !self.skipping_lines() && self.defines.contains_key(&captures[1]) {
                return Err(PreprocessorError::new(
                    &format!("Duplicate `#define`: `{}`", &captures[1]),
                    token.0,
                ));
            }

            let name = String::from(&captures[1]);
            let value = if captures[2].is_empty() {
                "0"
            } else {
                &captures[2]
            };

            self.defines.insert(name, convert_escapes(value));
            Ok(())
        } else {
            Err(PreprocessorError::new("Invalid `#define`.", token.0))
        }
    }

    fn handle_undef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = UNDEF.captures(&token.1) {
            self.defines.remove(&captures[1]);
        }

        Ok(())
    }

    fn handle_local_include<U>(
        &mut self,
        token: &StringToken,
        cwd: &U,
        mut output: &mut Vec<Spanned<Token>>,
    ) -> Result<()>
    where
        U: AsRef<Path>,
    {
        if self.skipping_lines() {
            return Ok(());
        }

        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = LOCAL_INCLUDE.captures(&token.1) {
            let matched = captures.get(1).unwrap();
            let included = self.include_local_file(matched.as_str(), &cwd, token.0)?;

            for spanned in included {
                self.append_spanned(&mut output, spanned)
            }

            Ok(())
        } else {
            Err(PreprocessorError::new("Invalid `#include`.", token.0))
        }
    }

    fn handle_ifdef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = IFDEF.captures(&token.1) {
            self.ifdefs.push(IfDef {
                code: String::from(&captures[1]),
                skipping_lines: !self.defines.contains_key(&captures[1]),
                compiled_out: self.skipping_lines(),
                span: token.0,
            });

            Ok(())
        } else {
            Err(PreprocessorError::new("Invalid `#ifdef`.", token.0))
        }
    }

    fn handle_ifndef(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if let Some(captures) = IFNDEF.captures(&token.1) {
            self.ifdefs.push(IfDef {
                code: String::from(&captures[1]),
                skipping_lines: self.defines.contains_key(&captures[1]),
                compiled_out: self.skipping_lines(),
                span: token.0,
            });

            Ok(())
        } else {
            Err(PreprocessorError::new("Invalid `#ifndef`.", token.0))
        }
    }

    fn handle_else(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if ELSE.is_match(&token.1) {
            if self.ifdefs.is_empty() {
                return Err(PreprocessorError::new(
                    "Found `#else` without a corresponding `#if` or `#ifdef`",
                    token.0,
                ));
            }

            if let Some(else_span) = &self.current_else {
                let mut err = PreprocessorError::new("Duplicate `#else` found", token.0);

                err.add_label(
                    "First used here",
                    else_span.file_id,
                    Range::from(else_span),
                );

                return Err(err);
            }

            self.current_else = Some(token.0);

            if !self.current_if_is_compiled_out() {
                let last = self.ifdefs.last_mut().unwrap();
                last.skipping_lines = !last.skipping_lines;
            }

            Ok(())
        } else {
            Err(PreprocessorError::new("Invalid `#else`.", token.0))
        }
    }

    fn handle_endif(&mut self, token: &StringToken) -> Result<()> {
        self.check_for_previous_newline(token.0)?;

        if self.ifdefs.is_empty() {
            return Err(PreprocessorError::new(
                "Found `#endif` without a corresponding `#if`",
                token.0,
            ));
        }

        self.ifdefs.pop();
        self.current_else = None;

        Ok(())
    }

    /// Read in a local file, and scan it through this preprocessor.
    ///
    /// # Arguments
    /// `path` - The path of the file we're going to scan. This is intended to be the file from
    ///     the `#include` directive.
    /// `cwd` - The current working directory. Used to resolving relative pathnames.
    /// `parent_line` - The line number of the file where the include is happening.
    /// `file_id` - The ID of the file from the preprocessor's `files` struct.
    fn include_local_file<T, U>(
        &mut self,
        path: T,
        cwd: U,
        span: Span,
    ) -> Result<Vec<Spanned<Token>>>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let canon_include_path = self.canonicalize_path(&path, &cwd);

        if !canon_include_path.starts_with(&self.context.root_dir) {
            return Err(PreprocessorError::new(
                &format!(
                    "Attempt to include a file outside the root: `{}` (expanded to `{}`)",
                    path.as_ref().display(),
                    canon_include_path.display()
                ),
                span,
            ));
        }

        let file_content = match fs::read_to_string(&canon_include_path) {
            Ok(content) => content,
            Err(e) => {
                return Err(PreprocessorError::new(
                    &format!(
                        "Unable to read include file `{}`: {:?}",
                        path.as_ref().display(),
                        e
                    ),
                    span,
                ));
            }
        };

        let local_canon_include_path = self.canonicalize_local_path(&path, &cwd);
        let filename = local_canon_include_path.file_name().unwrap();
        let cwd = local_canon_include_path.parent().unwrap();
        self.scan(filename, cwd, &file_content)
    }

    /// Are we skipping lines right now due to `#if`s?
    #[inline]
    fn skipping_lines(&self) -> bool {
        match self.ifdefs.last() {
            Some(ifdef) => ifdef.skipping_lines || ifdef.compiled_out,
            None => false,
        }
    }

    /// Is the entire current `#ifdef` compiled out?
    #[inline]
    fn current_if_is_compiled_out(&self) -> bool {
        match self.ifdefs.last() {
            Some(ifdef) => ifdef.compiled_out,
            None => false,
        }
    }

    /// skip-aware way to append to the output
    #[inline]
    fn append_spanned(&self, output: &mut Vec<Spanned<Token>>, to_append: Spanned<Token>) {
        if !self.skipping_lines() {
            output.push(to_append);
        }
    }

    /// A convenience function for checking if preprocessor directives follow a newline.
    fn check_for_previous_newline(&self, span: Span) -> Result<()> {
        if !self.last_slice.ends_with('\n') {
            return Err(PreprocessorError {
                message: "Preprocessor directives must appear on their own line.".to_string(),
                span: Some(span),
                labels: vec![],
            });
        }

        Ok(())
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            context: Context::default(),
            defines: HashMap::new(),
            ifdefs: Vec::new(),
            current_else: None,
            last_slice: String::from("\n"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn fixture() -> Preprocessor {
        let context = Context::new("test.c", "./tests/fixtures", Vec::new());
        Preprocessor::new(context)
    }

    fn test_valid(input: &str, expected: &[&str]) {
        let mut preprocessor = fixture();
        match preprocessor.scan("test.c", "/", input) {
            Ok(result) => {
                let mapped = result.iter().map(|i| i.1.to_string()).collect::<Vec<_>>();

                assert_eq!(mapped, expected)
            }
            Err(e) => {
                panic!("{:?}", e)
            }
        }
    }

    // `expected` is converted to a Regex, for easier matching on errors.
    fn test_invalid(input: &str, expected: &str) {
        let mut preprocessor = fixture();
        match preprocessor.scan("test.c", "/", input) {
            Ok(result) => {
                panic!("Expected to fail, but passed with {:?}", result);
            }
            Err(e) => {
                let regex = Regex::new(expected).unwrap();
                assert!(regex.is_match(&e.to_string()), "error = {:?}", e);
            }
        }
    }

    mod test_local_includes {
        use super::*;

        #[test]
        fn test_includes_the_file() {
            let input = r#"#include "include/simple.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected);
        }

        #[test]
        fn test_includes_multiple_levels() {
            let input = r#"#include "include/level_2/two_level.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected);
        }

        #[test]
        fn test_includes_multiple_files() {
            let input = indoc! {r#"
                #include "include/level_2/two_level.h"
                int j = 123;
                #include "include/simple.h"
            "#};

            let expected = vec![
                "1", "+", "2", "+", "3", "+", "4", "+", "5", ";", "int", "j", "=", "123", ";", "1",
                "+", "2", "+", "3", "+", "4", "+", "5", ";",
            ];

            test_valid(input, &expected);
        }

        #[test]
        fn test_includes_absolute_paths() {
            let input = r#"#include "/include/simple.h""#;

            let expected = vec!["1", "+", "2", "+", "3", "+", "4", "+", "5", ";"];

            test_valid(input, &expected);
        }

        #[test]
        fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #include "./simple.h"
                #endif
            "# };

            test_valid(input, &vec![]);
        }

        #[test]
        fn test_errors_for_nonexistent_paths() {
            let input = r#"#include "/askdf/foo.h""#;

            test_invalid(input, "No such file or directory");
        }

        #[test]
        fn test_errors_for_traversal_attacks() {
            let input = r#"#include "/../../some_file.h""#;

            test_invalid(input, "Attempt to include a file outside the root");
        }

        #[test]
        fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #include "foo.h"
            "#
            };

            test_invalid(
                prog,
                "Preprocessor directives must appear on their own line",
            );
        }

        #[test]
        fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #include "./include/simple.h" klasjd
            "#
            };

            test_invalid(prog, "Invalid `#include`");
        }
    }

    mod test_defines {
        use super::*;

        #[test]
        fn test_object_define() {
            let input = indoc! { r#"
                #define ASS 1 2\n\n3 5 t ass
                #define MAR
                #define DOOD 666 + MAR
                #define SNUH 0x123
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", "/", input) {
                Ok(_) => {
                    assert_eq!(preprocessor.defines.get("ASS").unwrap(), "1 2\n\n3 5 t ass");
                    assert_eq!(preprocessor.defines.get("MAR").unwrap(), "0");
                    assert_eq!(preprocessor.defines.get("DOOD").unwrap(), "666 + MAR");
                    assert_eq!(preprocessor.defines.get("SNUH").unwrap(), "0x123");
                }
                Err(e) => {
                    panic!("{:?}", e)
                }
            }
        }

        #[test]
        fn test_duplicate_define() {
            let input = indoc! { r#"
                #define ASS 123
                #define ASS 456
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", "/", input) {
                Ok(_) => {
                    panic!("Expected an error due to duplicate definition.");
                }
                Err(e) => {
                    assert_eq!(e.message, "Duplicate `#define`: `ASS`");
                }
            }
        }

        #[test]
        fn test_duplicate_after_undef() {
            let input = indoc! { r#"
                #define ASS 123
                #undef ASS
                #define ASS 456
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", "/", input) {
                Ok(_) => {
                    assert_eq!(preprocessor.defines.get("ASS").unwrap(), "456");
                }
                Err(e) => {
                    panic!("{:?}", e)
                }
            }
        }

        #[test]
        fn test_duplicate_ifdefed_out() {
            let input = indoc! { r#"
                #define HELLO 123
                #ifdef FOO
                #define HELLO 456
                #endif
            "# };
            let mut preprocessor = fixture();

            match preprocessor.scan("test.c", "/", input) {
                Ok(_) => {
                    assert_eq!(preprocessor.defines.get("HELLO").unwrap(), "123");
                }
                Err(e) => {
                    panic!("{:?}", e)
                }
            }
        }

        #[test]
        fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #define LOL WUT
            "#
            };

            test_invalid(
                prog,
                "Preprocessor directives must appear on their own line",
            );
        }

        #[test]
        fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #define
            "#
            };

            test_invalid(prog, "Invalid `#define`");
        }
    }

    mod test_ifdef {
        use super::*;

        #[test]
        fn test_with_defined() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                I should be rendered
                #endif
                #ifdef BAR
                I should not be rendered
                #endif
                #undef FOO
                #ifdef FOO
                I also should not be rendered
                #endif
            "# };

            let expected = vec!["I", "should", "be", "rendered"];

            test_valid(prog, &expected);
        }

        #[test]
        fn test_ifdefed_out() {
            let input = indoc! { r#"
                #define BAR
                #ifdef FOO
                #ifdef BAR
                i should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &vec![]);
        }

        #[test]
        fn test_error_without_if() {
            let prog = indoc! { r#"
                #define FOO
                "this will error because of the #endif without an #if or #ifdef";
                #endif
            "# };

            test_invalid(prog, "Found `#endif` without a corresponding `#if`");
        }

        #[test]
        fn test_error_without_endif() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding `#endif`");
        }

        #[test]
        fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #ifdef WUT
            "#
            };

            test_invalid(
                prog,
                "Preprocessor directives must appear on their own line",
            );
        }

        #[test]
        fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifdef
                123;
                #endif
            "#
            };

            test_invalid(prog, "Invalid `#ifdef`");
        }
    }

    mod test_ifndef {
        use super::*;

        #[test]
        fn test_with_not_defined() {
            let prog = indoc! { r#"
                #define BAR
                #ifndef FOO
                I should be rendered
                #endif
                #ifndef BAR
                I should not be rendered
                #endif
                #define FOO
                #ifndef FOO
                I also should not be rendered
                #endif
            "# };

            let expected = vec!["I", "should", "be", "rendered"];

            test_valid(prog, &expected);
        }

        #[test]
        fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #ifndef BAR
                i should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &vec![]);
        }

        #[test]
        fn test_error_without_endif() {
            let prog = indoc! { r#"
                #ifndef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding `#endif`");
        }

        #[test]
        fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #ifndef HELLO
                1 + 3;
            "#
            };

            test_invalid(
                prog,
                "Preprocessor directives must appear on their own line",
            );
        }

        #[test]
        fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifndef
                123;
                #endif
            "#
            };

            test_invalid(prog, "Invalid `#ifndef`");
        }
    }

    mod test_else {
        use super::*;

        #[test]
        fn test_else() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                I should be rendered 1
                #else
                I should not be rendered 1
                #endif
                #ifndef FOO
                I should not be rendered 2
                #else
                I should be rendered 2
                #endif
                #undef FOO
                #ifndef FOO
                I should be rendered 3
                #else
                I should not be rendered 3
                #endif
            "# };

            let expected = vec![
                "I", "should", "be", "rendered", "1", "I", "should", "be", "rendered", "2", "I",
                "should", "be", "rendered", "3",
            ];

            test_valid(prog, &expected);
        }

        #[test]
        fn test_ifdefed_out() {
            let input = indoc! { r#"
                #ifdef FOO
                #ifndef BAR
                i should not be rendered
                #else
                i also should not be rendered
                #endif
                #endif
            "# };

            test_valid(input, &vec![]);
        }

        #[test]
        fn test_error_on_duplicate_else() {
            let prog = indoc! { r#"
                #ifndef FOO
                #else
                "this will error because of the duplicate #else";
                #else
                #endif
            "# };

            test_invalid(prog, "Duplicate `#else`");
        }

        #[test]
        fn test_error_if_not_first_on_line() {
            let prog = indoc! { r#"
                a + 3 + as; #else
            "#
            };

            test_invalid(
                prog,
                "Preprocessor directives must appear on their own line",
            );
        }

        #[test]
        fn test_error_if_invalid() {
            let prog = indoc! { r#"
                #ifdef ASD
                #else 1 + 4
                #endif
            "#
            };

            test_invalid(prog, "Invalid `#else`");
        }
    }
}
