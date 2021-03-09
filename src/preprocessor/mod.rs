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

use std::ops::Range;

type Result<T> = result::Result<T, PreprocessorError>;

#[derive(Debug, Clone)]
pub enum PreprocessorDirective {
    LocalInclude(String, usize),
    SysInclude(String, usize),
    Define(String, usize),
    Undef(String, usize),
    // If
    // Else
    // Endif
}

#[derive(Debug)]
pub struct Preprocessor {
    /// The compilation context
    context: Context,

    defines: HashMap<String, String>,

    directives: Vec<PreprocessorDirective>,

    /// Are we currently within a block that is `if`'d out?
    /// We also track this as a stack.
    skip_lines: Vec<bool>,

    /// Stack of ifdefs that are in play, so we can handle #endifs
    /// This stores the def, along with the file_id and line of the directive.
    ifdefs: Vec<(String, usize, usize)>,

    /// Have we seen an `#else` clause for the current `#if`?
    /// Track the file_id and line number of it.
    current_else: Option<(usize, usize)>,
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

    // store file_id on token
    // tokenize original file
    // iterate tokens by lines
    //   foreach line
    //     if a preprocessor directive
    //       if #include
    //          scan file & emit to token stream
    //     else
    //       emit to token stream
    //

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

        Path::new(&localized_path.to_string_lossy().replace("//", "/"))
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
                .replace("//", "/"),
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
    pub fn scan_context<T>(&mut self, cwd: T) -> Result<String>
    where
        T: AsRef<Path>,
    {
        let file_id = self.context.files.add(self.context.filename.clone());
        let source = match self.context.files.source(file_id) {
            Ok(x) => x,
            Err(e) => {
                let canonical_path = self.canonicalize_path(&self.context.filename, &cwd);

                return Err(PreprocessorError {
                    message: format!("Unable to read `{}`: {}", canonical_path.display(), e),
                    file_id,
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
    pub fn scan<T, U, V>(&mut self, path: T, cwd: U, file_content: V) -> Result<String>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
        V: AsRef<str>,
    {
        lazy_static! {
            static ref SYS_INCLUDE: Regex =
                Regex::new(r"\A\s*#\s*include\s+<([^>]+)>\s*\z").unwrap();
            static ref LOCAL_INCLUDE: Regex =
                Regex::new(r#"\A\s*#\s*include\s+"([^"]+)"\s*\z"#).unwrap();
            static ref DEFINE: Regex =
                Regex::new(r#"\A\s*#\s*define\s+(\S+)(?:\s*(.*))?\z"#).unwrap();
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

        let file_id = self
            .context
            .files
            .add(String::from(path.as_ref().to_string_lossy()));

        let mut current_line = 1;

        let mut output = String::new();

        let filename = path.as_ref().file_name().unwrap();
        let canonical_path = self.canonicalize_local_path(filename, &cwd);

        let format_line =
            |line_num| format!("#line {} \"{}\"\n", line_num, canonical_path.display());

        self.append_str(&mut output, &format_line(current_line));

        for line in file_content.as_ref().lines() {
            if ENDIF.is_match(line) {
                if self.ifdefs.is_empty() || self.skip_lines.is_empty() {
                    return Err(PreprocessorError::new(
                        "Found `#endif` without a corresponding #if",
                        file_id,
                        self.context.files.file_line_span(file_id, current_line),
                    ));
                }

                self.ifdefs.pop();
                self.skip_lines.pop();
                self.current_else = None;
                current_line += 1;
                continue;
            } else if ELSE.is_match(line) {
                if self.ifdefs.is_empty() || self.skip_lines.is_empty() {
                    return Err(PreprocessorError::new(
                        "Found `#else` without a corresponding #if",
                        file_id,
                        self.context.files.file_line_span(file_id, current_line),
                    ));
                }

                if let Some((else_file_id, else_line)) = &self.current_else {
                    let mut err = PreprocessorError::new(
                        "Duplicate #else found",
                        file_id,
                        self.context.files.file_line_span(file_id, current_line),
                    );

                    err.add_label(
                        "Originally defined here",
                        *else_file_id,
                        Range::from(self.context.files.file_line_span(*else_file_id, *else_line)),
                    );

                    return Err(err);
                }

                self.current_else = Some((file_id, current_line));
                let last = self.skip_lines.last_mut().unwrap();
                *last = !*last;
                current_line += 1;
                continue;
            }

            if self.skipping_lines() {
                continue;
            }

            if let Some(captures) = SYS_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();

                self.directives.push(PreprocessorDirective::SysInclude(
                    String::from(matched.as_str()),
                    current_line,
                ));
            } else if let Some(captures) = LOCAL_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();
                let included =
                    self.include_local_file(matched.as_str(), &cwd, current_line, file_id)?;

                self.append_str(&mut output, &included);
                if !output.ends_with('\n') {
                    self.append_char(&mut output, '\n');
                }
                self.append_str(&mut output, &format_line(current_line + 1));
            } else if let Some(captures) = DEFINE.captures(line) {
                if self.defines.contains_key(&captures[1]) {
                    return Err(PreprocessorError::new(
                        &format!("Duplicate #define: `{}`", &captures[1]),
                        file_id,
                        self.context.files.file_line_span(file_id, current_line),
                    ));
                }

                let name = String::from(&captures[1]);
                let value = if captures[2].is_empty() {
                    "0"
                } else {
                    &captures[2]
                };

                self.defines.insert(name, convert_escapes(value));
            } else if let Some(captures) = UNDEF.captures(line) {
                self.defines.remove(&captures[1]);
            } else if let Some(captures) = IFDEF.captures(line) {
                self.ifdefs
                    .push((String::from(&captures[1]), file_id, current_line));
                self.skip_lines
                    .push(!self.defines.contains_key(&captures[1]));
            } else if let Some(captures) = IFNDEF.captures(line) {
                self.ifdefs
                    .push((String::from(&captures[1]), file_id, current_line));
                self.skip_lines
                    .push(self.defines.contains_key(&captures[1]));
            } else {
                self.append_str(&mut output, line);
                self.append_char(&mut output, '\n');
            }
            current_line += 1;
        }

        if !self.ifdefs.is_empty() {
            let ifdef = self.ifdefs.last().unwrap();
            let (file_id, line_num) = (ifdef.1, ifdef.2);
            let span = self.context.files.file_line_span(file_id, line_num);

            let e =
                PreprocessorError::new("Found `#if` without a corresponding #endif", file_id, span);

            return Err(e);
        }

        Ok(output)
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
        parent_line: usize,
        file_id: usize,
    ) -> Result<String>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let canon_include_path = self.canonicalize_path(&path, &cwd);

        if !canon_include_path.starts_with(&self.context.root_dir) {
            let range = if let Ok(r) = self.context.files.line_range(file_id, parent_line - 1) {
                r
            } else {
                0..1
            };

            return Err(PreprocessorError::new(
                &format!(
                    "Attempt to include a file outside the root: `{}` (expanded to `{}`)",
                    path.as_ref().display(),
                    canon_include_path.display()
                ),
                file_id,
                Span {
                    l: range.start,
                    r: range.end - 1,
                },
            ));
        }

        let file_content = match fs::read_to_string(&canon_include_path) {
            Ok(content) => content,
            Err(e) => {
                let range = if let Ok(r) = self.context.files.line_range(file_id, parent_line - 1) {
                    r
                } else {
                    0..1
                };

                return Err(PreprocessorError::new(
                    &format!(
                        "Unable to read include file `{}`: {:?}",
                        path.as_ref().display(),
                        e
                    ),
                    file_id,
                    Span {
                        l: range.start,
                        r: range.end - 1,
                    },
                ));
            }
        };

        let local_canon_include_path = self.canonicalize_local_path(&path, &cwd);
        let filename = local_canon_include_path.file_name().unwrap();
        let cwd = local_canon_include_path.parent().unwrap();
        self.scan(filename, cwd, &file_content)
    }

    /// Are we skipping lines?
    #[inline]
    fn skipping_lines(&self) -> bool {
        !self.skip_lines.is_empty() && *self.skip_lines.last().unwrap()
    }

    /// `skip_lines` aware way to append to the output
    #[inline]
    fn append_str(&self, output: &mut String, to_append: &str) {
        if !self.skipping_lines() {
            output.push_str(to_append);
        }
    }

    /// `skip_lines` aware way to append to the output
    #[inline]
    fn append_char(&self, output: &mut String, to_append: char) {
        if !self.skipping_lines() {
            output.push(to_append);
        }
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            context: Context::default(),
            defines: HashMap::new(),
            directives: Vec::new(),
            skip_lines: Vec::new(),
            ifdefs: Vec::new(),
            current_else: None,
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

    fn test_valid(input: &str, expected: &str) {
        let mut preprocessor = fixture();
        match preprocessor.scan("test.c", "/", input) {
            Ok(result) => {
                assert_eq!(result, expected)
            }
            Err(e) => {
                panic!(format!("{:?}", e))
            }
        }
    }

    // `expected` is converted to a Regex, for easier matching on errors.
    fn test_invalid(input: &str, expected: &str) {
        let mut preprocessor = fixture();
        match preprocessor.scan("test.c", "/", input) {
            Ok(result) => {
                panic!("Expected to fail, but passed with {}", result);
            }
            Err(e) => {
                let regex = Regex::new(expected).unwrap();
                assert!(regex.is_match(&e.to_string()));
            }
        }
    }

    mod test_local_includes {
        use super::*;

        #[test]
        fn test_includes_the_file() {
            let input = r#"#include "include/simple.h""#;

            let expected = indoc! {r#"
                #line 1 "/test.c"
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 2 "/test.c"
            "#};

            test_valid(input, expected);
        }

        #[test]
        fn test_includes_multiple_levels() {
            let input = r#"#include "include/level_2/two_level.h""#;

            let expected = indoc! {r#"
                #line 1 "/test.c"
                #line 1 "/include/level_2/two_level.h"
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 2 "/include/level_2/two_level.h"
                #line 2 "/test.c"
            "#};

            test_valid(input, expected);
        }

        #[test]
        fn test_includes_multiple_files() {
            let input = indoc! {r#"
                #include "include/level_2/two_level.h"
                int j = 123;
                #include "include/simple.h"
            "#};

            let expected = indoc! {r#"
                #line 1 "/test.c"
                #line 1 "/include/level_2/two_level.h"
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 2 "/include/level_2/two_level.h"
                #line 2 "/test.c"
                int j = 123;
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 4 "/test.c"
            "#};

            test_valid(input, expected);
        }

        #[test]
        fn test_includes_absolute_paths() {
            let input = r#"#include "/include/simple.h""#;

            let expected = indoc! {r#"
                #line 1 "/test.c"
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 2 "/test.c"
            "#};

            test_valid(input, expected);
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
                    panic!(format!("{:?}", e))
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
                    assert_eq!(e.message, "Duplicate #define: `ASS`");
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

            let expected = indoc! { r#"
                #line 1 "/test.c"
                I should be rendered
            "# };

            test_valid(prog, expected);
        }

        #[test]
        fn test_error_without_if() {
            let prog = indoc! { r#"
                #define FOO
                "this will error because of the #endif without an #if or #ifdef";
                #endif
            "# };

            test_invalid(prog, "Found `#endif` without a corresponding #if");
        }

        #[test]
        fn test_error_without_endif() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding #endif");
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

            let expected = indoc! { r#"
                #line 1 "/test.c"
                I should be rendered
            "# };

            test_valid(prog, expected);
        }

        #[test]
        fn test_error_without_endif() {
            let prog = indoc! { r#"
                #ifndef FOO
                "this will error because there's no endif";
            "# };

            test_invalid(prog, "Found `#if` without a corresponding #endif");
        }
    }

    mod test_else {
        use super::*;

        #[test]
        fn test_else() {
            let prog = indoc! { r#"
                #define FOO
                #ifdef FOO
                I should be rendered #1
                #else
                I should not be rendered #1
                #endif
                #ifndef FOO
                I should not be rendered #2
                #else
                I should be rendered #2
                #endif
                #undef FOO
                #ifndef FOO
                I should be rendered #3
                #else
                I should not be rendered #3
                #endif
            "# };

            let expected = indoc! { r#"
                #line 1 "/test.c"
                I should be rendered #1
                I should be rendered #2
                I should be rendered #3
            "# };

            test_valid(prog, expected);
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

            test_invalid(prog, "Duplicate #else");
        }
    }
}
