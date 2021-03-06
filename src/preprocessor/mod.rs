use std::{collections::HashMap, fs, path::Path};

use lazy_static::lazy_static;
use regex::Regex;

use crate::{context::Context, errors::preprocessor_error::PreprocessorError, parser::span::Span};
use codespan_reporting::files::Files;
use path_absolutize::Absolutize;
use std::{ffi::OsString, path::PathBuf, result};
use std::io::Error;

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
    skip_lines: bool,
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

    // read file into rope
    // for each line
    //   check for preprocessor lines
    //     if so, process as necessary
    //       strip escaped newlines
    //       if include, insert file immediately, continue read with first line of inserted content
    //       if define, store it, along with line number
    //       if undef, remove define
    //     if not, check if we're skipping lines
    //       if so, skip the line
    //       if not tokenize it through a parser, and check each token against defined items. replace as necessary
    //          emit the line
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
        let id = self.context.files.add(self.context.filename.clone());
        let source = match self.context.files.source(id) {
            Ok(x) => x,
            Err(e) => {
                let canonical_path = self.canonicalize_path(&self.context.filename, &cwd);

                return Err(PreprocessorError {
                    message: format!("Unable to read `{}`: {}", canonical_path.display(), e),
                    file_id: id,
                    span: None
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
        }

        let file_id = self
            .context
            .files
            .add(String::from(path.as_ref().to_string_lossy()));

        let mut current_line = 1;

        let mut output = String::new();

        let filename = path.as_ref().file_name().unwrap();
        let canonical_path = self.canonicalize_local_path(filename, &cwd);

        let format_line = |current| format!("#line {} \"{}\"\n", current, canonical_path.display());

        output.push_str(&format_line(current_line));

        for line in file_content.as_ref().lines() {
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
                output.push_str(&included);
                if !output.ends_with('\n') {
                    output.push('\n');
                }
                output.push_str(&format_line(current_line + 1));
            } else {
                output.push_str(line);
                output.push('\n');
            }
            current_line += 1;
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
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            context: Context::default(),
            defines: HashMap::new(),
            directives: vec![],
            skip_lines: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> Preprocessor {
        let context = Context::new("test.c", "./tests/fixtures", Vec::new());
        Preprocessor::new(context)
    }

    mod test_local_includes {
        use indoc::indoc;

        use super::*;

        fn test_include(input: &str, expected: &str) {
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
        fn test_include_error(input: &str, expected: &str) {
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

        #[test]
        fn test_includes_the_file() {
            let input = r#"#include "include/simple.h""#;

            let expected = indoc! {r#"
                #line 1 "/test.c"
                #line 1 "/include/simple.h"
                1 + 2 + 3 + 4 + 5;
                #line 2 "/test.c"
            "#};

            test_include(input, expected);
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

            test_include(input, expected);
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

            test_include(input, expected);
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

            test_include(input, expected);
        }

        #[test]
        fn test_errors_for_nonexistent_paths() {
            let input = r#"#include "/askdf/foo.h""#;

            test_include_error(input, "No such file or directory");
        }

        #[test]
        fn test_errors_for_traversal_attacks() {
            let input = r#"#include "/../../some_file.h""#;

            test_include_error(input, "Attempt to include a file outside the root");
        }
    }
}
