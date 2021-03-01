use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::HashMap, fs, path::Path, fmt};
use std::error::Error;
use std::fmt::{Display, Formatter};

/// Handle preprocessing

#[derive(Debug)]
pub struct PreprocessorError(String);

impl Error for PreprocessorError {}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.0)
    }
}

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
    /// The (relative to `root_dir`) path (including filename) of final file that we're emitting.
    /// Even if this starts with a `/`, it remains relative to `root_dir`.
    path: String,

    /// The true on-disk root dir, where in-game absolute paths point to.
    root_dir: String,
    include_dirs: Vec<String>,

    defines: HashMap<String, String>,

    directives: Vec<PreprocessorDirective>,

    current_path: String,
    current_line: usize,

    /// Are we currently within a block that is `if`'d out?
    skip_lines: bool,
}

impl Preprocessor {
    pub fn new(path: &str, root_dir: &str, include_dirs: Vec<String>) -> Self {
        Self {
            path: path.to_string(),
            root_dir: root_dir.to_string(),
            include_dirs,
            ..Self::default()
        }
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

    pub fn scan<T>(&mut self, path: T, file_content: &str) -> Result<String, PreprocessorError>
    where
        T: AsRef<Path>,
    {
        lazy_static! {
            static ref SYS_INCLUDE: Regex =
                Regex::new(r"\A\s*#\s*include\s+<([^>]+)>\s*\z").unwrap();
            static ref LOCAL_INCLUDE: Regex =
                Regex::new(r#"\A\s*#\s*include\s+"([^"]+)"\s*\z"#).unwrap();
        }

        self.current_path = String::from(path.as_ref().to_string_lossy());
        self.current_line = 1;

        let mut output = String::new();

        for line in file_content.lines() {
            if let Some(captures) = SYS_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();
                self.directives.push(PreprocessorDirective::SysInclude(
                    String::from(matched.as_str()),
                    self.current_line,
                ));
            } else if let Some(captures) = LOCAL_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();
                let cwd = path.as_ref().parent().unwrap().to_str().unwrap();
                let included = self.include_local_file(matched.as_str(), cwd)?;
                output.push_str(&included);
                if !output.ends_with("\n") {
                    output.push_str("\n");
                }
            } else {
                output.push_str(line);
                output.push_str("\n");
            }
            self.current_line += 1;
        }

        Ok(output)
    }

    fn include_local_file(&mut self, path: &str, cwd: &str) -> Result<String, PreprocessorError> {
        let root_path = String::from(Path::new(&self.root_dir).canonicalize().unwrap().to_str().unwrap());
        let sep = String::from(std::path::MAIN_SEPARATOR);
        // Do this the hard way because .join/.push overwrite if the arg starts with "/"
        let localized_path = if path.starts_with(&sep) {
            root_path + &sep + path
        } else {
            root_path + &sep + cwd + &sep + path
        };

        let canon_include_path = match fs::canonicalize(&localized_path) {
            Ok(pathbuf) => pathbuf,
            Err(e) => {
                return Err(PreprocessorError(format!(
                    "error canonicalizing the include file ({}): {:?}",
                    localized_path,
                    e
                )))
            }
        };

        let true_root = match fs::canonicalize(&self.root_dir) {
            Ok(pathbuf) => pathbuf,
            Err(e) => return Err(PreprocessorError(format!("{:?}", e))),
        };

        if !canon_include_path.starts_with(true_root) {
            return Err(PreprocessorError(format!(
                "Attempt to include a file outside the root: `{}` (expanded to `{}`)",
                path,
                canon_include_path.display()
            )));
        }

        let file_content = fs::read_to_string(&canon_include_path).unwrap();
        self.scan(path, &file_content)
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            path: "UNSET".to_string(),
            root_dir: ".".to_string(),
            include_dirs: vec![],
            defines: HashMap::new(),
            directives: vec![],
            current_path: "UNSET".to_string(),
            current_line: 0,
            skip_lines: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> Preprocessor {
        Preprocessor::new("test.c", "./tests/fixtures", vec![])
    }

    mod test_local_includes {
        use indoc::indoc;
        use super::*;

        fn test_include(input: &str, expected: &str) {
            let mut preprocessor = fixture();
            match preprocessor.scan("test.c", input) {
                Ok(result) => {
                    assert_eq!(result, expected)
                }
                Err(e) => {
                    panic!(e)
                }
            }
        }

        #[test]
        fn test_includes_the_file() {
            let input = r#"#include "include/simple.h""#;

            test_include(input, "1 + 2 + 3 + 4 + 5;\n");
        }

        #[test]
        fn test_includes_multiple_levels() {
            let input = r#"#include "include/level_2/two_level.h""#;

            test_include(input, "1 + 2 + 3 + 4 + 5;\n");
        }

        #[test]
        fn test_includes_multiple_files() {
            let input = indoc! {r#"
                #include "include/level_2/two_level.h"
                int j = 123;
                #include "include/simple.h"
            "#};

            let expected = indoc! {r#"
                1 + 2 + 3 + 4 + 5;
                int j = 123;
                1 + 2 + 3 + 4 + 5;
            "#};

            test_include(input, expected);
        }

        #[test]
        fn test_includes_absolute_paths() {
            let input = r#"#include "/include/simple.h""#;

            test_include(input, "1 + 2 + 3 + 4 + 5;\n");
        }
    }
}
