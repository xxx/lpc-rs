use std::{collections::HashMap, fs, path::Path};

use lazy_static::lazy_static;
use regex::Regex;

use crate::errors::preprocessor_error::PreprocessorError;
use path_absolutize::Absolutize;
use std::path::PathBuf;

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
    /// The true on-disk root dir, where in-game absolute paths point to.
    root_dir: PathBuf,
    include_dirs: Vec<PathBuf>,

    defines: HashMap<String, String>,

    directives: Vec<PreprocessorDirective>,

    /// Are we currently within a block that is `if`'d out?
    skip_lines: bool,
}

lazy_static! {
    static ref DOUBLE_SLASHES: Regex = Regex::new(r"//").unwrap();
}

impl Preprocessor {
    /// Create a new `Preprocessor`
    ///
    /// # Arguments
    /// `root_dir` - The path to the root of the in-game filesystem (sometimes a.k.a `LIBDIR`, etc.)
    /// `include_dirs` - A vector of *in-game* paths to be used for searching for system includes.
    ///     Searches will be done in the order given by this vector.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::preprocessor::Preprocessor;
    ///
    /// let preprocessor = Preprocessor::new("/home/mud/lib", vec!["/include", "/sys"]);
    /// ```
    pub fn new(root_dir: &str, include_dirs: Vec<&str>) -> Self {
        Self {
            root_dir: PathBuf::from(root_dir).absolutize().unwrap().to_path_buf(),
            include_dirs: include_dirs.iter().map(|i| PathBuf::from(*i)).collect(),
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

    /// Convert an in-game path, relative or absolute, to a canonical on-server path.
    /// This function is used for resolving included files.
    ///
    /// # Arguments
    /// `path` - An in-game path.
    /// `cwd` - The current working directory, needed to resolve relative paths.
    fn canonicalize_path<T, U>(&self, path: T, cwd: U) -> Result<PathBuf, PreprocessorError>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let path_ref = path.as_ref().to_str().unwrap();
        let sep = String::from(std::path::MAIN_SEPARATOR);
        let root_string = String::from(self.root_dir.to_str().unwrap());
        // Do this the hard way because .join/.push overwrite if the arg starts with "/"
        let localized_path = if path_ref.starts_with(&sep) {
            root_string + &sep + path_ref
        } else {
            root_string + &sep + cwd.as_ref().to_str().unwrap() + &sep + path_ref
        };

        Ok(
            Path::new(&*DOUBLE_SLASHES.replace_all(&localized_path, "/"))
                .absolutize()
                .unwrap()
                .to_path_buf(),
        )
    }

    /// Convert an in-game path, relative or absolute, to a canonical in-game path.
    ///
    /// # Arguments
    /// `path` - An in-game path.
    /// `cwd` - The current working directory, needed to resolve relative paths.
    fn canonicalize_local_path<T, U>(&self, path: T, cwd: U) -> Result<PathBuf, PreprocessorError>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        println!(
            "canon local {}, {}",
            path.as_ref().display(),
            cwd.as_ref().display()
        );
        let canon = self.canonicalize_path(path, cwd).unwrap();
        let buf = canon.to_str().unwrap();
        let root_str = self.root_dir.to_str().unwrap();

        Ok(PathBuf::from(&*DOUBLE_SLASHES.replace_all(
            &buf.chars().skip(root_str.len()).collect::<String>(),
            "/",
        )))
    }

    /// Scan a file's contents, transforming as necessary according to the preprocessing rules.
    ///
    /// # Arguments
    /// `path` - The name of the file being scanned.
    /// `cwd` - The current working directory on-server, used to resolve relative links
    /// `file_content` - The actual content of the file to scan.
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::preprocessor::Preprocessor;
    /// let mut preprocessor = Preprocessor::new(".", vec!["/include", "/sys/include"]);
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
    pub fn scan<T, U>(
        &mut self,
        path: T,
        cwd: U,
        file_content: &str,
    ) -> Result<String, PreprocessorError>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        lazy_static! {
            static ref SYS_INCLUDE: Regex =
                Regex::new(r"\A\s*#\s*include\s+<([^>]+)>\s*\z").unwrap();
            static ref LOCAL_INCLUDE: Regex =
                Regex::new(r#"\A\s*#\s*include\s+"([^"]+)"\s*\z"#).unwrap();
        }

        let mut current_line = 1;

        let mut output = String::new();

        let filename = path.as_ref().file_name().unwrap().to_str().unwrap();
        let canonical_path = self.canonicalize_local_path(filename, &cwd).unwrap();

        let format_line = |current| format!("#line {} \"{}\"\n", current, canonical_path.display());

        output.push_str(&format_line(current_line));

        for line in file_content.lines() {
            if let Some(captures) = SYS_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();
                self.directives.push(PreprocessorDirective::SysInclude(
                    String::from(matched.as_str()),
                    current_line,
                ));
            } else if let Some(captures) = LOCAL_INCLUDE.captures(line) {
                let matched = captures.get(1).unwrap();
                let included = self.include_local_file(matched.as_str(), &cwd)?;
                output.push_str(&included);
                if !output.ends_with("\n") {
                    output.push_str("\n");
                }
                output.push_str(&format_line(current_line + 1));
            } else {
                output.push_str(line);
                output.push_str("\n");
            }
            current_line += 1;
        }

        Ok(output)
    }

    fn include_local_file<T, U>(&mut self, path: T, cwd: U) -> Result<String, PreprocessorError>
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let canon_include_path = self.canonicalize_path(&path, &cwd)?;
        let true_root = PathBuf::from(&self.root_dir);

        if !canon_include_path.starts_with(true_root) {
            return Err(PreprocessorError(format!(
                "Attempt to include a file outside the root: `{}` (expanded to `{}`)",
                path.as_ref().display(),
                canon_include_path.display()
            )));
        }

        let file_content = match fs::read_to_string(&canon_include_path) {
            Ok(content) => content,
            Err(e) => {
                return Err(PreprocessorError(format!(
                    "Error including file `{}`: {:?}",
                    path.as_ref().display(),
                    e
                )))
            }
        };

        let local_canon_include_path = self.canonicalize_local_path(&path, &cwd)?;
        let filename = local_canon_include_path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();
        let cwd = local_canon_include_path.parent().unwrap().to_str().unwrap();
        println!("about to scan {} {}", filename, cwd);
        self.scan(filename, cwd, &file_content)
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            root_dir: PathBuf::from("."),
            include_dirs: vec![],
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
        Preprocessor::new("./tests/fixtures", vec![])
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
