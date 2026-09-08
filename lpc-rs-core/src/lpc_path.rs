//! LPC path inputs and resolution beneath a mudlib root.

use std::{
    ffi::OsStr,
    fmt::{self, Display, Formatter},
    ops::Deref,
    path::{Path, PathBuf},
};

use path_absolutize::{Absolutize, path_dedot::ParseDot};

use crate::mangle::Mangle;

mod root;
pub use root::{LibRoot, PathError, ResolvedPath, SourceName};

/// A path input whose host or in-game origin survives compilation.
#[derive(Clone, PartialEq, Eq)]
pub struct LpcPath(PathKind);

#[derive(Clone, PartialEq, Eq)]
enum PathKind {
    Server(PathBuf),
    InGame(PathBuf),
}

impl LpcPath {
    /// A host path, lexically absolutized against the driver's working directory.
    pub fn new_server(path: impl AsRef<Path>) -> Self {
        let path = path.as_ref();
        let absolute = path
            .absolutize()
            .map_or_else(|_| path.to_owned(), |p| p.into_owned());
        Self(PathKind::Server(absolute))
    }

    /// Resolve an in-game input, retaining the empty invalid-path sentinel.
    /// Use [`LibRoot::resolve`] when a file operation needs a checked path.
    pub fn new_in_game(
        path: impl AsRef<Path>,
        cwd: impl AsRef<Path>,
        lib_dir: impl AsRef<OsStr>,
    ) -> Self {
        LibRoot::new(Path::new(lib_dir.as_ref())).in_game(path, cwd)
    }

    /// An existing in-game identity, without input normalization.
    pub fn in_game(path: PathBuf) -> Self {
        Self(PathKind::InGame(path))
    }

    /// Replace this input's extension, preserving its origin.
    pub fn with_extension(&self, extension: impl AsRef<OsStr>) -> Self {
        self.map_path(|path| path.with_extension(extension))
    }

    /// The object's source: strip one trailing `.c`, then append `.c`.
    /// Thus `/x.h` names `/x.h.c`, while `/x.c` still names `/x.c`.
    pub fn source_file(&self) -> Self {
        self.map_path(|path| {
            let text = path.to_string_lossy();
            PathBuf::from(format!("{}.c", text.strip_suffix(".c").unwrap_or(&text)))
        })
    }

    /// The source's object identity, stripping `.c` before an optional clone suffix.
    pub fn object_file(&self, clone_id: Option<usize>) -> Self {
        self.map_path(|path| {
            let text = path.to_string_lossy();
            let name = text.strip_suffix(".c").unwrap_or(&text);
            match clone_id {
                Some(id) => format!("{name}#{id}").into(),
                None => name.into(),
            }
        })
    }

    /// Whether the final `#`-separated part parses as a clone number.
    pub fn is_clone(&self) -> bool {
        self.to_str().is_some_and(|s| {
            s.rsplit('#')
                .next()
                .is_some_and(|n| n.parse::<u64>().is_ok())
        })
    }

    /// The input's parent directory, preserving its origin.
    pub fn cwd(&self) -> Self {
        self.map_path(|path| {
            let mut parent = path.to_owned();
            parent.pop();
            parent
        })
    }

    fn map_path(&self, f: impl FnOnce(&Path) -> PathBuf) -> Self {
        Self(match &self.0 {
            PathKind::Server(path) => PathKind::Server(f(path)),
            PathKind::InGame(path) => PathKind::InGame(f(path)),
        })
    }
}

impl Mangle for LpcPath {
    fn mangle(&self) -> String {
        self.to_string_lossy().into_owned()
    }
}

// AsRef<OsStr> would let &LpcPath re-enter this conversion and lose its origin.
impl<T: Into<PathBuf>> From<T> for LpcPath {
    fn from(path: T) -> Self {
        Self::in_game(path.into().parse_dot_from("/").into_owned())
    }
}

impl AsRef<str> for LpcPath {
    fn as_ref(&self) -> &str {
        match &self.0 {
            PathKind::Server(path) => path.to_str().unwrap_or("."),
            PathKind::InGame(path) => path.to_str().unwrap_or("/"),
        }
    }
}

impl AsRef<Path> for LpcPath {
    fn as_ref(&self) -> &Path {
        match &self.0 {
            PathKind::Server(path) | PathKind::InGame(path) => path,
        }
    }
}

impl Display for LpcPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.0 {
            PathKind::Server(_) => f.write_str("<host path>"),
            PathKind::InGame(path) => Display::fmt(&path.display(), f),
        }
    }
}

impl fmt::Debug for LpcPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Default for LpcPath {
    fn default() -> Self {
        Self(PathKind::Server("/".into()))
    }
}

impl Deref for LpcPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_file_is_the_object_key_plus_dot_c() {
        for (given, expected) in [
            ("/x", "/x.c"),
            ("/x.c", "/x.c"),
            ("/include/x.h", "/include/x.h.c"),
            ("/std/sword#3", "/std/sword#3.c"),
            ("/d/foo.bar/baz", "/d/foo.bar/baz.c"),
        ] {
            assert_eq!(LpcPath::from(given).source_file(), LpcPath::from(expected));
            assert_eq!(
                LpcPath::new_server(format!("/lib{given}")).source_file(),
                LpcPath::new_server(format!("/lib{expected}"))
            );
        }
    }

    #[test]
    fn host_identity_is_explicit_and_never_formatted() {
        let path = LpcPath::new_server("/private/mud/lib/file.c");
        assert_eq!(path.mangle(), "/private/mud/lib/file.c");
        assert_eq!(path.to_string(), "<host path>");
        assert_eq!(format!("{path:?}"), "<host path>");
    }
}
