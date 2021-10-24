use bstr::ByteSlice;
use path_absolutize::Absolutize;
use std::{
    borrow::Cow,
    ffi::{OsStr, OsString},
    fmt::{Display, Formatter},
    ops::Deref,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum LpcPath {
    /// Represent an on-server path. Relative paths are relative to the running process' dir.
    Server(PathBuf),

    /// Represent an in-game path. Relative paths are relative to `lib_dir`.
    InGame(PathBuf),
}

impl LpcPath {
    /// Create a new on-server path. This essentially means the path will be treated
    /// as it's passed. An attempt will be made to canonicalize the path before storage.
    pub fn new_server<T>(path: T) -> Self
    where
        T: AsRef<Path>,
    {
        let canon = match path.as_ref().absolutize() {
            Ok(x) => x.to_path_buf(),
            Err(_) => path.as_ref().to_path_buf(),
        };

        Self::Server(canon)
    }

    /// Create a new in-game path.
    /// This will expand relative paths to (in-game) absolute, if necessary.
    ///
    /// # Arguments
    /// `path` - The relative path to the file
    /// `cwd` - The current directory used when resolving `.`s in `path`.
    /// `lib_dir` - The `LIB_DIR` configuration that's in use.
    pub fn new_in_game<T, U, V>(path: T, cwd: U, lib_dir: V) -> Self
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
        V: AsRef<OsStr>,
    {
        let buf = canonicalize_in_game_path(path, cwd, lib_dir);
        Self::InGame(buf)
    }

    /// Return this LpcPath, but with the passed extension added.
    pub fn with_extension<S>(&self, extension: S) -> LpcPath
    where
        S: AsRef<OsStr>,
    {
        match self {
            LpcPath::Server(x) => LpcPath::Server(x.with_extension(extension)),
            LpcPath::InGame(x) => LpcPath::InGame(x.with_extension(extension)),
        }
    }

    /// Return the full, absolute on-server path for a file
    ///
    /// # Arguments
    ///
    /// `root` - The root to use for resolving `InGame` variants.
    pub fn as_server<P>(&self, root: P) -> Cow<Path>
    where
        P: AsRef<Path>,
    {
        match self {
            LpcPath::Server(x) => Cow::Borrowed(x),
            LpcPath::InGame(x) => {
                let rt = canonicalize_server_path(x, "/", root.as_ref());

                Cow::Owned(rt)
            }
        }
    }

    /// Return the in-game path represented by this instance
    ///
    /// # Arguments
    ///
    /// `root` - The root to strip off of `Server` variants.
    pub fn as_in_game<P>(&self, root: P) -> Cow<Path>
    where
        P: AsRef<Path>,
    {
        match self {
            LpcPath::Server(x) => match x.strip_prefix(root) {
                Ok(y) => PathBuf::from(format!("/{}", y.display())).into(),
                Err(_) => x.into(),
            },
            LpcPath::InGame(x) => x.into(),
        }
    }

    /// Is this path underneath the given root? Used to check for traversal attacks
    pub fn is_within_root<P>(&self, root: P) -> bool
    where
        P: AsRef<Path>,
    {
        match self {
            LpcPath::Server(x) => {
                let slice = <[u8]>::from_path(x).expect(
                    "Path must be valid UTF-8 on Windows. This error should never occur on Unix.",
                );
                slice.starts_with_str(root.as_ref().as_os_str().as_bytes())
            }
            LpcPath::InGame(x) => !x.as_os_str().is_empty(),
        }
    }
}

pub trait ToLpcPath {
    fn to_lpc_path(&self) -> LpcPath;
}

impl From<PathBuf> for LpcPath {
    fn from(pb: PathBuf) -> Self {
        Self::new_server(pb)
    }
}

impl From<&str> for LpcPath {
    fn from(s: &str) -> Self {
        Self::new_server(s)
    }
}

impl From<Cow<'_, Path>> for LpcPath {
    fn from(c: Cow<'_, Path>) -> Self {
        Self::new_server(c.as_os_str())
    }
}

impl From<OsString> for LpcPath {
    fn from(c: OsString) -> Self {
        Self::new_server(c.as_os_str())
    }
}

impl From<&LpcPath> for LpcPath {
    fn from(p: &LpcPath) -> Self {
        p.clone()
    }
}

impl ToLpcPath for PathBuf {
    fn to_lpc_path(&self) -> LpcPath {
        LpcPath::new_server(self.as_os_str())
    }
}

impl ToLpcPath for &Path {
    fn to_lpc_path(&self) -> LpcPath {
        LpcPath::Server(self.to_path_buf())
    }
}

impl ToLpcPath for &str {
    fn to_lpc_path(&self) -> LpcPath {
        LpcPath::new_server(self)
    }
}

impl AsRef<str> for LpcPath {
    fn as_ref(&self) -> &str {
        match self {
            // TODO: terrible defaults here
            LpcPath::Server(x) => x.to_str().unwrap_or("."),
            LpcPath::InGame(x) => x.to_str().unwrap_or("/"),
        }
    }
}

impl AsRef<OsStr> for LpcPath {
    fn as_ref(&self) -> &OsStr {
        match self {
            LpcPath::Server(x) | LpcPath::InGame(x) => x.as_os_str(),
        }
    }
}

impl AsRef<Path> for LpcPath {
    fn as_ref(&self) -> &Path {
        match self {
            LpcPath::Server(x) => x,
            LpcPath::InGame(x) => x,
        }
    }
}

impl Display for LpcPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let p: &Path = self.as_ref();

        write!(f, "{}", p.display())
    }
}

impl Default for LpcPath {
    fn default() -> Self {
        Self::Server("/".into())
    }
}

impl Deref for LpcPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        match self {
            LpcPath::Server(x) | LpcPath::InGame(x) => x,
        }
    }
}

/// Convert an in-game path, relative or absolute, to a canonical, absolute *on-server* path.
/// This function is used for resolving included files.
///
/// # Arguments
/// `path` - An in-game path.
/// `cwd` - The current working directory, needed to resolve relative paths.
/// `lib_dir` - The game's lib directory.
pub fn canonicalize_server_path<T, U, V>(path: T, cwd: U, lib_dir: V) -> PathBuf
where
    T: AsRef<Path>,
    U: AsRef<Path>,
    V: Into<OsString>,
{
    let path_ref = path.as_ref().as_os_str();
    let mut buf: [u8; 4] = [0; 4];
    let sep: &str = std::path::MAIN_SEPARATOR.encode_utf8(&mut buf);
    let os_sep = OsString::from(&sep);
    let mut lib_string = lib_dir.into();

    // turn relative paths into absolute
    let slice = <[u8]>::from_os_str(path_ref).expect("Paths must be valid UTF-8 on Windows");
    if !slice.starts_with_str(sep) {
        lib_string.push(&os_sep);
        lib_string.push(cwd.as_ref().as_os_str());
    }

    lib_string.push(&os_sep);
    lib_string.push(&path_ref);

    Path::new(
        &lib_string
            .to_string_lossy()
            .replace("//", "/")
            .replace("/./", "/"),
    )
    .absolutize()
    .unwrap()
    .to_path_buf()
}

/// Convert an in-game path, relative or absolute, to a canonical, absolute *in-game* path.
/// This just returns the canonical path, without the in-game root dir at the front.
///
/// # Arguments
/// `path` - An in-game path.
/// `cwd` - The current working directory, needed to resolve relative paths.
/// `lib_dir` - The game lib directory.
pub fn canonicalize_in_game_path<T, U, V>(path: T, cwd: U, lib_dir: V) -> PathBuf
where
    T: AsRef<Path>,
    U: AsRef<Path>,
    V: AsRef<OsStr>,
{
    let canon = canonicalize_server_path(path, cwd, lib_dir.as_ref());

    // Strip off the root_dir prefix, then clean up the rest.
    let stripped = canon
        .strip_prefix(lib_dir.as_ref())
        .unwrap_or_else(|_| AsRef::<Path>::as_ref(""));
    let mut result = stripped
        .to_string_lossy()
        .into_owned()
        .replace("//", "/")
        .replace("/./", "/");
    if !result.is_empty() && !result.starts_with('/') {
        result = format!("/{}", result);
    }

    PathBuf::from(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    const LIB_DIR: &str = "/home/mud/lib";
    const CWD: &str = "/home/wizard/mpd";

    #[test]
    fn test_canonicalize_server_path() {
        assert_eq!(
            canonicalize_server_path("my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/mud/lib/home/wizard/mpd/my_file.c"
        );
        assert_eq!(
            canonicalize_server_path("./my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/mud/lib/home/wizard/mpd/my_file.c"
        );
        assert_eq!(
            canonicalize_server_path("../my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/mud/lib/home/wizard/my_file.c"
        );
        assert_eq!(
            canonicalize_server_path("/my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/mud/lib/my_file.c"
        );
        assert_eq!(
            canonicalize_server_path("../../../../../../../../../my_file.c", CWD, LIB_DIR)
                .as_os_str(),
            "/my_file.c"
        );

        assert_eq!(
            canonicalize_server_path("root.c", ".", LIB_DIR).as_os_str(),
            "/home/mud/lib/root.c"
        );
        // assert_eq!(canonicalize_server_path("root.c", "/", LIB_DIR).as_os_str(), "/home/mud/lib/root.c");
        assert_eq!(
            canonicalize_server_path("./root.c", "/foobar", LIB_DIR).as_os_str(),
            "/home/mud/lib/foobar/root.c"
        );
    }

    #[test]
    fn test_canonicalize_in_game_path() {
        assert_eq!(
            canonicalize_in_game_path("my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/wizard/mpd/my_file.c"
        );
        assert_eq!(
            canonicalize_in_game_path("./my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/wizard/mpd/my_file.c"
        );
        assert_eq!(
            canonicalize_in_game_path("../my_file.c", CWD, LIB_DIR).as_os_str(),
            "/home/wizard/my_file.c"
        );
        assert_eq!(
            canonicalize_in_game_path("/my_file.c", CWD, LIB_DIR).as_os_str(),
            "/my_file.c"
        );
        assert_eq!(
            canonicalize_in_game_path("../../../../../../../../../my_file.c", CWD, LIB_DIR)
                .as_os_str(),
            ""
        );
    }

    #[test]
    fn test_as_in_game() {
        assert_eq!(
            LpcPath::new_server("/some/root/foo.c")
                .as_in_game("/some/root")
                .as_os_str(),
            "/foo.c"
        );
    }

    #[test]
    fn test_new_in_game() {
        assert_eq!(
            LpcPath::new_in_game("/some/root/foo.c", "/taccos", "/my_hero").as_os_str(),
            "/some/root/foo.c"
        );

        assert_eq!(
            LpcPath::new_in_game("./some/root/foo.c", "/taccos", "/my_hero").as_os_str(),
            "/taccos/some/root/foo.c"
        );

        assert_eq!(
            LpcPath::new_in_game("../some/root/foo.c", "/marf/taccos", "/my_hero").as_os_str(),
            "/marf/some/root/foo.c"
        );

        assert_eq!(
            LpcPath::new_in_game("../../../../../../some/foo.c", "/marf/taccos", "/my_hero")
                .as_os_str(),
            ""
        );

        assert_eq!(
            LpcPath::new_in_game("/../../../../../../some/foo.c", "/marf/taccos", "/my_hero")
                .as_os_str(),
            ""
        );
    }
}
