use std::{
    borrow::Cow,
    error::Error,
    fmt,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;
use relative_path::RelativePath;

use super::{LpcPath, PathKind};

/// The LPC-visible name of a source, never a host filename.
#[derive(Clone, PartialEq, Eq)]
pub struct SourceName(Option<String>);

impl SourceName {
    /// The name as LPC text.
    pub fn as_str(&self) -> &str {
        self.0.as_deref().unwrap_or("<outside mudlib>")
    }
}

impl fmt::Display for SourceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for SourceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// A resolution failure carrying only an LPC-visible name.
#[derive(Debug)]
pub struct PathError(SourceName);

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "attempt to access a file outside of lib_dir: `{}`",
            self.0
        )
    }
}

impl Error for PathError {}

/// A materialized host location paired with its safe name and original identity.
/// Constructed by [`LibRoot`]; only `resolve` and `confine` enforce containment.
#[derive(Clone)]
pub struct ResolvedPath {
    input: LpcPath,
    server: PathBuf,
    name: SourceName,
}

impl ResolvedPath {
    /// The host location, for filesystem operations and internal identity.
    pub fn server(&self) -> &Path {
        &self.server
    }

    /// The safe name for diagnostics and permission applies.
    pub fn name(&self) -> &SourceName {
        &self.name
    }

    /// The original path input, retaining host provenance for compilation.
    pub fn input(&self) -> &LpcPath {
        &self.input
    }

    /// Place this basename under `directory`, preserving the destination's paired identity.
    pub fn in_directory(&self, directory: &Self) -> Option<Self> {
        let filename = self.server.file_name()?;
        Some(Self {
            input: directory.input.map_path(|path| path.join(filename)),
            server: directory.server.join(filename),
            name: SourceName(directory.name.0.as_ref().map(|name| {
                Path::new(name)
                    .join(filename)
                    .to_string_lossy()
                    .into_owned()
            })),
        })
    }

    /// Append the save-file suffix to the location and its in-game identity.
    pub fn save_file(mut self) -> Self {
        self.input = self.input.map_path(|path| {
            let mut text = path.as_os_str().to_owned();
            text.push(".o");
            text.into()
        });
        self.server.as_mut_os_string().push(".o");
        if let Some(name) = &mut self.name.0 {
            name.push_str(".o");
        }
        self
    }
}

impl fmt::Display for ResolvedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

impl fmt::Debug for ResolvedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

/// Lexical LPC path resolution relative to the configured mudlib directory.
/// This never resolves symlinks or accesses the filesystem.
#[derive(Clone, Copy)]
pub struct LibRoot<'a> {
    root: &'a Path,
}

impl<'a> LibRoot<'a> {
    /// Use the configured, already-absolutized lib directory.
    pub fn new(root: &'a (impl AsRef<Path> + ?Sized)) -> Self {
        Self {
            root: root.as_ref(),
        }
    }

    /// Normalize an LPC input, preserving the legacy empty invalid-path form.
    pub fn in_game(&self, path: impl AsRef<Path>, cwd: impl AsRef<Path>) -> LpcPath {
        let host = self.join(path.as_ref(), cwd.as_ref());
        let relative = host
            .strip_prefix(self.root)
            .unwrap_or_else(|_| Path::new(""));
        let game = if relative.as_os_str().is_empty() {
            PathBuf::new()
        } else {
            Path::new("/").join(relative)
        };
        LpcPath::in_game(game)
    }

    /// Resolve LPC text against `cwd` and reject a final location outside the lib.
    pub fn resolve(
        &self,
        path: impl AsRef<Path>,
        cwd: impl AsRef<Path>,
    ) -> Result<ResolvedPath, PathError> {
        self.confine(&self.in_game(path, cwd))
    }

    /// Resolve an existing input and enforce lexical containment before IO.
    pub fn confine(&self, path: &LpcPath) -> Result<ResolvedPath, PathError> {
        let resolved = self.source(path);
        if path.as_os_str().is_empty() || !resolved.server.starts_with(self.root) {
            Err(PathError(self.source_name(path)))
        } else {
            Ok(resolved)
        }
    }

    /// Normalize an in-game include directory, allowing the root and returning `None` for escapes.
    pub fn include_dir(&self, path: impl AsRef<Path>) -> Option<LpcPath> {
        self.localize(&self.join(path.as_ref(), Path::new("/")))
            .map(LpcPath::in_game)
    }

    /// Locate a compiler input without containment, retaining a safe display name.
    pub fn source(&self, path: &LpcPath) -> ResolvedPath {
        let server = match &path.0 {
            PathKind::Server(host) => host.clone(),
            PathKind::InGame(game) => self.join(game, Path::new("/")),
        };
        let name = SourceName(
            self.localize(&server)
                .map(|p| p.to_string_lossy().into_owned()),
        );
        ResolvedPath {
            input: path.clone(),
            server,
            name,
        }
    }

    /// The in-game name, or an opaque label when a host source lies outside the lib.
    pub fn source_name(&self, path: &LpcPath) -> SourceName {
        SourceName(
            self.game_path(path)
                .map(|p| p.to_string_lossy().into_owned()),
        )
    }

    /// Localize an object identity, including the historical lib-prefix spelling collision.
    pub fn object_name(&self, path: &LpcPath) -> SourceName {
        if let PathKind::InGame(game) = &path.0
            && let Ok(relative) = game.strip_prefix(self.root)
        {
            return SourceName(Some(
                Path::new("/").join(relative).to_string_lossy().into_owned(),
            ));
        }
        self.source_name(path)
    }

    /// The program's in-game identity, retaining an outside host identity internally.
    pub fn program_path(&self, path: &LpcPath) -> LpcPath {
        self.game_path(path)
            .map_or_else(|| path.clone(), |p| LpcPath::in_game(p.into_owned()))
    }

    /// The include directory; an unrestricted outside source keeps its host cwd internally.
    pub fn source_cwd(&self, path: &LpcPath) -> PathBuf {
        let game = self
            .game_path(path)
            .unwrap_or_else(|| Cow::Borrowed(path.as_ref()));
        game.parent().unwrap_or_else(|| Path::new("/")).to_owned()
    }

    /// The existing inheritance base, including host-origin compilation's filename base.
    pub fn inheritance_cwd(&self, path: &LpcPath) -> PathBuf {
        match &path.0 {
            PathKind::Server(_) => self
                .game_path(path)
                .unwrap_or_else(|| Cow::Borrowed(path.as_ref()))
                .into_owned(),
            PathKind::InGame(_) => path.cwd().to_path_buf(),
        }
    }

    fn game_path<'p>(&self, path: &'p LpcPath) -> Option<Cow<'p, Path>> {
        match &path.0 {
            PathKind::InGame(game) => Some(Cow::Borrowed(game)),
            PathKind::Server(host) => self.localize(host).map(Cow::Owned),
        }
    }

    fn localize(&self, host: &Path) -> Option<PathBuf> {
        host.strip_prefix(self.root)
            .ok()
            .map(|p| Path::new("/").join(p))
    }

    fn join(&self, path: &Path, cwd: &Path) -> PathBuf {
        let root = if self.root.as_os_str().is_empty() {
            Path::new("/")
        } else {
            self.root
        };
        let root = root.absolutize().unwrap_or(Cow::Borrowed(root));
        let base = if path.is_absolute() {
            root.into_owned()
        } else {
            RelativePath::new(&cwd.to_string_lossy()).to_logical_path(root)
        };
        let joined = RelativePath::new(&path.to_string_lossy()).to_logical_path(base);
        joined
            .absolutize()
            .map_or_else(|_| joined.clone(), |p| p.into_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_inputs_with_the_existing_virtual_root_rules() {
        let root = LibRoot::new("/home/mud/lib");
        for (given, cwd, expected) in [
            (
                "my_file.c",
                "/home/wizard/mpd",
                "/home/wizard/mpd/my_file.c",
            ),
            ("../my_file.c", "/home/wizard/mpd", "/home/wizard/my_file.c"),
            ("/my_file.c", "/home/wizard/mpd", "/my_file.c"),
            ("/../lib/item", "/room", "/item"),
            ("a//./b/../é.c", "/room", "/room/a/é.c"),
            ("", "/room", "/room"),
        ] {
            let path = root.resolve(given, cwd).unwrap();
            assert_eq!(path.name().as_str(), expected);
            assert_eq!(
                path.server(),
                Path::new(&format!("/home/mud/lib{expected}"))
            );
        }
        for given in ["/", "/../secret", "../../../../secret"] {
            assert!(root.resolve(given, "/").is_err(), "{given}");
        }
    }

    #[test]
    fn host_names_are_redacted_without_collapsing_identity() {
        let root = LibRoot::new("/private/mud/lib");
        for file in ["/private/mud/lib-other/a.c", "/secret/b.c"] {
            let input = LpcPath::new_server(file);
            let path = root.source(&input);
            assert_eq!(path.name().as_str(), "<outside mudlib>");
            assert_eq!(path.server(), Path::new(file));
            assert_eq!(root.program_path(&input), input);
            assert!(
                !root
                    .confine(&input)
                    .unwrap_err()
                    .to_string()
                    .contains("/private")
            );
        }
        assert_eq!(
            root.source_name(&LpcPath::new_server("/private/mud/lib/a.c"))
                .as_str(),
            "/a.c"
        );
    }

    #[test]
    fn include_directories_reject_escapes_without_losing_the_root() {
        let root = LibRoot::new("/home/mud/lib");
        for path in ["/../lib-other", "../private", "../../secret"] {
            assert!(root.include_dir(path).is_none(), "{path}");
        }
        for (path, expected) in [("/", "/"), ("", "/"), ("/sys/../include", "/include")] {
            assert_eq!(root.include_dir(path).unwrap().as_os_str(), expected);
        }
    }

    #[test]
    fn save_suffix_keeps_the_path_pair_together() {
        let path = LibRoot::new("/home/mud/lib")
            .resolve("x.o", "/")
            .unwrap()
            .save_file();
        assert_eq!(path.name().as_str(), "/x.o.o");
        assert_eq!(path.server(), Path::new("/home/mud/lib/x.o.o"));
    }

    #[test]
    fn a_directory_destination_keeps_its_origin_and_safe_name() {
        let root = LibRoot::new("/home/mud/lib");
        let source = root.resolve("/room/report.txt", "/").unwrap();
        for (directory, expected, name) in [
            (
                LpcPath::from("/archive"),
                LpcPath::from("/archive/report.txt"),
                "/archive/report.txt",
            ),
            (
                LpcPath::new_server("/home/mud/lib/archive"),
                LpcPath::new_server("/home/mud/lib/archive/report.txt"),
                "/archive/report.txt",
            ),
            (
                LpcPath::new_server("/private/archive"),
                LpcPath::new_server("/private/archive/report.txt"),
                "<outside mudlib>",
            ),
        ] {
            let target = source.in_directory(&root.source(&directory)).unwrap();
            assert_eq!(target.input(), &expected);
            assert_eq!(target.server(), root.source(&expected).server());
            assert_eq!(target.name().as_str(), name);
            assert_eq!(format!("{target:?}"), name);
        }
    }

    #[test]
    fn source_and_inheritance_keep_their_existing_directory_rules() {
        let root = LibRoot::new("/home/mud/lib");
        let host = LpcPath::new_server("/home/mud/lib/room/child.c");
        let game = LpcPath::from("/room/child.c");
        assert_eq!(root.source_cwd(&host), Path::new("/room"));
        assert_eq!(root.source_cwd(&game), Path::new("/room"));
        assert_eq!(root.inheritance_cwd(&host), Path::new("/room/child.c"));
        assert_eq!(root.inheritance_cwd(&game), Path::new("/room"));
    }

    #[test]
    fn identity_inputs_do_not_gain_file_operation_normalization() {
        let root = LibRoot::new("/home/mud/lib");
        assert_eq!(LpcPath::from("/../secret"), LpcPath::from("/secret"));
        assert!(root.resolve("/../secret", "/").is_err());
        assert_eq!(
            LpcPath::in_game("raw/../object".into()).to_string(),
            "raw/../object"
        );
        let collision = LpcPath::from("/home/mud/lib/object.c");
        assert_eq!(
            root.source_name(&collision).as_str(),
            "/home/mud/lib/object.c"
        );
        assert_eq!(root.object_name(&collision).as_str(), "/object.c");
    }

    #[test]
    fn an_unconfigured_root_keeps_in_game_names_absolute_without_duplicate_slashes() {
        for root in ["", "/"] {
            let root = LibRoot::new(root);
            let path = root.in_game("/secure/simul_efuns", "/");
            assert_eq!(path.to_string(), "/secure/simul_efuns");
            assert_eq!(
                root.source(&path).server(),
                Path::new("/secure/simul_efuns")
            );
        }
    }
}
