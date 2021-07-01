use path_absolutize::Absolutize;
use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

/// Convert an in-game path, relative or absolute, to a canonical, absolute *on-server* path.
/// This function is used for resolving included files.
///
/// # Arguments
/// `path` - An in-game path.
/// `cwd` - The current working directory, needed to resolve relative paths.
/// `root_dir` - The game root directory.
pub fn canonicalize_server_path<T, U, V>(path: T, cwd: U, root_dir: V) -> PathBuf
where
    T: AsRef<Path>,
    U: AsRef<Path>,
    V: Into<OsString>,
{
    let path_ref = path.as_ref().as_os_str();
    let sep = String::from(std::path::MAIN_SEPARATOR);
    let os_sep = OsString::from(&sep);
    let mut root_string = root_dir.into();

    // turn relative paths into absolute
    if !path_ref.to_string_lossy().starts_with(&sep) {
        root_string.push(&os_sep);
        root_string.push(cwd.as_ref().as_os_str());
    }

    root_string.push(&os_sep);
    root_string.push(&path_ref);

    Path::new(
        &root_string
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
/// `lib_dir` - The game root directory.
pub fn canonicalize_in_game_path<T, U, V>(path: T, cwd: U, lib_dir: V) -> PathBuf
where
    T: AsRef<Path>,
    U: AsRef<Path>,
    V: AsRef<OsStr>,
{
    let canon = canonicalize_server_path(path, cwd, lib_dir.as_ref());
    let buf = canon.as_os_str();
    let root_len = lib_dir.as_ref().len();

    // Strip off the root_dir prefix, then clean up the rest.
    PathBuf::from(
        &buf.to_string_lossy()
            .chars()
            .skip(root_len)
            .collect::<String>()
            .replace("//", "/")
            .replace("/./", "/"),
    )
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
}
