//! `crypt(str [, salt])`: Unix `crypt(3)` password hashing, every family
//! behind one efun so a stored hash of any age still verifies.

use lpc_rs_errors::Result;
use pwhash::{
    HashSetup,
    bcrypt::{self, BcryptSetup, BcryptVariant},
    md5_crypt, sha256_crypt, sha512_crypt, unix,
};

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `crypt(str [, salt])`: hash `str` in the family the salt names, or a
/// fresh SHA-512 hash when the salt is absent, an int or `""`.
pub fn crypt<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(password) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "crypt: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let salt = if context.arg_count() < 2 {
        None
    } else {
        match context.arg(1) {
            LpcRef::Int(_) => None,
            other => match other.as_str() {
                Some("") => None,
                Some(salt) => Some(salt),
                None => {
                    return Err(context.runtime_error(format!(
                        "crypt: {} is not a string or int",
                        other.type_name()
                    )));
                }
            },
        }
    };
    let hashed = match salt {
        None => sha512_crypt::hash(password).map_err(|e| format!("crypt: {e}")),
        Some(salt) => {
            hash_with_salt(password, salt).map_err(|e| format!("crypt: invalid salt {salt:?}: {e}"))
        }
    };
    match hashed {
        Ok(hash) => {
            context.return_efun_result(LpcRef::from(hash));
            Ok(())
        }
        Err(message) => Err(context.runtime_error(message)),
    }
}

/// A bare family prefix (`$6$`, `$6$rounds=N$`, `$2b$`, `$2b$NN$`, ...)
/// gets a fresh salt; anything else is a salt or stored hash, dispatched
/// on its prefix like `crypt(3)`.
fn hash_with_salt(password: &str, salt: &str) -> pwhash::Result<String> {
    if salt == "$1$" {
        #[expect(deprecated, reason = "the lib asked for this family by name")]
        return md5_crypt::hash(password);
    }
    if let Some(rounds) = salt.strip_prefix("$5$").and_then(bare_rounds) {
        #[expect(deprecated, reason = "the lib asked for this family by name")]
        return sha256_crypt::hash_with(HashSetup { salt: None, rounds }, password);
    }
    if let Some(rounds) = salt.strip_prefix("$6$").and_then(bare_rounds) {
        return sha512_crypt::hash_with(HashSetup { salt: None, rounds }, password);
    }
    if let Some(cost) = salt.strip_prefix("$2b$").and_then(bare_cost) {
        let setup = BcryptSetup {
            salt: None,
            cost,
            variant: Some(BcryptVariant::V2b),
        };
        return bcrypt::hash_with(setup, password);
    }
    unix::crypt(password, salt)
}

/// `""` is `Some(None)` and `rounds=N$` is `Some(Some(N))`; a salt follows otherwise.
fn bare_rounds(rest: &str) -> Option<Option<u32>> {
    if rest.is_empty() {
        return Some(None);
    }
    let n = rest.strip_prefix("rounds=")?.strip_suffix('$')?;
    n.parse().ok().map(Some)
}

/// `""` is `Some(None)` and `NN$` is `Some(Some(NN))`; a salt follows otherwise.
fn bare_cost(rest: &str) -> Option<Option<u32>> {
    if rest.is_empty() {
        return Some(None);
    }
    rest.strip_suffix('$')?.parse().ok().map(Some)
}

#[cfg(test)]
mod tests {
    use crate::test_support::{strings_of, try_run_prog};

    /// `crypt("password", "ab")` from every `crypt(3)`.
    const DES: &str = "abJnggxhB/yWI";
    /// `openssl passwd -1 -salt saltsalt password`.
    const MD5: &str = "$1$saltsalt$qjXMvbEw8oaL.CzflDtaK/";
    /// `openssl passwd -5 -salt saltsalt password`.
    const SHA256: &str = "$5$saltsalt$gOjOtoMpVhru2uyjeJSEc/JaLQWOXMNmlOnj6T4AtC.";
    /// `openssl passwd -6 -salt saltsalt password`.
    const SHA512: &str = "$6$saltsalt$qFmFH.bQmmtXzyBY0s9v7Oicd2z4XSIecDzlB5KiA2/jctKu9YterLp8wwnSq.qc.eoxqOmSuNp2xS0ktL3nh/";

    async fn crypts(args: &[&str]) -> Vec<String> {
        let calls: Vec<String> = args.iter().map(|a| format!("crypt({a})")).collect();
        let code = format!("mixed create() {{ return ({{ {} }}); }}", calls.join(", "));
        strings_of(&code).await
    }

    async fn error_of(call: &str) -> String {
        let code = format!("mixed create() {{ return {call}; }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn a_two_character_salt_is_classic_des() {
        assert_eq!(crypts(&[r#""password", "ab""#]).await, [DES]);
    }

    #[tokio::test]
    async fn a_stored_des_hash_is_its_own_salt() {
        let stored = format!(r#""password", "{DES}""#);
        assert_eq!(crypts(&[&stored]).await, [DES]);
    }

    #[tokio::test]
    async fn a_wrong_password_does_not_reproduce_the_stored_hash() {
        let stored = format!(r#""nope", "{DES}""#);
        assert_ne!(crypts(&[&stored]).await, [DES]);
    }

    #[tokio::test]
    async fn the_salt_prefix_selects_the_glibc_family() {
        let md5 = r#""password", "$1$saltsalt$""#;
        let sha256 = r#""password", "$5$saltsalt$""#;
        let sha512 = r#""password", "$6$saltsalt$""#;
        assert_eq!(crypts(&[md5, sha256, sha512]).await, [MD5, SHA256, SHA512]);
    }

    #[tokio::test]
    async fn a_stored_glibc_hash_is_its_own_salt() {
        let stored = format!(r#""password", "{SHA512}""#);
        assert_eq!(crypts(&[&stored]).await, [SHA512]);
    }

    #[tokio::test]
    async fn a_bcrypt_hash_round_trips_and_rejects_the_wrong_password() {
        let fresh = crypts(&[r#""password", "$2b$""#]).await.remove(0);
        assert!(fresh.starts_with("$2b$"), "{fresh}");
        let right = format!(r#""password", "{fresh}""#);
        let wrong = format!(r#""nope", "{fresh}""#);
        let results = crypts(&[&right, &wrong]).await;
        assert_eq!(results[0], fresh);
        assert_ne!(results[1], fresh);
    }

    #[tokio::test]
    async fn without_a_salt_the_hash_is_fresh_sha512() {
        let results = crypts(&[r#""password""#, r#""password", 0"#, r#""password", """#]).await;
        for hash in &results {
            assert!(hash.starts_with("$6$"), "{hash}");
            // `$6$` + 16 salt characters + `$` + 86 hash characters.
            assert_eq!(hash.len(), 106, "{hash}");
        }
        assert_ne!(results[0], results[1], "salts are random");
    }

    #[tokio::test]
    async fn a_fresh_hash_verifies_as_its_own_salt() {
        let fresh = crypts(&[r#""password""#]).await.remove(0);
        let again = format!(r#""password", "{fresh}""#);
        assert_eq!(crypts(&[&again]).await, [fresh]);
    }

    #[tokio::test]
    async fn a_bare_family_prefix_gets_a_fresh_salt_of_that_family() {
        let results = crypts(&[
            r#""password", "$1$""#,
            r#""password", "$5$""#,
            r#""password", "$6$rounds=10000$""#,
            r#""password", "$2b$12$""#,
        ])
        .await;
        assert!(results[0].starts_with("$1$"), "{}", results[0]);
        assert!(results[1].starts_with("$5$"), "{}", results[1]);
        assert!(results[2].starts_with("$6$rounds=10000$"), "{}", results[2]);
        assert!(results[3].starts_with("$2b$12$"), "{}", results[3]);
        for (arg, hash) in ["password"; 4].iter().zip(&results) {
            let again = format!(r#""{arg}", "{hash}""#);
            assert_eq!(crypts(&[&again]).await, std::slice::from_ref(hash));
        }
    }

    #[tokio::test]
    async fn an_unknown_family_is_an_error() {
        let err = error_of(r#"crypt("password", "$7$bogus$")"#).await;
        assert!(err.contains("crypt: invalid salt \"$7$bogus$\""), "{err}");
    }

    #[tokio::test]
    async fn a_one_character_salt_is_an_error() {
        let err = error_of(r#"crypt("password", "a")"#).await;
        assert!(err.contains("crypt: invalid salt \"a\""), "{err}");
    }

    #[tokio::test]
    async fn a_non_string_password_is_an_error() {
        let code = r#"mixed create() { mixed n = 42; return crypt(n, "ab"); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("crypt: int is not a string"), "{err}");
    }

    #[tokio::test]
    async fn a_salt_of_another_type_is_an_error() {
        let code = r#"mixed create() { mixed a = ({ "ab" }); return crypt("password", a); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("crypt: array is not a string or int"), "{err}");
    }
}
