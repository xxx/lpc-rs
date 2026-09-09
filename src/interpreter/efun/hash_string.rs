use lpc_rs_errors::Result;
use md5::Md5;
use sha1::Sha1;
use sha2::{Digest, Sha256, Sha384, Sha512};

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// `hash_string(str, algorithm)`: an unsalted, lowercase hexadecimal digest.
pub fn hash_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let input = match context.arg(0) {
        LpcRef::Int(_) => {
            context.return_efun_result(NULL);
            return Ok(());
        }
        LpcRef::String(s) => s.to_str(),
        other => {
            return Err(context.runtime_error(format!(
                "hash_string: {} is not a string or int",
                other.type_name()
            )));
        }
    };
    let LpcRef::Int(algorithm) = context.arg(1) else {
        return Err(context.runtime_error("hash_string: algorithm is not an int"));
    };
    let input = input
        .split_once('\0')
        .map_or(input, |(prefix, _)| prefix)
        .as_bytes();
    let digest = match algorithm.0 {
        0 => format!("{:x}", Md5::digest(input)),
        1 => format!("{:x}", Sha1::digest(input)),
        2 => format!("{:x}", Sha256::digest(input)),
        3 => format!("{:x}", Sha384::digest(input)),
        4 => format!("{:x}", Sha512::digest(input)),
        n => {
            return Err(context.runtime_error(format!(
                "hash_string: invalid algorithm {n}; expected 0 (MD5), 1 (SHA1), 2 (SHA256), 3 (SHA384), or 4 (SHA512)"
            )));
        }
    };
    context.return_efun_result(LpcRef::from(digest));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, strings_of, try_run_prog},
    };

    #[tokio::test]
    async fn matches_known_digests_for_every_algorithm() {
        let digests = strings_of(indoc! { r#"
            mixed create() {
                string s = "The quick brown fox jumped over the lazy dog.";
                return ({ hash_string(s, 0), hash_string(s, 1), hash_string(s, 2),
                    hash_string(s, 3), hash_string(s, 4) });
            }
        "# })
        .await;
        assert_eq!(
            digests,
            [
                "5c6ffbdd40d9556b73a21e63c3e0e904",
                "c0854fb9fb03c41cce3802cb0d220529e6eef94e",
                "68b1282b91de2c054c36629cb8dd447f12f096d3e3c587978dc2248444633483",
                "b7273c05ad141ccb6696b3659e57137c453b6d64690fa7d5cf96368df4a7138703a8c6ead31727b487b3628746510391",
                "0a8c150176c2ba391d7f1670ef4955cd99d3c3ec8cf06198cec30d436f2ac0c9b64229b5a54bdbd5563160503ce992a74be528761da9d0c48b7c74627302eb25",
            ]
        );
    }

    #[tokio::test]
    async fn empty_strings_have_the_standard_empty_digests() {
        let digests = strings_of(indoc! { r#"
            mixed create() {
                return ({ hash_string("", 0), hash_string("", 1), hash_string("", 2),
                    hash_string("", 3), hash_string("", 4) });
            }
        "# })
        .await;
        assert_eq!(
            digests,
            [
                "d41d8cd98f00b204e9800998ecf8427e",
                "da39a3ee5e6b4b0d3255bfef95601890afd80709",
                "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
                "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b",
                "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
            ]
        );
    }

    #[tokio::test]
    async fn hashes_utf8_bytes() {
        let digests = strings_of(
            r#"
            mixed create() { return ({ hash_string("pässwörd 🐉", 2) }); }
        "#,
        )
        .await;
        assert_eq!(
            digests,
            ["d453adf217b02be6db88042a62e893f785c3678cc669e1fafa5bbeafd2ec7170"]
        );
    }

    #[tokio::test]
    async fn stops_at_the_first_nul() {
        let digests = strings_of(indoc! { r#"
            mixed create() {
                return ({ hash_string("foobar" + sprintf("%c", 0) + "ignored", 1) });
            }
        "# })
        .await;
        assert_eq!(digests, ["8843d7f92416211de9ebb963ff4ce28125932878"]);
    }

    #[tokio::test]
    async fn integers_return_zero() {
        let task = run_prog(indoc! { r#"
            int *create() {
                return ({ hash_string(0, 1) == 0, hash_string(666, 1) == 0,
                    hash_string(666, 666) == 0 });
            }
        "# })
        .await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[1, 1, 1][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn invalid_algorithm_ids_are_errors() {
        for algorithm in [-1, 5, 666] {
            let code =
                format!(r#"mixed create() {{ return hash_string("foobar", {algorithm}); }}"#);
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                error.contains(&format!("hash_string: invalid algorithm {algorithm};")),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn a_non_integer_algorithm_is_an_error() {
        let error = try_run_prog(
            r#"
            mixed create() { mixed algorithm = "sha512"; return hash_string("foobar", algorithm); }
        "#,
        )
        .await
        .unwrap_err()
        .to_string();
        assert!(
            error.contains("hash_string: algorithm is not an int"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn an_unsupported_input_type_is_an_error() {
        for input in ["1.5", "({ 1 })", "this_object()"] {
            let code = format!(
                "mixed create() {{ mixed input = {input}; return hash_string(input, 1); }}"
            );
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(error.contains("is not a string or int"), "{error}");
        }
    }

    #[tokio::test]
    async fn an_old_sha512_password_can_be_verified_and_migrated_to_crypt() {
        let task = run_prog(indoc! { r#"
            int *create() {
                string stored = "b109f3bbbc244eb82441917ed06d618b9008dd09b3befd1b5e07394c706a8bb980b1d7785e5976ec049b46df5f1326af5a2ea6d103fd07c95385ffab0cacbc86";
                int wrong_rejected = hash_string("wrong", 4) != stored;
                if (hash_string("password", 4) != stored) { return ({ 0, 0 }); }
                stored = crypt("password", 0);
                return ({ wrong_rejected, crypt("password", stored) == stored });
            }
        "# }).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[1, 1][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn works_through_an_efun_pointer() {
        let task = run_prog(indoc! { r#"
            int create() {
                function hash = &hash_string();
                return hash("foobar", 1) == "8843d7f92416211de9ebb963ff4ce28125932878";
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }
}
