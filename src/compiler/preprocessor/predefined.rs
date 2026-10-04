use lpc_rs_errors::{Result, lpc_bug, source_map::DiagnosticSources, span::Span};

use crate::{
    compile_time_config::DRIVER_VERSION,
    compiler::lexer::{
        Token,
        logos_token::{IntToken, StringToken},
    },
};

/// Reserved macros whose values are supplied by the compiler at the invocation site.
pub(super) enum Predefined {
    File,
    Dir,
    Version,
    Port,
}

impl Predefined {
    pub(super) fn from_name(name: &str) -> Option<Self> {
        match name {
            "__FILE__" => Some(Self::File),
            "__DIR__" => Some(Self::Dir),
            "__VERSION__" => Some(Self::Version),
            "__PORT__" => Some(Self::Port),
            _ => None,
        }
    }

    pub(super) fn expand(
        self,
        span: Span,
        sources: &DiagnosticSources,
        port: u16,
    ) -> Result<Token> {
        let value = match self {
            Self::Port => return Ok(Token::IntLiteral(IntToken(span, port.into()))),
            Self::Version => DRIVER_VERSION.to_owned(),
            Self::File | Self::Dir => {
                let sources = sources.read();
                let file = sources.get(span.file_id()).map_err(|_| {
                    lpc_bug!(Some(span), "missing source for predefined macro expansion")
                })?;
                let name = file.name();
                match self {
                    Self::Dir => name.rfind('/').map_or(name.as_str(), |end| &name[..=end]),
                    _ => name.as_str(),
                }
                .to_owned()
            }
        };
        Ok(Token::StringLiteral(StringToken(span, value)))
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_errors::span::HasSpan;
    use lpc_rs_utils::config::{Config, ConfigBuilder};

    use super::*;
    use crate::{
        compiler::{compilation_context::CompilationContextBuilder, preprocessor::Preprocessor},
        test_support::{lib_holding, temp_lib_config},
    };

    fn preprocessor(config: Config, sources: DiagnosticSources) -> Preprocessor {
        let context = CompilationContextBuilder::default()
            .config(config)
            .diagnostic_sources(sources)
            .build()
            .unwrap();
        Preprocessor::new(context)
    }

    fn fixture() -> Preprocessor {
        preprocessor(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .build()
                .unwrap(),
            DiagnosticSources::scoped(),
        )
    }

    fn strings(tokens: Vec<Token>) -> Vec<String> {
        tokens
            .into_iter()
            .map(|token| match token {
                Token::StringLiteral(value) => value.1,
                other => panic!("expected a string literal, got {other:?}"),
            })
            .collect()
    }

    #[tokio::test]
    async fn default_port_expands_to_an_integer_literal() {
        let tokens = fixture().scan("/root.c", "__PORT__").await.unwrap();
        assert!(matches!(&tokens[..], [Token::IntLiteral(value)] if value.1 == 24960));
    }

    #[tokio::test]
    async fn disabled_plaintext_telnet_keeps_its_configured_port_instead_of_the_tls_port() {
        use lpc_rs_utils::tls::{CertificateFiles, TlsConfig};

        let outside = lib_holding("configured-port-tls-files", &[]);
        let config = ConfigBuilder::default()
            .lib_dir("tests/fixtures/code")
            .port(33100)
            .telnet_enabled(false)
            .tls(TlsConfig {
                port: 44300,
                files: CertificateFiles {
                    certificate: outside.join("identity.pem"),
                    private_key: outside.join("identity.pem"),
                },
            })
            .build()
            .unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan("/root.c", "__PORT__")
            .await
            .unwrap();
        assert!(matches!(&tokens[..], [Token::IntLiteral(value)] if value.1 == 33100));
    }

    #[tokio::test]
    async fn configured_port_values_are_isolated_between_preprocessors() {
        for port in [0, 4100, u16::MAX] {
            let config = ConfigBuilder::default().port(port).build().unwrap();
            let tokens = preprocessor(config, DiagnosticSources::scoped())
                .scan("/root.c", "__PORT__")
                .await
                .unwrap();
            assert!(
                matches!(&tokens[..], [Token::IntLiteral(value)] if value.1 == u64::from(port))
            );
        }
    }

    #[tokio::test]
    async fn configured_port_is_available_in_auto_includes_and_headers() {
        let root = lib_holding(
            "configured-port-headers",
            &[
                (
                    "include/global.h",
                    "__PORT__\n#define PORT_ALIAS __PORT__\n",
                ),
                ("include/extra.h", "__PORT__\n"),
            ],
        );
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .auto_include_file("/include/global.h")
            .port(33100)
            .build()
            .unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan(
                "/root.c",
                "#include \"/include/extra.h\"\nPORT_ALIAS __PORT__",
            )
            .await
            .unwrap();
        assert_eq!(tokens.len(), 4);
        assert!(
            tokens
                .iter()
                .all(|token| matches!(token, Token::IntLiteral(value) if value.1 == 33100))
        );
    }

    #[tokio::test]
    async fn port_aliases_arguments_and_stringification_preserve_numeric_spelling() {
        let config = ConfigBuilder::default().port(33100).build().unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan(
                "/root.c",
                indoc! {r#"
                #define PORT_ALIAS __PORT__
                #define ID(x) x
                #define STR(x) #x
                #define STRINGIZE(x) STR(x)
                #define SHADOW(__PORT__) __PORT__
                ID(PORT_ALIAS) STR(__PORT__) STRINGIZE(PORT_ALIAS) SHADOW(7)
            "#},
            )
            .await
            .unwrap();
        assert!(
            matches!(&tokens[..], [Token::IntLiteral(port), Token::StringLiteral(raw), Token::StringLiteral(expanded), Token::IntLiteral(argument)]
            if port.1 == 33100 && raw.1 == "__PORT__" && expanded.1 == "33100" && argument.1 == 7)
        );
    }

    #[tokio::test]
    async fn configured_port_supports_arithmetic_aliases_and_elif_conditions() {
        let config = ConfigBuilder::default().port(4100).build().unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan(
                "/root.c",
                indoc! {r#"
                #define PORT_ALIAS __PORT__
                #if __PORT__ + 3 != 4103 || PORT_ALIAS != 4100
                #error incorrect configured port
                #endif
                #if !__PORT__ || !PORT_ALIAS
                #error a nonzero port is truthy
                #endif
                #if 0
                #error inactive branch
                #elif PORT_ALIAS * 2 == 8200 && (__PORT__ >> 1) == 2050
                "selected"
                #else
                #error incorrect port arithmetic
                #endif
            "#},
            )
            .await
            .unwrap();
        assert_eq!(strings(tokens), ["selected"]);
    }

    #[tokio::test]
    async fn zero_configured_port_is_defined_but_false_in_conditions() {
        let config = ConfigBuilder::default().port(0).build().unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan(
                "/root.c",
                indoc! {r#"
                #define PORT_ALIAS __PORT__
                #ifndef __PORT__
                #error missing port
                #endif
                #if defined(__PORT__) && !__PORT__ && !PORT_ALIAS && __PORT__ == 0
                "zero"
                #else
                #error zero port is not false
                #endif
                #if 0
                #elif __PORT__
                #error zero port is not false in elif
                #else
                "zero elif"
                #endif
            "#},
            )
            .await
            .unwrap();
        assert_eq!(strings(tokens), ["zero", "zero elif"]);
    }

    #[tokio::test]
    async fn direct_values_use_normalized_in_game_paths_and_native_version() {
        let mut pp = fixture();
        let tokens = pp
            .scan("/areas/old/../room.c", "__FILE__ __DIR__ __VERSION__")
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            ["/areas/room.c", "/areas/", DRIVER_VERSION]
        );
        let tokens = fixture().scan("/root.c", "__DIR__").await.unwrap();
        assert_eq!(strings(tokens), ["/"]);
        assert_eq!(
            DRIVER_VERSION,
            concat!("lpc-rs ", env!("CARGO_PKG_VERSION"))
        );
    }

    #[tokio::test]
    async fn includes_and_header_defined_macros_use_the_current_invocation_file() {
        let root = lib_holding(
            "predefined-includes",
            &[
                (
                    "inc/outer.h",
                    "__FILE__ __DIR__\n#include \"nested/inner.h\"\n__FILE__ __DIR__\n#define LOCATION __FILE__\n#define DIRECTORY() __DIR__\n",
                ),
                ("inc/nested/inner.h", "__FILE__ __DIR__\n"),
            ],
        );
        for sources in [DiagnosticSources::default(), DiagnosticSources::scoped()] {
            let mut pp = preprocessor(temp_lib_config(&root), sources);
            let tokens = pp
                .scan(
                    "/rooms/room.c",
                    indoc! {r#"
                __FILE__ __DIR__
                #include "/inc/outer.h"
                LOCATION DIRECTORY() __FILE__ __DIR__
            "#},
                )
                .await
                .unwrap();
            assert_eq!(
                strings(tokens),
                [
                    "/rooms/room.c",
                    "/rooms/",
                    "/inc/outer.h",
                    "/inc/",
                    "/inc/nested/inner.h",
                    "/inc/nested/",
                    "/inc/outer.h",
                    "/inc/",
                    "/rooms/room.c",
                    "/rooms/",
                    "/rooms/room.c",
                    "/rooms/"
                ]
            );
        }
    }

    #[tokio::test]
    async fn auto_include_values_end_at_the_header_boundary() {
        let root = lib_holding(
            "predefined-auto",
            &[(
                "include/global.h",
                "__FILE__ __DIR__\n#define GLOBAL_FILE __FILE__\n",
            )],
        );
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .auto_include_file("/include/global.h")
            .build()
            .unwrap();
        let tokens = preprocessor(config, DiagnosticSources::scoped())
            .scan("/root.c", "GLOBAL_FILE __DIR__")
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            ["/include/global.h", "/include/", "/root.c", "/"]
        );
    }

    #[tokio::test]
    async fn aliases_function_arguments_and_parameter_names_expand_normally() {
        let tokens = fixture()
            .scan(
                "/where/room.c",
                indoc! {r#"
            #define FILE_ALIAS __FILE__
            #define ID(x) x
            #define WRAP(x) ID(x)
            #define SHADOW(__FILE__) __FILE__
            #define BOTH(x) x __DIR__
            WRAP(FILE_ALIAS) BOTH(__VERSION__) SHADOW("argument")
        "#},
            )
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            ["/where/room.c", DRIVER_VERSION, "/where/", "argument"]
        );
    }

    #[tokio::test]
    async fn raw_stringification_keeps_names_and_wrapped_stringification_uses_values() {
        let tokens = fixture()
            .scan(
                "/where/room.c",
                indoc! {r#"
            #define STR(x) #x
            #define STRINGIZE(x) STR(x)
            #define FILE_ALIAS __FILE__
            STR(__FILE__) STR(__DIR__) STR(__VERSION__)
            STRINGIZE(FILE_ALIAS) STRINGIZE(__DIR__) STRINGIZE(__VERSION__)
        "#},
            )
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            [
                "__FILE__".to_owned(),
                "__DIR__".into(),
                "__VERSION__".into(),
                "\"/where/room.c\"".into(),
                "\"/where/\"".into(),
                format!("\"{DRIVER_VERSION}\"")
            ]
        );
    }

    #[tokio::test]
    async fn generated_paths_preserve_unicode_quotes_backslashes_and_control_characters() {
        let path = "/zone/雪\"\\tab\tline\n.c";
        let tokens = fixture()
            .scan(
                path,
                "#define STR(x) #x\n#define STRINGIZE(x) STR(x)\n__FILE__ STRINGIZE(__FILE__)",
            )
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            [path, "\"/zone/雪\\\"\\\\tab\\tline\\n.c\""]
        );
    }

    #[tokio::test]
    async fn literals_comments_and_inactive_source_do_not_expand_predefined_names() {
        let tokens = fixture()
            .scan(
                "/root.c",
                indoc! {r#"
            "__FILE__ __DIR__ __VERSION__ __PORT__"
            // __FILE__ __PORT__
            /* __VERSION__ */
            #if 0
            __DIR__
            #define __FILE__ "changed"
            #undef __VERSION__
            #define __PORT__ 7
            #undef __PORT__
            #endif
            __FILE__
        "#},
            )
            .await
            .unwrap();
        assert_eq!(
            strings(tokens),
            ["__FILE__ __DIR__ __VERSION__ __PORT__", "/root.c"]
        );
    }

    #[tokio::test]
    async fn all_defined_predicates_recognize_predefined_names() {
        let tokens = fixture()
            .scan(
                "/root.c",
                indoc! {r#"
            #ifdef __FILE__
            "file"
            #endif
            #ifndef __DIR__
            #error missing directory
            #endif
            #if defined(__FILE__) + defined(__DIR__) + defined(__VERSION__) + defined(__PORT__) == 4
            "all four"
            #endif
            #if not defined(__VERSION__)
            #error missing version
            #endif
            #if 0
                #elif defined(__VERSION__)
            "version"
            #endif
            #if defined(FLUFFOS) || defined(__SANE_EXPLODE_STRING__)
            #error unexpected driver feature flags
            #endif
        "#},
            )
            .await
            .unwrap();
        assert_eq!(strings(tokens), ["file", "all four", "version"]);
    }

    #[tokio::test]
    async fn string_predefines_follow_the_existing_conditional_expression_rules() {
        let tokens = fixture()
            .scan(
                "/root.c",
                indoc! {r#"
            #define PATH_ALIAS __FILE__
            #if PATH_ALIAS && __DIR__ && __VERSION__
            "truthy"
            #endif
            #if !__FILE__
            #error a string is truthy
            #endif
        "#},
            )
            .await
            .unwrap();
        assert_eq!(strings(tokens), ["truthy"]);
        let error = fixture()
            .scan("/root.c", "#if __FILE__ + 1\n#endif\n")
            .await
            .unwrap_err();
        assert_eq!(error.message(), "unable to resolve into an int: `__FILE__`");
    }

    #[tokio::test]
    async fn predefined_names_cannot_be_redefined_or_undefined() {
        for name in ["__FILE__", "__DIR__", "__VERSION__", "__PORT__"] {
            for (directive, action) in [
                (format!("#define {name} \"changed\""), "redefine"),
                (format!("#define {name}() \"changed\""), "redefine"),
                (format!("#undef {name}"), "undefine"),
            ] {
                let error = fixture()
                    .scan("/root.c", format!("{directive}\n"))
                    .await
                    .unwrap_err();
                assert_eq!(
                    error.message(),
                    format!("cannot {action} predefined macro `{name}`")
                );
            }
        }
    }

    #[tokio::test]
    async fn host_inputs_outside_the_mudlib_keep_the_opaque_source_label() {
        let root = lib_holding("predefined-root", &[]);
        let outside = lib_holding("predefined-outside", &[]);
        let mut pp = preprocessor(temp_lib_config(&root), DiagnosticSources::scoped());
        let tokens = pp
            .scan(
                LpcPath::new_server(outside.join("secret.c")),
                "__FILE__ __DIR__",
            )
            .await
            .unwrap();
        assert_eq!(strings(tokens), ["<outside mudlib>", "<outside mudlib>"]);
    }

    #[tokio::test]
    async fn predefined_tokens_keep_use_site_spans_and_warning_policy() {
        let sources = DiagnosticSources::scoped();
        let mut pp = preprocessor(ConfigBuilder::default().build().unwrap(), sources.clone());
        let tokens = pp.scan("/root.c", "#define FILE __FILE__\n#define ID(x) x\n#pragma no_warnings\nFILE ID(__DIR__) __VERSION__ __PORT__").await.unwrap();
        let spelling: Vec<_> = tokens
            .iter()
            .map(|token| {
                let span = token.span();
                assert!(span.warnings_suppressed());
                span.code_in(&sources.read()).unwrap()
            })
            .collect();
        assert_eq!(spelling, ["FILE", "__DIR__", "__VERSION__", "__PORT__"]);
    }

    #[tokio::test]
    async fn predefined_expansion_still_obeys_the_token_work_limit() {
        let mut code = "#define B0 __FILE__\n".to_owned();
        for i in 1..18 {
            code.push_str(&format!("#define B{i} B{} B{}\n", i - 1, i - 1));
        }
        code.push_str("B17\n");
        let error = fixture().scan("/root.c", code).await.unwrap_err();
        assert!(error.message().contains("too many tokens"), "{error:?}");
    }
}
