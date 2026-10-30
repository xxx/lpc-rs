use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_mapping::LpcMapping};

/// Read process resource usage into a fresh mapping, or an empty mapping when unavailable.
pub fn rusage<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    context.return_mapping(snapshot());
    Ok(())
}

#[cfg(all(
    unix,
    not(any(
        target_os = "redox",
        target_os = "fuchsia",
        target_os = "solaris",
        target_os = "haiku"
    ))
))]
use backend::snapshot;

#[cfg(not(all(
    unix,
    not(any(
        target_os = "redox",
        target_os = "fuchsia",
        target_os = "solaris",
        target_os = "haiku"
    ))
)))]
fn snapshot() -> LpcMapping {
    Default::default()
}

#[cfg(all(
    unix,
    not(any(
        target_os = "redox",
        target_os = "fuchsia",
        target_os = "solaris",
        target_os = "haiku"
    ))
))]
mod backend {
    use lpc_rs_core::LpcIntInner;
    use nix::sys::{
        resource::{Usage, UsageWho, getrusage},
        time::TimeVal,
    };

    use super::LpcMapping;
    use crate::interpreter::lpc_ref::LpcRef;

    pub(super) fn snapshot() -> LpcMapping {
        usage_mapping(getrusage(UsageWho::RUSAGE_SELF))
    }

    fn cpu_milliseconds(time: TimeVal) -> i128 {
        i128::from(time.tv_sec()) * 1000 + i128::from(time.tv_usec()) / 1000
    }

    fn resident_kib(raw: i128, in_bytes: bool) -> i128 {
        if in_bytes { raw / 1024 } else { raw }
    }

    fn usage_mapping(result: nix::Result<Usage>) -> LpcMapping {
        let Ok(usage) = result else {
            return LpcMapping::default();
        };
        let maxrss = resident_kib(i128::from(usage.max_rss()), cfg!(target_vendor = "apple"));
        let fields = [
            ("utime", cpu_milliseconds(usage.user_time())),
            ("stime", cpu_milliseconds(usage.system_time())),
            ("maxrss", maxrss),
            ("ixrss", i128::from(usage.shared_integral())),
            ("idrss", i128::from(usage.unshared_data_integral())),
            ("isrss", i128::from(usage.unshared_stack_integral())),
            ("minflt", i128::from(usage.minor_page_faults())),
            ("majflt", i128::from(usage.major_page_faults())),
            ("nswap", i128::from(usage.full_swaps())),
            ("inblock", i128::from(usage.block_reads())),
            ("oublock", i128::from(usage.block_writes())),
            ("msgsnd", i128::from(usage.ipc_sends())),
            ("msgrcv", i128::from(usage.ipc_receives())),
            ("nsignals", i128::from(usage.signals())),
            ("nvcsw", i128::from(usage.voluntary_context_switches())),
            ("nivcsw", i128::from(usage.involuntary_context_switches())),
        ];
        LpcMapping::new(
            fields
                .into_iter()
                .map(|(key, value)| {
                    let value = value
                        .clamp(i128::from(LpcIntInner::MIN), i128::from(LpcIntInner::MAX))
                        as LpcIntInner;
                    (LpcRef::from(key), LpcRef::from(value))
                })
                .collect(),
        )
    }

    #[cfg(test)]
    mod tests {
        use nix::errno::Errno;

        use super::*;
        use crate::test_support::run_prog;

        #[test]
        fn converts_all_resource_fields_with_cpu_milliseconds_and_resident_kib() {
            let mut usage = getrusage(UsageWho::RUSAGE_SELF).unwrap();
            let raw = usage.as_mut();
            raw.ru_utime.tv_sec = 1;
            raw.ru_utime.tv_usec = 123_999;
            raw.ru_stime.tv_sec = 2;
            raw.ru_stime.tv_usec = 456_999;
            raw.ru_maxrss = if cfg!(target_vendor = "apple") {
                5120
            } else {
                5
            };
            raw.ru_ixrss = 6;
            raw.ru_idrss = 7;
            raw.ru_isrss = 8;
            raw.ru_minflt = 9;
            raw.ru_majflt = 10;
            raw.ru_nswap = 11;
            raw.ru_inblock = 12;
            raw.ru_oublock = 13;
            raw.ru_msgsnd = 14;
            raw.ru_msgrcv = 15;
            raw.ru_nsignals = 16;
            raw.ru_nvcsw = 17;
            raw.ru_nivcsw = 18;
            let mapping = usage_mapping(Ok(usage));
            let actual: Vec<_> = mapping
                .iter()
                .map(|(key, value)| (key.to_string(), value.clone()))
                .collect();
            let expected = [
                ("utime", 1123),
                ("stime", 2456),
                ("maxrss", 5),
                ("ixrss", 6),
                ("idrss", 7),
                ("isrss", 8),
                ("minflt", 9),
                ("majflt", 10),
                ("nswap", 11),
                ("inblock", 12),
                ("oublock", 13),
                ("msgsnd", 14),
                ("msgrcv", 15),
                ("nsignals", 16),
                ("nvcsw", 17),
                ("nivcsw", 18),
            ]
            .map(|(key, value)| (key.to_owned(), LpcRef::from(value)));
            assert_eq!(actual, expected);
        }

        #[test]
        fn discards_cpu_fractions_smaller_than_one_millisecond() {
            assert_eq!(
                [1, 999, 1000, 1999].map(|us| cpu_milliseconds(TimeVal::new(0, us))),
                [0, 0, 1, 1]
            );
        }

        #[cfg(target_pointer_width = "64")]
        #[test]
        fn saturates_cpu_time_instead_of_wrapping() {
            let mut usage = getrusage(UsageWho::RUSAGE_SELF).unwrap();
            usage.as_mut().ru_utime.tv_sec = i64::MAX;
            usage.as_mut().ru_utime.tv_usec = 999_999;
            let mapping = usage_mapping(Ok(usage));
            assert_eq!(
                mapping.get(&LpcRef::from("utime")),
                Some(&LpcRef::from(i64::MAX))
            );
        }

        #[test]
        fn resident_memory_units_preserve_kib_and_truncate_bytes() {
            for (raw, kib) in [(0, 0), (1023, 0), (1024, 1), (2047, 1), (5120, 5)] {
                assert_eq!(resident_kib(raw, false), raw);
                assert_eq!(resident_kib(raw, true), kib);
            }
        }

        #[test]
        fn failed_sampling_returns_an_empty_mapping() {
            assert!(usage_mapping(Err(Errno::EIO)).is_empty());
        }

        #[tokio::test]
        async fn efun_reading_is_bracketed_by_real_process_samples() {
            let before = snapshot();
            assert_eq!(before.len(), 16);
            let task = run_prog("mapping create() { return rusage(); }").await;
            let result = task.result().unwrap();
            let actual = result
                .with_mapping(task.context.txn(), Clone::clone)
                .unwrap();
            let after = snapshot();
            assert_eq!(actual.len(), 16);
            for (key, low) in before.iter() {
                let Some(LpcRef::Int(value)) = actual.get(key) else {
                    panic!("missing integer {key}: {actual:?}");
                };
                let LpcRef::Int(low) = low else {
                    panic!("expected an integer")
                };
                let Some(LpcRef::Int(high)) = after.get(key) else {
                    panic!("expected an integer")
                };
                assert!(
                    (low.0..=high.0).contains(&value.0),
                    "{key}: {low} <= {value} <= {high}"
                );
            }
        }

        #[cfg(target_os = "linux")]
        #[test]
        fn includes_cpu_consumed_by_another_thread() {
            let before = getrusage(UsageWho::RUSAGE_SELF).unwrap();
            std::thread::spawn(|| {
                let start = getrusage(UsageWho::RUSAGE_THREAD).unwrap();
                let start = start.user_time() + start.system_time();
                loop {
                    let mut work = 1_u64;
                    for _ in 0..10_000 {
                        work = std::hint::black_box(
                            work.wrapping_mul(6364136223846793005).wrapping_add(1),
                        );
                    }
                    let now = getrusage(UsageWho::RUSAGE_THREAD).unwrap();
                    let elapsed = now.user_time() + now.system_time() - start;
                    // Separate user/system counters can lose one millisecond to truncation.
                    if elapsed >= TimeVal::new(0, 11_000) {
                        break;
                    }
                }
            })
            .join()
            .unwrap();
            let after = snapshot();
            let LpcRef::Int(user) = after.get(&LpcRef::from("utime")).unwrap() else {
                panic!("expected CPU time")
            };
            let LpcRef::Int(system) = after.get(&LpcRef::from("stime")).unwrap() else {
                panic!("expected CPU time")
            };
            let start =
                cpu_milliseconds(before.user_time()) + cpu_milliseconds(before.system_time());
            assert!(i128::from(user.0) + i128::from(system.0) >= start + 10);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::lpc_ref::LpcRef;
    use crate::{
        interpreter::vm::Vm,
        test_support::{run_prog, test_config},
    };

    #[tokio::test]
    async fn bare_efun_pointers_return_independent_mappings() {
        let task = run_prog(
            r#"
            int create() {
                function query = rusage;
                mapping first = query();
                first["fixture_marker"] = 1;
                mapping second = query();
                return mappingp(second) && !has_key(second, "fixture_marker")
                    && (sizeof(second) == 0 || sizeof(second) == 16);
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn rejects_arguments_to_direct_calls() {
        let error = Vm::new(test_config())
            .initialize_process_from_code("/rusage.c", "mapping create() { return rusage(1); }")
            .await
            .expect_err("a direct rusage call requires no arguments");
        assert!(
            error.to_string().contains("incorrect argument count"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn efun_pointers_evaluate_and_drop_unused_arguments() {
        let task = run_prog(
            r#"
            int create() {
                function query = rusage;
                int evaluated;
                mapping result = query(++evaluated);
                return evaluated == 1 && mappingp(result)
                    && (sizeof(result) == 0 || sizeof(result) == 16);
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
