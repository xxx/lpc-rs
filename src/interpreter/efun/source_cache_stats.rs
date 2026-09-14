use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::{Result, source_map::SOURCE_MAP};

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
};

/// Return a process-wide snapshot of retained diagnostic source storage.
pub fn source_cache_stats<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let stats = SOURCE_MAP.read().stats();
    let stats = stats.map_err(|error| {
        context.runtime_error(format!("unable to read source cache stats: {error}"))
    })?;
    let fields = [
        ("file_versions", stats.file_versions),
        ("unique_files", stats.unique_files),
        ("source_bytes", stats.source_bytes),
        ("source_capacity_bytes", stats.source_capacity_bytes),
        ("filename_capacity_bytes", stats.filename_capacity_bytes),
        ("line_index_bytes", stats.line_index_bytes),
    ];
    context.return_mapping(LpcMapping::new(
        fields
            .into_iter()
            .map(|(key, value)| {
                let value = LpcIntInner::try_from(value).unwrap_or(LpcIntInner::MAX);
                (LpcRef::from(key), LpcRef::from(value))
            })
            .collect(),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::test_support::run_prog;

    #[tokio::test]
    async fn reports_storage_in_a_fresh_mapping() {
        let task = run_prog(indoc! { r#"
            int create() {
                mapping first = source_cache_stats();
                if (sizeof(first) != 6 || first["unique_files"] < 1
                    || first["file_versions"] < first["unique_files"]
                    || first["source_bytes"] <= 0
                    || first["source_capacity_bytes"] < first["source_bytes"]
                    || first["filename_capacity_bytes"] <= 0
                    || first["line_index_bytes"] <= 0) {
                    return 0;
                }
                int versions = first["file_versions"];
                first["file_versions"] = -1;
                mapping second = source_cache_stats();
                return second["file_versions"] >= versions;
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }
}
