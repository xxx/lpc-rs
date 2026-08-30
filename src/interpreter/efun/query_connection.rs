use indexmap::IndexMap;
use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{self, efun_context::EfunContext},
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    lpc_string::LpcString,
};

/// `query_connection`, an efun returning what the driver knows about an
/// object's connection as a mapping with a fixed key set; 0 without one.
pub async fn query_connection<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(connection) = efun::connection_of(context) else {
        return Ok(());
    };
    let snapshot = connection.snapshot();
    let charset = match &snapshot.charset {
        Some(charset) => LpcString::from(charset.as_str()).into(),
        None => LpcRef::from(0),
    };
    let entries: [(&str, LpcRef); 10] = [
        (
            "ip",
            LpcString::from(connection.address.ip().to_string()).into(),
        ),
        (
            "port",
            LpcRef::from(connection.address.port() as LpcIntInner),
        ),
        ("cols", LpcRef::from(snapshot.cols as LpcIntInner)),
        ("rows", LpcRef::from(snapshot.rows as LpcIntInner)),
        ("charset", charset),
        ("gmcp", LpcRef::from(snapshot.gmcp)),
        ("mxp", LpcRef::from(snapshot.mxp)),
        ("eor", LpcRef::from(snapshot.eor)),
        ("idle", LpcRef::from(connection.idle() as LpcIntInner)),
        ("overflowed", LpcRef::from(connection.is_overflowed())),
    ];
    let mapping: IndexMap<LpcRef, LpcRef> = entries
        .into_iter()
        .map(|(key, value)| (LpcString::from(key).into(), value))
        .collect();
    let cell = context
        .txn()
        .with(|t| t.mint_mapping(LpcMapping::new(mapping)));
    context.return_efun_result(LpcRef::Mapping(cell));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_telnet::Session;

    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, lpc_string::LpcString, task::Task,
            task::task_template::TaskTemplate, vm::Vm,
        },
        test_support::{connect, test_config},
    };

    const IAC: u8 = 255;
    const SB: u8 = 250;
    const SE: u8 = 240;
    const DO: u8 = 253;
    const NAWS: u8 = 31;
    const GMCP: u8 = 201;

    #[tokio::test]
    async fn a_connected_object_answers_every_key() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let connected = connect(&vm, &player).await;
        let mut session = Session::new();
        session.feed(&[IAC, DO, GMCP, IAC, SB, NAWS, 0, 100, 0, 40, IAC, SE]);
        connected.connection.refresh(&session);
        connected.connection.set_overflowed(true);

        let main = indoc! { r#"
            mapping m;
            void create() { m = query_connection(find_object("/player")); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        let LpcRef::Mapping(cell) = vm.global_state.committed_global(&main, 0u16) else {
            panic!("a mapping");
        };
        let m = vm.global_state.committed_mapping(cell.id).unwrap();
        let get = |k: &str| m.get(&LpcString::from(k).into()).cloned().unwrap();
        assert_eq!(get("ip").to_string(), "127.0.0.1");
        assert_eq!(get("port"), LpcRef::from(23123));
        assert_eq!(get("cols"), LpcRef::from(100));
        assert_eq!(get("rows"), LpcRef::from(40));
        assert_eq!(get("charset"), LpcRef::from(0));
        assert_eq!(get("gmcp"), LpcRef::from(1));
        assert_eq!(get("mxp"), LpcRef::from(0));
        assert_eq!(get("eor"), LpcRef::from(0));
        assert_eq!(get("idle"), LpcRef::from(0));
        assert_eq!(get("overflowed"), LpcRef::from(1));
        assert_eq!(m.len(), 10, "the key set is fixed");
    }

    #[tokio::test]
    async fn the_argument_defaults_to_this_player() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            mapping m;
            void create() { set_this_player(this_object()); m = query_connection(); }
        "# };
        let player = vm
            .create_process_from_code("/player.c", code)
            .await
            .unwrap();
        let _connected = connect(&vm, &player).await;
        Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player.clone()),
        )
        .await
        .unwrap();
        let LpcRef::Mapping(cell) = vm.global_state.committed_global(&player, 0u16) else {
            panic!("a mapping");
        };
        let m = vm.global_state.committed_mapping(cell.id).unwrap();
        assert_eq!(
            m.get(&LpcString::from("port").into()),
            Some(&LpcRef::from(23123))
        );
    }

    #[tokio::test]
    async fn an_object_without_a_connection_is_zero() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/npc.c", "").await.unwrap();
        let main = indoc! { r#"
            mixed m;
            void create() { m = query_connection(find_object("/npc")); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(0)
        );
    }
}
