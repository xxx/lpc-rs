use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{CommitProtocol, Committer, LiveSnapshot, Transaction, VarId, WorldRoot},
};

struct Attempt {
    live: LiveSnapshot,
    transaction: Transaction,
    base: usize,
    snapshot: [Option<i64>; 4],
    writes: [Option<Option<i64>>; 4],
    reads: [bool; 4],
}

/// Compare controlled transaction interleavings with a read-validation reference model.
pub fn stm(data: &[u8]) {
    let (sender, receiver) = flume::unbounded();
    let mut committer = Committer::new();
    let cells = std::array::from_fn::<_, 4, _>(|_| VarId::new());
    let mut attempts: [Option<Attempt>; 4] = std::array::from_fn(|_| None);
    let mut world = [None; 4];
    let mut last_written = [0; 4];
    let mut version = 0;

    for operation in data.as_chunks::<4>().0.iter().take(256) {
        let slot = usize::from(operation[1]) % attempts.len();
        let cell = usize::from(operation[2]) % cells.len();
        match operation[0] % 7 {
            0 => {
                let (reply, result) = flume::bounded(1);
                committer.process(CommitProtocol::Start { reply }, &sender);
                let live = result.recv().unwrap();
                let transaction = Transaction::new(live.inner.clone());
                attempts[slot] = Some(Attempt {
                    live,
                    transaction,
                    base: version,
                    snapshot: world,
                    writes: [None; 4],
                    reads: [false; 4],
                });
            }
            1 => {
                if let Some(attempt) = &mut attempts[slot] {
                    let expected = match attempt.writes[cell] {
                        Some(value) => value,
                        None => {
                            attempt.reads[cell] = true;
                            attempt.snapshot[cell]
                        }
                    };
                    assert_eq!(
                        attempt.transaction.read(cells[cell]),
                        expected.map(LpcRef::from)
                    );
                }
            }
            2 => {
                if let Some(attempt) = &mut attempts[slot] {
                    let value = i64::from(operation[3] as i8);
                    attempt.transaction.write(cells[cell], LpcRef::from(value));
                    attempt.writes[cell] = Some(Some(value));
                }
            }
            3 => {
                if let Some(attempt) = &mut attempts[slot] {
                    attempt.transaction.drop_var(cells[cell]);
                    attempt.writes[cell] = Some(None);
                }
            }
            4 => {
                if let Some(mut attempt) = attempts[slot].take() {
                    let expected =
                        (0..4).all(|i| !attempt.reads[i] || last_written[i] <= attempt.base);
                    let changeset = attempt.transaction.take_changeset();
                    let (reply, result) = flume::bounded(1);
                    committer.process(
                        CommitProtocol::Commit {
                            changeset,
                            releases_base: false,
                            reply,
                        },
                        &sender,
                    );
                    assert_eq!(result.recv().unwrap().is_ok(), expected);
                    if expected && attempt.writes.iter().any(Option::is_some) {
                        version += 1;
                        for i in 0..4 {
                            if let Some(value) = attempt.writes[i] {
                                world[i] = value;
                                last_written[i] = version;
                            }
                        }
                    }
                    drop(attempt.live);
                }
            }
            5 => {
                attempts[slot] = None;
            }
            _ => {
                let live = attempts.iter().flatten().count();
                let (reply, result) = flume::bounded(1);
                committer.process(
                    CommitProtocol::GcPass {
                        roots: cells
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| operation[3] & (1 << i) != 0)
                            .map(|(_, &cell)| WorldRoot::Var(cell))
                            .collect(),
                        reply,
                    },
                    &sender,
                );
                let result = result.recv().unwrap();
                if live == 0 {
                    let mut reclaimed = 0;
                    for (i, value) in world.iter_mut().enumerate() {
                        if operation[3] & (1 << i) == 0 && value.take().is_some() {
                            reclaimed += 1;
                        }
                    }
                    assert_eq!(result.unwrap().reclaimed, reclaimed);
                } else {
                    assert_eq!(result.unwrap_err().live, live);
                }
            }
        }
        while let Ok(message) = receiver.try_recv() {
            assert!(committer.process(message, &sender));
        }
        let snapshot = committer.snapshot_clone();
        for i in 0..4 {
            let actual = snapshot.read(cells[i]).map(|value| value.lpc_ref());
            assert_eq!(actual, world[i].map(LpcRef::from));
        }
    }
}
