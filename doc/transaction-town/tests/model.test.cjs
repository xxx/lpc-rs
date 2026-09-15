const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
function load(files) {
  const context = { window: {} };
  for (const file of files)
    vm.runInNewContext(
      fs.readFileSync(path.join(__dirname, '../js', file + '.js'), 'utf8'),
      context
    );
  return context.window;
}
const T = load(['model']).Txn;

test('a newer write to a tracked cell rejects an immutable older snapshot', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.write(b, 'gold', 13);
  T.commit(w, b);
  assert.equal(T.read(a, 'gold'), 10);
  T.write(a, 'gold', 14);
  const result = T.commit(w, a);
  assert.equal(result.ok, false);
  assert.equal(result.conflict.cell, 'gold');
  assert.equal(result.conflict.writtenAt, 1);
  assert.equal(w.values.gold, 13);
});
test('write history catches a write even when the value is unchanged', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.read(a, 'gold');
  T.write(b, 'gold', 10);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, false);
});
test('unrelated newer writes preserve the read dependency', () => {
  const w = T.world({ gold: 10, bells: 0 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.write(a, 'gold', T.read(a, 'gold') + 4);
  T.write(b, 'bells', 3);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, true);
  assert.equal(w.values.gold, 14);
  assert.equal(w.values.bells, 3);
});
test('blind writes, including reads of own writes, follow commit order', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.write(a, 'gold', 4);
  assert.equal(T.read(a, 'gold'), 4);
  assert.equal(Object.keys(a.reads).length, 0);
  T.write(b, 'gold', 13);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, true);
  assert.equal(w.values.gold, 4);
});
test('unread addition merges fold onto the latest committed value', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.merge(a, 'gold', 4);
  T.merge(b, 'gold', 3);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, true);
  assert.equal(w.values.gold, 17);
});
test('reading a queued merge introduces a dependency and a private write', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.merge(a, 'gold', 4);
  assert.equal(T.read(a, 'gold'), 14);
  assert.equal(a.writes.gold, 14);
  assert.equal(Object.keys(a.merges).length, 0);
  T.write(b, 'gold', 13);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, false);
});
test('a merge type mismatch rejects all writes before publication', () => {
  const w = T.world({ gold: 10, bells: 0 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.merge(a, 'gold', 4);
  T.write(a, 'bells', 9);
  T.write(b, 'gold', 'changed type');
  T.commit(w, b);
  assert.equal(T.commit(w, a).conflict.kind, 'merge');
  assert.equal(w.values.bells, 0);
});
test('reads of absence conflict with creation and removals stay private', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  assert.equal(T.read(a, 'newCell'), undefined);
  T.write(b, 'newCell', 7);
  T.commit(w, b);
  assert.equal(T.commit(w, a).ok, false);
  const c = T.begin(w, 'C');
  T.remove(c, 'gold');
  assert.equal(T.read(c, 'gold'), undefined);
  assert.equal(w.values.gold, 10);
  T.commit(w, c);
  assert.equal(w.values.gold, undefined);
});
test('read-only commit validates without advancing the version', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A');
  T.read(a, 'gold');
  assert.equal(T.commit(w, a).ok, true);
  assert.equal(w.version, 0);
  assert.equal(w.history.length, 0);
});
test('buffered output never escapes a failed attempt, and success delivers once', () => {
  const w = T.world({ gold: 10 }),
    a = T.begin(w, 'A'),
    b = T.begin(w, 'B');
  T.read(a, 'gold');
  T.effect(a, 'stale receipt');
  assert.throws(() => T.deliver(w, a), /Uncommitted/);
  T.write(b, 'gold', 13);
  T.commit(w, b);
  T.commit(w, a);
  T.discard(a);
  assert.equal(a.effects.length, 0);
  assert.equal(w.output.length, 0);
  const c = T.begin(w, 'A');
  T.write(c, 'gold', T.read(c, 'gold') + 4);
  T.effect(c, 'success');
  T.commit(w, c);
  assert.equal(Object.keys(c.writes).length, 0);
  assert.equal(Object.keys(c.reads).length, 0);
  assert.equal(c.effects.length, 1);
  assert.equal(w.values.gold, 17);
  assert.equal(w.output.length, 0);
  T.deliver(w, c);
  T.deliver(w, c);
  assert.equal(w.output.length, 1);
});
test('retry queues are FIFO and independent per cell', () => {
  const w = T.world({});
  T.enqueue(w, 'gold', 'A');
  T.enqueue(w, 'gold', 'B');
  T.enqueue(w, 'bells', 'C');
  assert.equal(T.admitted(w, 'gold', 'A'), true);
  assert.equal(T.admitted(w, 'gold', 'B'), false);
  assert.equal(T.admitted(w, 'bells', 'C'), true);
  T.release(w, 'gold', 'A');
  assert.equal(T.admitted(w, 'gold', 'B'), true);
  T.release(w, 'gold', 'B');
  assert.equal(w.queues.gold, undefined);
});
test('all input combinations follow computed routes and emit exactly one receipt', () => {
  for (const mode of ['read', 'blind', 'merge'])
    for (const sameCell of [true, false])
      for (let rounds = 0; rounds <= 3; rounds++)
        for (const amount of [1, 4, 9]) {
          const { Sim: S } = load(['iso', 'model', 'world', 'sim']);
          Object.assign(S.state.input, {
            mode,
            sameCell,
            rounds,
            amount,
            rivalAmount: 3
          });
          S.state.speed = 8;
          S.run();
          for (let i = 0; i < 12000 && !S.state.finished; i++) S.update(0.05);
          assert.equal(S.state.finished, true);
          const m = S.state.model,
            conflicts = mode === 'read' && sameCell ? rounds : 0;
          assert.equal(m.conflicts, conflicts);
          assert.equal(m.attempts, conflicts + 1);
          const bCommits = conflicts ? rounds : Math.min(rounds, 1);
          assert.equal(
            m.world.values.gold,
            mode === 'blind'
              ? amount
              : 10 + amount + (sameCell ? 3 * bCommits : 0)
          );
          assert.equal(m.world.values.bells, sameCell ? 0 : 3 * bCommits);
          assert.equal(m.world.output.length, 1);
          assert.equal(Object.keys(m.world.queues).length, 0);
        }
});
test('reading stops ignore travel boosts, resets preserve reading, and replay clears it', () => {
  const { Sim: S, World: W } = load(['iso', 'model', 'world', 'sim']);
  S.run();
  S.update(0.01);
  const dwell = S.state.dwellLeft;
  assert.equal(dwell, W.readSeconds('snapshot'));
  S.state.fastForward = true;
  S.state.tourDone = true;
  S.update(1);
  assert.equal(S.state.dwellLeft, dwell - 1);
  S.reset();
  S.update(0.01);
  assert.ok(S.state.dwellLeft < 3);
  S.replayTour();
  S.reset();
  S.update(0.01);
  assert.equal(S.state.dwellLeft, dwell);
});
test('step skips the dwell, pauses at exactly one new station, and new parameters affect future work', () => {
  const { Sim: S } = load(['iso', 'model', 'world', 'sim']);
  S.run();
  S.update(0.01);
  S.pause();
  S.step();
  for (let i = 0; i < 1000 && !S.state.paused; i++) S.update(0.05);
  assert.equal(S.state.station, 'read');
  S.state.input.amount = 9;
  S.step();
  for (let i = 0; i < 1000 && !S.state.paused; i++) S.update(0.05);
  assert.equal(S.state.station, 'write');
  assert.equal(S.state.model.tx.writes.gold, 19);
  S.state.input.amount = 1;
  assert.equal(S.state.model.tx.writes.gold, 19);
});
