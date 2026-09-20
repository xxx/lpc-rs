const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

function load(simulation = false) {
  const context = { window: {} };
  const files = simulation ? ['iso', 'model', 'world', 'sim'] : ['model'];
  for (const name of files) {
    vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../js', name + '.js'), 'utf8'), context);
  }
  return context.window;
}
function finish(GC, m) {
  GC.roots(m); GC.admit(m); GC.seed(m);
  let visits = 0;
  while (m.stack.length) {
    assert.ok(++visits < 500, 'the graph walk terminates');
    GC.take(m); GC.trace(m);
  }
  GC.sweep(m); GC.report(m);
  return m;
}
function until(Sim, predicate, budget = 15000) {
  for (let i = 0; i < budget; i++) {
    if (predicate()) return;
    Sim.update(0.05);
  }
  assert.fail('simulation did not reach the expected state');
}

test('default graph retains mapping keys, partial arguments, captures, and old image globals', () => {
  const { GC } = load(), m = finish(GC, GC.create());
  assert.equal(m.report.reclaimed, 4);
  assert.equal(GC.counts(m).remaining, 14);
  for (const id of ['key', 'partial', 'capture', 'keepsake', 'old-global']) assert.ok(m.marked[id], id);
  assert.deepEqual(Array.from(m.removed), ['dead-capture', 'orphan-image', 'cycle-0-a', 'cycle-0-b']);
  assert.ok(m.duplicates > 0);
  assert.equal(m.visits, 28);
});

test('cutting the global root makes the entire cyclic subgraph collectible', () => {
  const { GC } = load(), m = finish(GC, GC.create({ linked: false }));
  assert.equal(m.report.reclaimed, 11);
  for (const id of ['pack', 'ledger', 'key', 'partial', 'capture', 'keepsake', 'old-global']) assert.ok(!m.cells[id], id);
  assert.ok(m.cells.global);
});

test('all world controls produce derived counts, including each callback owner', () => {
  const { GC } = load();
  for (const linked of [false, true]) {
    for (const callback of ['none', 'callout', 'input', 'rule']) {
      for (let cycles = 0; cycles <= 3; cycles++) {
        for (let pins = 0; pins <= 3; pins++) {
          const m = finish(GC, GC.create({ linked, callback, cycles, pins }));
          const c = GC.counts(m);
          assert.equal(c.before, 16 + cycles * 2);
          if (pins) {
            assert.equal(m.report.refused, pins);
            assert.equal(m.visits, 0);
            assert.equal(c.marked, 0);
            assert.equal(c.remaining, c.before);
          } else {
            assert.equal(m.report.reclaimed, 2 + cycles * 2 + (!linked && callback === 'none' ? 7 : 0));
            assert.equal(c.before, c.remaining + c.reclaimed);
            assert.equal(c.remaining, c.marked + c.protected);
          }
        }
      }
    }
  }
});

test('the mark set distinguishes an unrooted connection from a reachable cell', () => {
  const { GC } = load(), m = finish(GC, GC.create());
  assert.equal(GC.status(m, 'tombstone'), 'retained');
  assert.ok(!m.marked.tombstone);
  assert.equal(GC.collectable({ kind: 'Rules' }), false);
  assert.equal(GC.collectable({ kind: 'Process' }), false);
  assert.equal(GC.collectable({ kind: 'Image' }), true);
});

test('admission and the sweep guard prevent partial collection', () => {
  const { GC } = load(), m = GC.create();
  GC.sweep(m);
  assert.equal(m.swept, false);
  GC.roots(m); GC.admit(m); GC.seed(m); GC.take(m);
  GC.sweep(m);
  assert.equal(m.swept, false);
  assert.equal(GC.counts(m).remaining, 18);
  const refused = GC.create({ pins: 1 });
  const before = JSON.stringify(refused.cells);
  finish(GC, refused);
  assert.equal(JSON.stringify(refused.cells), before);
  assert.equal(refused.seeded, false);
});

test('bootstrap processes trace their initial image when its committed slot is absent', () => {
  const { GC } = load(), m = GC.create();
  delete m.cells.image;
  finish(GC, m);
  assert.ok(m.marked.global);
  assert.ok(m.marked.pack);
  assert.ok(m.imagesSeen.current);
});

test('primitive and object references add no payload edges', () => {
  const { GC } = load(), m = GC.create();
  m.cells.global.value = { kind: 'Object', id: 'pack' };
  finish(GC, m);
  assert.ok(!m.marked.pack);
  assert.equal(m.report.reclaimed, 11);
});

test('image generations are deduplicated and a missing Var has no edges', () => {
  const { GC } = load(), m = GC.create();
  GC.roots(m); GC.admit(m); GC.seed(m);
  m.stack.push({ kind: 'Var', id: 'absent' }, { kind: 'Image', id: 'older' }, { kind: 'Image', id: 'older' });
  while (m.stack.length) { GC.take(m); GC.trace(m); }
  assert.ok(m.marked.absent);
  assert.ok(m.marked['old-global']);
  assert.ok(m.duplicates >= 2);
});

test('station sequence follows the work stack and normal tour lasts about five minutes', () => {
  const { Sim, GC } = load(true), events = [];
  Sim.on((kind, id) => { if (kind === 'station') events.push(id); });
  Sim.run(); let seconds = 0;
  while (!Sim.state.finished && seconds < 600) { Sim.update(0.05); seconds += 0.05; }
  assert.ok(Sim.state.finished);
  assert.ok(seconds > 240 && seconds < 360);
  assert.equal(events.filter(id => id === 'take').length, Sim.state.model.visits);
  assert.equal(events.filter(id => id === 'trace').length, Sim.state.model.visits);
  assert.ok(!events.includes('refuse'));
  assert.deepEqual([...new Set(events)], ['request', 'gate', 'roots', 'take', 'trace', 'sweep', 'report', 'done']);
  assert.equal(GC.counts(Sim.state.model).reclaimed, 4);
});

test('a refused request takes the short branch and never visits the mark loop', () => {
  const { Sim } = load(true), events = [];
  Sim.state.options.pins = 2;
  Sim.on((kind, id) => { if (kind === 'station') events.push(id); });
  Sim.run(); until(Sim, () => Sim.state.finished);
  assert.deepEqual(events, ['request', 'gate', 'refuse', 'done']);
  assert.equal(Sim.state.model.report.refused, 2);
});

test('stepping advances exactly one station and pause holds its reading time', () => {
  const { Sim } = load(true);
  Sim.run(); Sim.pause(); Sim.step();
  until(Sim, () => Sim.state.paused);
  assert.equal(Sim.state.station, 'request');
  const dwell = Sim.state.dwellLeft;
  for (let i = 0; i < 100; i++) Sim.update(0.05);
  assert.equal(Sim.state.dwellLeft, dwell);
  Sim.step(); until(Sim, () => Sim.state.paused);
  assert.equal(Sim.state.station, 'gate');
});

test('reading stops ignore travel acceleration, and only Replay forgets read topics', () => {
  const { Sim } = load(true);
  Sim.run(); until(Sim, () => Sim.state.station === 'request');
  const before = Sim.state.dwellLeft;
  Sim.state.fastForward = true; Sim.state.tourDone = true; Sim.state.speed = 2;
  Sim.update(0.5);
  assert.equal(Sim.state.dwellLeft, before - 1);
  Sim.run(); until(Sim, () => Sim.state.station === 'request');
  assert.equal(Sim.state.reading, false);
  Sim.replayTour(); Sim.run(); until(Sim, () => Sim.state.station === 'request');
  assert.equal(Sim.state.reading, true);
});
