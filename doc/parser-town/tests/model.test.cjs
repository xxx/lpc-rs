const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const context = { window: {} };
vm.runInNewContext(
  fs.readFileSync(path.join(__dirname, '../js/model.js'), 'utf8'),
  context
);
const M = context.window.ParserModel;
const plain = (x) => JSON.parse(JSON.stringify(x));

test('all three frontends recognize give and chat through the same engine', () => {
  for (const family of M.families)
    for (const lesson of ['give', 'chat']) {
      const m = M.run({ family, lesson });
      assert.equal(m.error, null, family + ' ' + lesson);
      assert.equal(m.recognized, true);
      assert.equal(m.chart.sets.length, m.scan.tokens.length + 1);
      assert.ok(
        m.chart.counts.predict && m.chart.counts.scan && m.chart.counts.complete
      );
    }
  assert.deepEqual(plain(M.run({ family: 'native' }).output), [
    { object: '/room/sword' },
    { object: '/players/bob' },
  ]);
  assert.equal(M.run({ family: 'parser' }).output, 1);
  assert.deepEqual(plain(M.run({ family: 'dgd' }).output), [
    'give',
    'red',
    'sword',
    'to',
    'bob',
  ]);
});
test('parser package removes the verb before entering the grammar', () => {
  const m = M.run({ family: 'parser' });
  assert.equal(m.engineInput, 'red sword to bob');
  assert.equal(m.scan.tokens.length, 4);
  assert.equal(M.run({ family: 'native' }).scan.tokens.length, 5);
  const unknown = M.run({ family: 'parser', line: 'take sword' });
  assert.match(unknown.error, /No registered rule/);
  assert.equal(unknown.chart, null);
});
test('syntax and noun resolution have different failure boundaries', () => {
  for (const family of ['native', 'parser']) {
    const m = M.run({ family, sword: false });
    assert.equal(m.recognized, true);
    assert.match(m.error, /noun/);
    assert.equal(m.output, null);
  }
  assert.equal(M.run({ family: 'dgd', sword: false }).error, null);
  for (const family of M.families) {
    const m = M.run({ family, line: 'give red sword with bob' });
    assert.equal(m.recognized, false);
    assert.match(m.error, /complete start/);
    assert.equal(m.calls.length, 0);
  }
});
test('native empty text, parser STR and DGD plus retain their dialect difference', () => {
  const m = M.run({ family: 'native', lesson: 'chat', line: 'say' });
  assert.deepEqual(plain(m.output), ['']);
  for (const family of ['parser', 'dgd'])
    assert.equal(
      M.run({ family, lesson: 'chat', line: 'say' }).recognized,
      false
    );
});
test('handler refusal occurs outside the shared chart', () => {
  for (const family of M.families) {
    const m = M.run({ family, allow: false });
    assert.equal(m.recognized, true);
    assert.equal(m.output, null);
    assert.ok(m.error);
    assert.equal(m.calls.length, 1);
  }
  const m = M.run({ family: 'parser' });
  assert.equal(m.calls.length, 6);
  assert.match(m.calls[1], /direct_give_obj_to_liv\(\/room\/sword, 0/);
  assert.match(m.calls[3], /all slots filled/);
  assert.match(m.calls[5], /^do_give/);
});
test('maximal munch, rule-order ties, dropped whitespace and exact spans', () => {
  const g = M.compileGroups(M.nativeGroups('%w %w'));
  const scan = M.tokenize(g, '42  42x');
  assert.deepEqual(
    plain(scan.tokens.map((t) => [t.className, t.start, t.end])),
    [
      ['number', 0, 2],
      ['word', 4, 7],
    ]
  );
  const dgd = M.compileDgd("word = /[a-z]+/\nS: 'give' word");
  assert.equal(M.tokenize(dgd, 'give').tokens[0].className, "'give'");
  assert.equal(M.tokenize(dgd, 'given').tokens[0].className, 'word');
  assert.match(
    M.run({ family: 'dgd', line: 'give sword to bob!' }).error,
    /No token/
  );
  const m = M.run({
    family: 'native',
    lesson: 'chat',
    line: 'say hello   there',
  });
  assert.equal(m.captures[0].text, 'hello   there');
});
function recognize(g, text) {
  const scan = M.tokenize(g, text),
    chart = M.chart(g, scan.tokens);
  while (chart.column < scan.tokens.length) M.advanceColumn(chart);
  return chart;
}
test('nullable productions and left recursion terminate with real chart deduplication', () => {
  const g = M.compileGroups(M.nativeGroups("'say' [to] %s"));
  assert.equal(M.accepted(recognize(g, 'say')), true);
  assert.equal(M.accepted(recognize(g, 'say to bob')), true);
  assert.equal(M.accepted(recognize(g, 'SAY to bob')), false);
  for (const n of [1, 2, 8, 20]) {
    const c = recognize(
      M.compileGroups(M.nativeGroups('%s')),
      Array(n).fill('x').join(' ')
    );
    assert.equal(M.accepted(c), true);
    assert.equal(M.trees(c).length, 1);
    for (const set of c.sets)
      assert.equal(
        new Set(set.map((s) => s.prod + '/' + s.dot + '/' + s.origin)).size,
        set.length
      );
  }
});
test('derivations try the longest earlier capture before a shorter split', () => {
  const g = M.compileGroups(M.nativeGroups("'say' %s 'to' %s"));
  const c = recognize(g, 'say hi to bob to sam');
  const m = {
    g,
    scan: { tokens: c.tokens },
    engineInput: 'say hi to bob to sam',
  };
  const trees = M.trees(c);
  assert.equal(trees.length, 2);
  assert.deepEqual(plain(M.captures(m, trees[0]).map((c) => c.text)), [
    'hi to bob',
    'sam',
  ]);
  assert.deepEqual(plain(M.captures(m, trees[1]).map((c) => c.text)), [
    'hi',
    'bob to sam',
  ]);
});
test('unsupported lesson syntax is rejected, never silently accepted', () => {
  assert.throws(() => M.nativeGroups('%i'), /lesson supports/);
  assert.throws(() => M.parserGroups('STR STR'), /Only one STR/);
  assert.throws(() => M.parserGroups('OBJect'), /Token inside/);
  assert.throws(
    () => M.compileDgd('word = /.+/\nS: word'),
    /outside the lesson/
  );
});
function loadSim() {
  const c = { window: {} };
  for (const file of ['iso', 'model', 'world', 'sim']) {
    vm.runInNewContext(
      fs.readFileSync(path.join(__dirname, '../js/' + file + '.js'), 'utf8'),
      c
    );
    Object.assign(c, c.window);
  }
  return c;
}
test('every station has a finite waypoint and every route can be traversed', () => {
  const { World } = loadSim();
  for (const [name, stations] of Object.entries(World.stations)) {
    for (const st of stations) {
      assert.ok(Number.isFinite(st.dist), name + '/' + st.id);
      assert.ok(st.dist >= 0 && st.dist <= World.routes[name].total);
      assert.ok(World.byId[st.id]);
    }
  }
});
test('simulation routes success, noun failure, refusal, syntax and lexical failures', () => {
  for (const family of M.families)
    for (const changes of [
      {},
      { sword: false },
      { allow: false },
      { line: 'give sword with bob' },
      { line: '!?' },
    ]) {
      const { Sim } = loadSim();
      Object.assign(Sim.state.input, { family }, changes);
      const oracle = M.run(Sim.state.input);
      let visited = [];
      Sim.on((event, id) => {
        if (event === 'station') visited.push(id);
      });
      Sim.run();
      for (let i = 0; i < 15000 && !Sim.state.finished; i++) Sim.update(0.05);
      assert.ok(Sim.state.finished, JSON.stringify(Sim.state.input));
      assert.equal(Sim.state.model.error, oracle.error);
      assert.deepEqual(plain(Sim.state.model.output), plain(oracle.output));
      assert.equal(visited.at(-1), oracle.error ? 'reject' : 'receipt');
      if (!oracle.error)
        assert.equal(
          visited.filter((id) => id === 'chart').length,
          oracle.scan.tokens.length + 1
        );
      if (family === 'dgd') assert.ok(!visited.includes('groups'));
      if (!oracle.chart) assert.ok(!visited.includes('chart'));
    }
});
test('first stops have reading time, repeat runs remember it, and stepping pauses exactly once', () => {
  const { Sim, World } = loadSim();
  Sim.run();
  Sim.update(0.001);
  assert.equal(Sim.state.station, 'native');
  assert.equal(Sim.state.reading, true);
  assert.equal(Sim.state.dwellTotal, World.readSeconds('native'));
  const before = Sim.state.dwellLeft;
  Sim.state.fastForward = true;
  Sim.state.tourDone = true;
  Sim.update(0.05);
  assert.ok(Math.abs(Sim.state.dwellLeft - (before - 0.05)) < 0.0001);
  Sim.pause();
  Sim.update(1);
  assert.ok(Math.abs(Sim.state.dwellLeft - (before - 0.05)) < 0.0001);
  Sim.step();
  for (let i = 0; i < 1000 && !Sim.state.paused; i++) Sim.update(0.05);
  assert.equal(Sim.state.station, 'groups');
  assert.equal(Sim.state.paused, true);
  Sim.run();
  Sim.update(0.001);
  assert.equal(Sim.state.reading, false);
  Sim.replayTour();
  Sim.run();
  Sim.update(0.001);
  assert.equal(Sim.state.reading, true);
});
test('a first native tour at 1× has time to read and completes in four to six minutes', () => {
  const { Sim } = loadSim();
  Sim.run();
  let seconds = 0;
  while (!Sim.state.finished && seconds < 500) {
    Sim.update(0.05);
    seconds += 0.05;
  }
  assert.ok(seconds >= 240 && seconds <= 360, seconds);
});

test('verb selection and teaching limits do not pretend to be grammar failures', () => {
  const native = M.run({ family: 'native', line: 'take sword' });
  assert.equal(native.stopKind, 'verb');
  assert.equal(native.chart, null);
  const capped = M.run({
    family: 'parser',
    lesson: 'chat',
    line: 'say ' + Array(25).fill('x').join(' '),
  });
  assert.equal(capped.stopKind, 'limit');
  assert.equal(capped.chart, null);
});
