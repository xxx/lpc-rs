(function (global) {
  'use strict';

  // ASSUMED: this small committed world illustrates edges, not a measured game heap.
  function ref(kind, id) { return { kind: kind, id: id }; }
  function work(kind, id) { return { kind: kind, id: id }; }
  function owns(map, id) { return Object.prototype.hasOwnProperty.call(map, id); }
  function collectable(cell) {
    return ['Ref', 'Array', 'Mapping', 'Image'].indexOf(cell.kind) !== -1;
  }
  function create(options) {
    var p = { linked: true, callback: 'none', pins: 0, cycles: 1 };
    Object.keys(options || {}).forEach(function (key) { p[key] = options[key]; });
    var m = {
      options: p, cells: {}, order: [], roots: [], stack: [], marked: {}, imagesSeen: {},
      current: null, removed: [], visits: 0, duplicates: 0, admitted: null, seeded: false,
      swept: false, report: null, events: [], message: 'Prepare a collection request.'
    };
    function cell(id, kind, value, label) {
      m.cells[id] = { id: id, kind: kind, value: value, label: label || id };
      m.order.push(id);
    }
    m.functions = {
      bell: { image: 'older', partial: [ref('Array', 'partial')], captures: ['capture'] }
    };
    m.images = { current: ['global'], older: ['old-global'], abandoned: ['dead-capture'] };
    m.processes = { player: { vars: ['image', 'connection', 'rules'], imageCell: 'image', initial: 'current' } };
    cell('revision', 'Ref', ref('Int', 1), 'Object-space revision');
    cell('path', 'Process', 'player', 'Live object identity');
    cell('image', 'Image', 'current', 'Current image slot');
    cell('global', 'Ref', p.linked ? ref('Array', 'pack') : ref('Int', 0), 'Object global');
    cell('pack', 'Array', [ref('Mapping', 'ledger')], 'Inventory array');
    cell('ledger', 'Mapping', [[ref('Array', 'key'), ref('Function', 'bell')]], 'Mapping payload');
    cell('key', 'Array', [], 'Array used as a key');
    cell('partial', 'Array', [ref('Array', 'pack')], 'Partial argument');
    cell('capture', 'Ref', ref('Array', 'keepsake'), 'Captured cell');
    cell('keepsake', 'Array', [], 'Captured array');
    cell('old-global', 'Ref', ref('Int', 7), 'Retained image global');
    cell('connection', 'Connection', p.callback === 'input' ? ref('Function', 'bell') : null, 'Input callback owner');
    cell('rules', 'Rules', p.callback === 'rule' ? [ref('Function', 'bell')] : [], 'Parser rule owner');
    cell('tombstone', 'Connection', null, 'Unrooted identity');
    cell('dead-capture', 'Ref', ref('Int', 0), 'Abandoned capture');
    cell('orphan-image', 'Image', 'abandoned', 'Abandoned image slot');
    for (var i = 0; i < p.cycles; i++) {
      cell('cycle-' + i + '-a', 'Array', [ref('Array', 'cycle-' + i + '-b')], 'Unrooted cycle ' + (i + 1) + ' / a');
      cell('cycle-' + i + '-b', 'Array', [ref('Array', 'cycle-' + i + '-a')], 'Unrooted cycle ' + (i + 1) + ' / b');
    }
    m.initial = {};
    m.order.forEach(function (id) { m.initial[id] = m.cells[id]; });
    return m;
  }
  function note(m, text) { m.message = text; m.events.push(text); }
  function roots(m) {
    m.roots = [work('Var', 'revision'), work('Var', 'path'), work('Process', 'player')];
    if (m.options.callback === 'callout') m.roots.push(work('Ref', ref('Function', 'bell')));
    note(m, m.roots.length + ' root entries: object-space cells, a live process' +
      (m.options.callback === 'callout' ? ', and a queued call-out function.' : '.'));
  }
  function admit(m) {
    m.admitted = m.options.pins === 0;
    note(m, m.admitted ? 'No live snapshot pins. The committer can complete this whole pass atomically.' :
      m.options.pins + ' live snapshot pin(s): GcRefused. No marking or sweeping happens.');
    return m.admitted;
  }
  function seed(m) {
    if (!m.admitted || m.seeded) return;
    m.stack = m.roots.slice();
    m.seeded = true;
    note(m, m.stack.length + ' root entries go onto the work stack. The last entry is visited first.');
  }
  function describe(item) {
    if (!item) return 'none';
    return item.kind === 'Ref' ? item.id.kind + ' → ' + item.id.id : item.kind + ' → ' + item.id;
  }
  function take(m) {
    if (!m.admitted || !m.stack.length || m.current) return;
    m.current = m.stack.pop();
    note(m, 'Pop ' + describe(m.current) + '; ' + m.stack.length + ' item(s) remain on the stack.');
  }
  function refWork(value) {
    return value && ['Array', 'Mapping', 'Function'].indexOf(value.kind) !== -1 ? work('Ref', value) : null;
  }
  function trace(m) {
    if (!m.admitted || !m.current) return;
    var item = m.current, added = [], duplicate = false;
    function push(kind, id) { added.push(work(kind, id)); }
    function pushRef(value) { var next = refWork(value); if (next) added.push(next); }
    function edges(cell) {
      if (cell.kind === 'Ref') pushRef(cell.value);
      else if (cell.kind === 'Array') cell.value.forEach(pushRef);
      else if (cell.kind === 'Mapping') cell.value.forEach(function (pair) { pair.forEach(pushRef); });
      else if (cell.kind === 'Process' && cell.value) push('Process', cell.value);
      else if (cell.kind === 'Image') push('Image', cell.value);
      else if (cell.kind === 'Connection') pushRef(cell.value);
      else if (cell.kind === 'Rules') cell.value.forEach(pushRef);
    }
    if (item.kind === 'Var') {
      if (owns(m.marked, item.id)) duplicate = true;
      else {
        m.marked[item.id] = true;
        if (owns(m.cells, item.id)) edges(m.cells[item.id]);
      }
    } else if (item.kind === 'Process') {
      var process = m.processes[item.id];
      process.vars.forEach(function (id) { push('Var', id); });
      if (!owns(m.cells, process.imageCell)) push('Image', process.initial);
    } else if (item.kind === 'Image') {
      if (owns(m.imagesSeen, item.id)) duplicate = true;
      else {
        m.imagesSeen[item.id] = true;
        m.images[item.id].forEach(function (id) { push('Var', id); });
      }
    } else if (item.kind === 'Ref') {
      var value = item.id;
      if (value.kind === 'Array' || value.kind === 'Mapping') push('Var', value.id);
      else if (value.kind === 'Function') {
        var fn = m.functions[value.id];
        if (fn.image) push('Image', fn.image);
        fn.partial.forEach(pushRef);
        fn.captures.forEach(function (id) { push('Var', id); });
      }
    }
    m.stack = m.stack.concat(added);
    m.visits++;
    if (duplicate) m.duplicates++;
    m.last = { item: item, added: added, duplicate: duplicate };
    m.current = null;
    note(m, describe(item) + (duplicate ? ' is already visited. Skip its edges; the cycle terminates.' :
      ' adds ' + added.length + ' work item(s)' + (added.length ? ': ' + added.map(describe).join(', ') : '') + '.'));
  }
  function sweep(m) {
    if (!m.admitted || !m.seeded || m.stack.length || m.current || m.swept) return;
    m.order.forEach(function (id) {
      if (owns(m.cells, id) && !owns(m.marked, id) && collectable(m.cells[id])) {
        m.removed.push(id);
        delete m.cells[id];
      }
    });
    m.swept = true;
    note(m, m.removed.length + ' unmarked Ref, Array, Mapping or Image cells reclaimed. Identity and rule cells stay.');
  }
  function report(m) {
    if (m.admitted === false) {
      m.report = { refused: m.options.pins };
      note(m, 'GcRefused { live: ' + m.options.pins + ' }. The world is unchanged.');
    } else if (m.swept) {
      m.report = { reclaimed: m.removed.length };
      note(m, 'GcReport { reclaimed: ' + m.removed.length + ' }. The atomic pass has returned its cell count.');
    }
    return m.report;
  }
  function counts(m) {
    var marked = m.order.filter(function (id) { return owns(m.marked, id); }).length;
    return { before: m.order.length, remaining: Object.keys(m.cells).length, marked: marked,
      work: m.stack.length + (m.current ? 1 : 0), reclaimed: m.removed.length,
      protected: m.order.filter(function (id) { return !owns(m.marked, id) && !collectable(m.initial[id]); }).length };
  }
  function status(m, id) {
    if (!owns(m.cells, id)) return 'reclaimed';
    if (owns(m.marked, id)) return 'marked';
    if (!collectable(m.cells[id])) return 'retained';
    return 'unmarked';
  }
  global.GC = { create: create, roots: roots, admit: admit, seed: seed, take: take, trace: trace,
    sweep: sweep, report: report, counts: counts, status: status, describe: describe, collectable: collectable };
})(window);
