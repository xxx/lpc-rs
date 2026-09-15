(function (global) {
  'use strict';

  function copy(object) {
    var result = Object.create(null);
    Object.keys(object).forEach(function (key) {
      result[key] = object[key];
    });
    return result;
  }
  function owns(object, key) {
    return Object.prototype.hasOwnProperty.call(object, key);
  }

  // SCALED: two scalar cells replace the driver's persistent map of WorldValues.
  function world(values) {
    return {
      version: 0,
      values: copy(values),
      history: [],
      output: [],
      queues: Object.create(null)
    };
  }
  function begin(w, owner) {
    return {
      owner: owner,
      base: w.version,
      snapshot: copy(w.values),
      reads: Object.create(null),
      writes: Object.create(null),
      merges: Object.create(null),
      removed: Object.create(null),
      effects: [],
      status: 'open',
      delivered: false
    };
  }
  function assertOpen(tx) {
    if (tx.status !== 'open')
      throw new Error('Attempt is already ' + tx.status);
  }
  function read(tx, cell) {
    assertOpen(tx);
    if (owns(tx.removed, cell)) return undefined;
    if (owns(tx.writes, cell)) return tx.writes[cell];
    tx.reads[cell] = true;
    var value = tx.snapshot[cell];
    if (owns(tx.merges, cell)) {
      value = (value === undefined ? 0 : value) + tx.merges[cell];
      tx.writes[cell] = value;
      delete tx.merges[cell];
    }
    return value;
  }
  function write(tx, cell, value) {
    assertOpen(tx);
    tx.writes[cell] = value;
    delete tx.merges[cell];
    delete tx.removed[cell];
  }
  function merge(tx, cell, delta) {
    assertOpen(tx);
    if (owns(tx.writes, cell)) tx.writes[cell] += delta;
    else if (owns(tx.removed, cell)) write(tx, cell, delta);
    else tx.merges[cell] = (tx.merges[cell] || 0) + delta;
  }
  function remove(tx, cell) {
    assertOpen(tx);
    tx.removed[cell] = true;
    delete tx.writes[cell];
    delete tx.merges[cell];
  }
  function effect(tx, message) {
    assertOpen(tx);
    tx.effects.push(message);
  }

  function validate(w, tx) {
    if (tx.base > w.version) return { kind: 'future' };
    for (var i = 0; i < w.history.length; i++) {
      var entry = w.history[i];
      if (entry.version <= tx.base) continue;
      for (var j = 0; j < entry.cells.length; j++) {
        if (owns(tx.reads, entry.cells[j])) {
          return {
            kind: 'read',
            cell: entry.cells[j],
            base: tx.base,
            current: w.version,
            writtenAt: entry.version,
            writer: entry.owner
          };
        }
      }
    }
    var cells = Object.keys(tx.merges);
    for (var k = 0; k < cells.length; k++) {
      var value = w.values[cells[k]];
      if (value !== undefined && typeof value !== 'number')
        return { kind: 'merge', cell: cells[k] };
    }
    return null;
  }
  function commit(w, tx) {
    assertOpen(tx);
    var conflict = validate(w, tx);
    if (conflict) {
      tx.status = 'rejected';
      return { ok: false, conflict: conflict };
    }
    var next = copy(w.values),
      changed = Object.create(null);
    Object.keys(tx.writes).forEach(function (cell) {
      next[cell] = tx.writes[cell];
      changed[cell] = true;
    });
    Object.keys(tx.merges).forEach(function (cell) {
      next[cell] =
        (next[cell] === undefined ? 0 : next[cell]) + tx.merges[cell];
      changed[cell] = true;
    });
    Object.keys(tx.removed).forEach(function (cell) {
      delete next[cell];
      changed[cell] = true;
    });
    var cells = Object.keys(changed);
    if (cells.length) {
      w.version++;
      w.values = next;
      w.history.push({ version: w.version, cells: cells, owner: tx.owner });
    }
    tx.status = 'committed';
    clearChanges(tx);
    return { ok: true, version: w.version };
  }
  function clearChanges(tx) {
    tx.reads = Object.create(null);
    tx.writes = Object.create(null);
    tx.merges = Object.create(null);
    tx.removed = Object.create(null);
  }
  function discard(tx) {
    if (tx.status !== 'rejected')
      throw new Error('Only a rejected attempt can be discarded');
    clearChanges(tx);
    tx.effects = [];
  }
  function deliver(w, tx) {
    if (tx.status !== 'committed')
      throw new Error('Uncommitted output cannot be delivered');
    if (tx.delivered) return;
    tx.effects.forEach(function (message) {
      w.output.push({ owner: tx.owner, message: message });
    });
    tx.effects = [];
    tx.delivered = true;
  }
  function enqueue(w, cell, owner) {
    var queue = w.queues[cell] || (w.queues[cell] = []);
    if (queue.indexOf(owner) < 0) queue.push(owner);
  }
  function admitted(w, cell, owner) {
    return !!w.queues[cell] && w.queues[cell][0] === owner;
  }
  function release(w, cell, owner) {
    var queue = w.queues[cell];
    if (!queue) return;
    var index = queue.indexOf(owner);
    if (index >= 0) queue.splice(index, 1);
    if (!queue.length) delete w.queues[cell];
  }

  // ASSUMED: B commits before A on the selected number of attempts.
  function lesson() {
    return {
      world: world({ gold: 10, bells: 0 }),
      tx: null,
      rival: null,
      attempts: 0,
      conflicts: 0,
      competingCommits: 0,
      turn: null,
      result: null,
      lastSubmission: null,
      observed: null,
      ownValue: null,
      discardedEffects: 0,
      admittedTurns: 0,
      trace: []
    };
  }
  function record(m, text) {
    m.trace.push(text);
  }
  var operations = {
    snapshot: function (m, input) {
      m.tx = begin(m.world, 'A');
      m.attempts++;
      m.mode = input.mode;
      m.observed = null;
      m.ownValue = null;
      m.result = null;
      record(m, 'A opens attempt ' + m.attempts + ' at v' + m.tx.base + '.');
    },
    read: function (m) {
      if (m.mode === 'read') m.observed = read(m.tx, 'gold');
      record(
        m,
        m.mode === 'read'
          ? 'A reads gold = ' + m.observed + '.'
          : 'A does not read committed gold.'
      );
    },
    write: function (m, input) {
      m.amount = input.amount;
      if (m.mode === 'merge') merge(m.tx, 'gold', m.amount);
      else {
        write(
          m.tx,
          'gold',
          m.mode === 'blind' ? m.amount : m.observed + m.amount
        );
        m.ownValue = read(m.tx, 'gold');
      }
      record(
        m,
        m.mode === 'merge'
          ? 'A queues add(' + m.amount + ').'
          : 'A privately writes gold = ' + m.ownValue + '.'
      );
    },
    effects: function (m) {
      effect(
        m.tx,
        m.mode === 'blind'
          ? 'A set gold to ' + m.amount + '.'
          : 'A added ' + m.amount + ' gold.'
      );
      record(m, 'A buffers one receipt; delivered output is still empty.');
    },
    rival: function (m, input) {
      if (m.competingCommits >= input.rounds) {
        m.rival = null;
        record(m, 'B has no more scheduled writes.');
        return;
      }
      var b = begin(m.world, 'B'),
        cell = input.sameCell ? 'gold' : 'bells';
      var before = read(b, cell);
      write(b, cell, before + input.rivalAmount);
      var result = commit(m.world, b);
      m.rival = {
        tx: b,
        cell: cell,
        before: before,
        after: m.world.values[cell],
        version: result.version
      };
      m.competingCommits++;
      record(
        m,
        'B commits ' +
          cell +
          ' = ' +
          m.rival.after +
          ' at v' +
          result.version +
          '.'
      );
    },
    commit: function (m) {
      m.lastSubmission = {
        reads: Object.keys(m.tx.reads),
        writes: copy(m.tx.writes),
        merges: copy(m.tx.merges),
        effects: m.tx.effects.length,
        base: m.tx.base
      };
      m.result = commit(m.world, m.tx);
      if (m.turn) {
        release(m.world, m.turn, 'A');
        m.turn = null;
      }
      if (!m.result.ok) {
        m.conflicts++;
        m.discardedEffects += m.tx.effects.length;
        discard(m.tx);
        enqueue(m.world, m.result.conflict.cell, 'A');
      }
      record(
        m,
        m.result.ok
          ? 'A commits at v' + m.world.version + '; output awaits delivery.'
          : 'A rejected: ' +
              m.result.conflict.cell +
              ' was written after v' +
              m.tx.base +
              '.'
      );
    },
    retry: function (m) {
      record(m, 'Discarded buffers stay empty on the return road.');
    },
    admission: function (m) {
      var cell = m.result.conflict.cell;
      if (!admitted(m.world, cell, 'A'))
        throw new Error('Retry must wait for its cell turn');
      m.turn = cell;
      m.admittedTurns++;
      record(
        m,
        'A acquires the retry turn for ' +
          cell +
          ' before taking a fresh snapshot.'
      );
    },
    deliver: function (m) {
      deliver(m.world, m.tx);
      record(m, 'The successful attempt delivers its receipt once.');
    },
    finish: function (m) {
      record(m, 'Task finished after ' + m.attempts + ' attempt(s).');
    }
  };

  global.Txn = {
    world: world,
    begin: begin,
    read: read,
    write: write,
    merge: merge,
    remove: remove,
    effect: effect,
    validate: validate,
    commit: commit,
    discard: discard,
    deliver: deliver,
    enqueue: enqueue,
    admitted: admitted,
    release: release,
    lesson: lesson,
    operations: operations
  };
})(window);
