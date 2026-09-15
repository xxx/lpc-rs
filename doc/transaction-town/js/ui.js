(function (global) {
  'use strict';
  var Sim = global.Sim,
    World = global.World,
    Renderer = global.Renderer;
  var pin = null,
    fly = null,
    lastPaint = 0,
    lastTopic = null,
    aboutWasPlaying = false,
    aboutFocus = null,
    sheetWasPlaying = false;
  function el(id) {
    return document.getElementById(id);
  }
  function text(id, value) {
    el(id).textContent = value;
  }
  function activeDistrict() {
    return pin || Sim.state.station || 'snapshot';
  }
  function showDistrict(d, move) {
    pin = d.id;
    Sim.pause();
    if (move) fly = { x: d.x, y: d.y };
    paint(true);
  }
  function unpin() {
    pin = null;
    lastTopic = null;
    paint(true);
  }
  function run() {
    pin = null;
    Sim.run();
    Sim.update(0.001);
    paint(true);
  }
  function resetAll() {
    Sim.replayTour();
    run();
  }
  function closeAbout() {
    if (el('about').hidden) return;
    el('about').hidden = true;
    if (aboutWasPlaying) Sim.play();
    if (aboutFocus) aboutFocus.focus();
  }
  function init() {
    el('btn-play').onclick = function () {
      if (Sim.state.finished) run();
      else Sim.toggle();
      paint(true);
    };
    el('btn-step').onclick = function () {
      pin = null;
      Sim.step();
    };
    el('btn-run').onclick = run;
    el('btn-reset').onclick = resetAll;
    el('btn-live').onclick = function () {
      unpin();
      Sim.play();
      el('follow').checked = true;
      el('follow').dispatchEvent(new Event('change'));
    };
    el('btn-panel').onclick = function () {
      var hidden = el('inspector').classList.toggle('hidden');
      this.setAttribute('aria-expanded', String(!hidden));
      this.textContent = hidden ? 'Show panel' : 'Hide panel';
    };
    el('btn-tune').onclick = function () {
      var open = el('dock').classList.toggle('tune-open');
      this.setAttribute('aria-expanded', String(open));
    };
    el('sheet-handle').onclick = function () {
      var open = el('inspector').classList.toggle('open');
      document.body.classList.toggle('details-open', open);
      this.setAttribute('aria-expanded', String(open));
      if (open) {
        sheetWasPlaying = !Sim.state.paused;
        Sim.pause();
      } else if (sheetWasPlaying) Sim.play();
      paint(true);
    };
    el('operation').onchange = function () {
      Sim.state.input.mode = this.value;
      run();
    };
    [
      ['amount', 'amount'],
      ['rival-amount', 'rivalAmount'],
      ['rounds', 'rounds']
    ].forEach(function (pair) {
      el(pair[0]).oninput = function () {
        Sim.state.input[pair[1]] = Number(this.value);
        paint(true);
      };
    });
    el('speed').oninput = function () {
      Sim.state.speed = Number(this.value);
      paint(true);
    };
    el('same-cell').onchange = function () {
      Sim.state.input.sameCell = this.checked;
      paint(true);
    };
    el('labels').onchange = function () {
      Renderer.setLabels(this.checked);
    };
    el('btn-about').onclick = function () {
      aboutWasPlaying = !Sim.state.paused;
      aboutFocus = document.activeElement;
      Sim.pause();
      el('about').hidden = false;
      el('about-close').focus();
    };
    el('about-close').onclick = closeAbout;
    el('about').onclick = function (e) {
      if (e.target === this) closeAbout();
    };
    el('about').onkeydown = function (e) {
      if (e.key === 'Escape') {
        e.preventDefault();
        e.stopPropagation();
        closeAbout();
      }
      if (e.key === 'Tab') {
        var nodes = this.querySelectorAll('button,a[href]'),
          first = nodes[0],
          last = nodes[nodes.length - 1];
        if (e.shiftKey && document.activeElement === first) {
          e.preventDefault();
          last.focus();
        }
        if (!e.shiftKey && document.activeElement === last) {
          e.preventDefault();
          first.focus();
        }
      }
    };
    World.districts.forEach(function (d) {
      var button = document.createElement('button');
      button.textContent = d.name;
      button.dataset.district = d.id;
      button.title = 'Read about ' + d.name;
      button.onclick = function () {
        showDistrict(d, true);
      };
      el('district-chips').appendChild(button);
    });
    Sim.on(function (event) {
      if (event === 'station') {
        pin = null;
        lastTopic = null;
      }
      paint(true);
    });
  }
  function change(tx, cell) {
    if (!tx) return '—';
    if (Object.prototype.hasOwnProperty.call(tx.writes, cell))
      return String(tx.writes[cell]);
    if (Object.prototype.hasOwnProperty.call(tx.merges, cell))
      return 'add(' + tx.merges[cell] + ')';
    return '—';
  }
  function interpretation(m) {
    if (!m.tx) return 'A is about to open a snapshot.';
    if (Sim.state.finished)
      return (
        'Gold is ' +
        m.world.values.gold +
        ' after ' +
        m.attempts +
        ' attempt' +
        (m.attempts === 1 ? '' : 's') +
        '. ' +
        m.conflicts +
        ' rejected; ' +
        m.world.output.length +
        ' receipt delivered.'
      );
    if (m.result && !m.result.ok)
      return (
        'Rejected: B wrote ' +
        m.result.conflict.cell +
        ' after A’s base. The world keeps B’s commit; A’s buffers are empty.'
      );
    if (m.result && m.result.ok)
      return (
        'Accepted: gold is ' +
        m.world.values.gold +
        '. ' +
        (m.world.output.length
          ? 'The receipt has been delivered.'
          : 'The receipt is still buffered until Delivery Office.')
      );
    if (m.rival && m.rival.version > m.tx.base) {
      if (m.tx.reads[m.rival.cell])
        return 'A’s gold dependency is stale. Its proposal will be rejected at the committer.';
      return (
        'World v' +
        m.world.version +
        ' is newer, but B has not invalidated any cell A read.'
      );
    }
    if (m.mode === 'merge' && Object.keys(m.tx.merges).length)
      return (
        'add(' +
        m.amount +
        ') has no fixed result yet. The committer will apply it to current gold.'
      );
    if (change(m.tx, 'gold') !== '—')
      return (
        'A sees its private ' +
        change(m.tx, 'gold') +
        '; everyone reading the committed world still sees ' +
        m.world.values.gold +
        '.'
      );
    return (
      'A’s fixed snapshot holds ' +
      m.tx.snapshot.gold +
      ' gold; its base is v' +
      m.tx.base +
      '.'
    );
  }
  function paint(force) {
    var now = Date.now();
    if (!force && now - lastPaint < 100) return;
    lastPaint = now;
    var s = Sim.state,
      m = s.model;
    if (!m) return;
    var id = activeDistrict(),
      d = World.districtById[id] || World.districtById.snapshot,
      tx = m.tx,
      input = s.input;
    if (lastTopic !== id) {
      text('stage-chip', ('0' + d.number).slice(-2) + ' / 10');
      text('stage-tag', d.tag);
      text('stage-name', d.name);
      text('stage-short', d.short);
      text('stage-body', d.body);
      el('stage-source').href = '../../src/interpreter/stm/' + d.source;
      text('stage-source', 'Driver source · ' + d.source + ' ↗');
      lastTopic = id;
      document.querySelectorAll('[data-district]').forEach(function (b) {
        b.classList.toggle('on', b.dataset.district === id);
        b.setAttribute('aria-pressed', String(b.dataset.district === id));
      });
    }
    el('btn-live').hidden = !pin;
    text('hud-attempt', m.attempts || '—');
    text('hud-world', 'v' + m.world.version);
    text('hud-conflicts', m.conflicts);
    text('hud-output', m.world.output.length);
    text(
      'hud-note',
      s.finished
        ? 'Run complete · change the operation to compare outcomes.'
        : s.paused
          ? 'Paused · take your time, or advance one station.'
          : s.reading
            ? 'Reading stop · Space holds here; ⇥ moves on.'
            : s.fastForward
              ? 'Retrying · familiar stops are shorter; new topics keep their reading time.'
              : 'Following A · drag the map to explore.'
    );
    text('play-glyph', s.finished ? '↻' : s.paused ? '▶' : '❚❚');
    el('btn-play').setAttribute(
      'aria-label',
      s.finished ? 'Run again' : s.paused ? 'Play tour' : 'Pause tour'
    );
    el('dwell').hidden = !s.dwellTotal || !!pin;
    el('dwell-bar').style.width =
      (s.dwellTotal ? (100 * s.dwellLeft) / s.dwellTotal : 0) + '%';
    text(
      'dwell-hint',
      s.paused
        ? 'Paused at this stop · press Space to continue'
        : Math.ceil(s.dwellLeft / s.speed) + 's at this speed · next stop ⇥'
    );
    text('attempt-status', tx ? tx.status : 'waiting');
    text('snapshot-heading', tx ? 'Base v' + tx.base : 'Snapshot');
    text('world-heading', 'World v' + m.world.version);
    ['gold', 'bells'].forEach(function (cell) {
      text('snapshot-' + cell, tx ? tx.snapshot[cell] : '—');
      text('private-' + cell, change(tx, cell));
      text('world-' + cell, m.world.values[cell]);
    });
    text(
      'read-set',
      tx && Object.keys(tx.reads).length
        ? Object.keys(tx.reads).join(', ')
        : '∅'
    );
    text('interpretation', interpretation(m));
    var evidence =
      'Blue = tracked read. Gold = pending write or merge. Violet = buffered effect.';
    if (m.result && !m.result.ok) {
      var c = m.result.conflict;
      evidence =
        'Rejected submission: reads {' +
        m.lastSubmission.reads.join(', ') +
        '}; B wrote ' +
        c.cell +
        ' at v' +
        c.writtenAt +
        ' > base v' +
        c.base +
        '.';
    } else if (m.result && m.result.ok)
      evidence =
        m.conflicts +
        ' rejected attempt(s) discarded ' +
        m.discardedEffects +
        ' buffered receipt(s).';
    text('validation', evidence);
    text('pending-output', (tx ? tx.effects.length : 0) + ' pending');
    text(
      'output',
      m.world.output.length
        ? m.world.output
            .map(function (o) {
              return '“' + o.message + '”';
            })
            .join('\n')
        : 'No receipt has left the attempt.'
    );
    text('v-amount', input.amount);
    text('v-rival', input.rivalAmount);
    text('v-rounds', input.rounds);
    text('v-speed', s.speed.toFixed(1) + '×');
    text(
      'control-note',
      'Upcoming work: A ' +
        (input.mode === 'blind'
          ? 'sets ' + input.amount
          : 'adds ' + input.amount) +
        '; B adds ' +
        input.rivalAmount +
        ' to ' +
        (input.sameCell ? 'gold' : 'bells') +
        '. Parameters affect upcoming operations; changing operation starts a new run.'
    );
  }
  global.UI = {
    init: init,
    run: run,
    resetAll: resetAll,
    paint: paint,
    showDistrict: showDistrict,
    unpin: unpin,
    activeDistrict: activeDistrict,
    takeFlyTo: function () {
      var p = fly;
      fly = null;
      return p;
    },
    closeAbout: closeAbout
  };
})(window);
