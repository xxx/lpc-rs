(function (global) {
  'use strict';
  var Sim = global.Sim,
    World = global.World,
    M = global.ParserModel,
    Renderer = global.Renderer;
  var pin = null,
    fly = null,
    lastPaint = 0,
    lastTopic = null,
    aboutPlaying = false,
    aboutFocus = null,
    sheetPlaying = false;
  var sources = {
    native: 'frontend/native/mod.rs',
    parser: 'frontend/parser.rs',
    dgd: 'frontend/dgd.rs',
    groups: 'grammar/builtins.rs',
    grammar: 'grammar/model.rs',
    tokens: 'grammar/tokenizer.rs',
    chart: 'grammar/earley.rs',
    trees: 'grammar/tree.rs',
    nativeOut: 'frontend/native/matcher.rs',
    parserOut: 'parser/attempt.rs',
    dgdOut: '../interpreter/efun/parse_string.rs',
    receipt: 'grammar/mod.rs',
    reject: '../interpreter/efun/parse_sentence.rs',
  };
  function el(id) {
    return document.getElementById(id);
  }
  function text(id, value) {
    var node = el(id);
    if (node.textContent !== String(value)) node.textContent = value;
  }
  function esc(value) {
    return String(value).replace(/[&<>"']/g, function (c) {
      return {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;',
      }[c];
    });
  }
  function activeDistrict() {
    return pin || Sim.state.station || Sim.state.input.family;
  }
  function showDistrict(d, move) {
    pin = d.id;
    Sim.pause();
    if (move) fly = { x: d.bx, y: d.by };
    paint(true);
  }
  function unpin() {
    pin = null;
    lastTopic = null;
    paint(true);
  }
  function follow() {
    el('follow').checked = true;
    el('follow').dispatchEvent(new Event('change'));
  }
  function run() {
    pin = null;
    lastTopic = null;
    Sim.run();
    Sim.update(0.001);
    follow();
    if (global.matchMedia('(prefers-reduced-motion: reduce)').matches)
      Sim.pause();
    paint(true);
  }
  function resetAll() {
    Sim.replayTour();
    run();
  }
  function closeAbout() {
    if (el('about').hidden) return;
    el('about').hidden = true;
    if (aboutPlaying) Sim.play();
    if (aboutFocus) aboutFocus.focus();
  }
  function inputChange() {
    Sim.state.input.line = el('sentence').value;
    run();
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
    el('btn-run').onclick = inputChange;
    el('btn-reset').onclick = resetAll;
    el('btn-live').onclick = function () {
      unpin();
      Sim.play();
      follow();
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
        sheetPlaying = !Sim.state.paused;
        Sim.pause();
      } else if (sheetPlaying) Sim.play();
      paint(true);
    };
    document.querySelectorAll('[data-family]').forEach(function (button) {
      button.onclick = function () {
        Sim.state.input.family = this.dataset.family;
        inputChange();
      };
    });
    el('lesson').onchange = function () {
      Sim.state.input.lesson = this.value;
      el('sentence').value =
        this.value === 'give' ? 'give red sword to bob' : 'say hello there';
      inputChange();
    };
    el('sentence-form').onsubmit = function (event) {
      event.preventDefault();
      inputChange();
    };
    ['sword', 'allow'].forEach(function (id) {
      el(id).onchange = function () {
        Sim.state.input[id] = this.checked;
        inputChange();
      };
    });
    el('speed').oninput = function () {
      Sim.state.speed = Number(this.value);
      paint(true);
    };
    el('labels').onchange = function () {
      Renderer.setLabels(this.checked);
    };
    el('btn-about').onclick = function () {
      aboutPlaying = !Sim.state.paused;
      aboutFocus = document.activeElement;
      Sim.pause();
      el('about').hidden = false;
      el('about-close').focus();
    };
    el('about-close').onclick = closeAbout;
    el('about').onclick = function (event) {
      if (event.target === this) closeAbout();
    };
    el('about').onkeydown = function (event) {
      if (event.key === 'Escape') {
        event.preventDefault();
        event.stopPropagation();
        closeAbout();
      }
      if (event.key === 'Tab') {
        var nodes = this.querySelectorAll('button,a[href]'),
          first = nodes[0],
          last = nodes[nodes.length - 1];
        if (event.shiftKey && document.activeElement === first) {
          event.preventDefault();
          last.focus();
        }
        if (!event.shiftKey && document.activeElement === last) {
          event.preventDefault();
          first.focus();
        }
      }
    };
    World.districts.forEach(function (d) {
      var b = document.createElement('button');
      b.textContent = d.name;
      b.dataset.district = d.id;
      b.onclick = function () {
        showDistrict(d, true);
      };
      el('district-chips').appendChild(b);
    });
    Sim.on(function (event) {
      if (event === 'station') {
        pin = null;
        lastTopic = null;
      }
      paint(true);
    });
  }
  function grammarText(g) {
    return g.rules
      .map(function (p) {
        return (
          p.lhs +
          ' → ' +
          (p.rhs.length
            ? p.rhs
                .map(function (s) {
                  return (
                    (s.type === 'lit' ? "'" + s.value + "'" : s.value) +
                    (s.label != null ? ' ⟨' + s.label + '⟩' : '')
                  );
                })
                .join(' ')
            : 'ε')
        );
      })
      .join('\n');
  }
  function treeText(m) {
    if (!m.derivations.length)
      return m.error || 'Waiting for Derivation Grove.';
    var lines = [];
    function walk(node, depth) {
      var name =
        node.token != null
          ? JSON.stringify(m.scan.tokens[node.token].text)
          : m.g.rules[node.prod].lhs;
      lines.push(
        new Array(depth + 1).join('  ') +
          name +
          ' [' +
          node.start +
          ',' +
          node.end +
          ')'
      );
      if (node.children)
        node.children.forEach(function (child) {
          walk(child, depth + 1);
        });
    }
    walk(m.derivations[0], 0);
    return (
      lines.join('\n') +
      '\n\n' +
      (m.captures.length
        ? m.captures
            .map(function (c) {
              return (
                '⟨' + c.slot + '⟩ ' + c.kind + ' = ' + JSON.stringify(c.text)
              );
            })
            .join('\n')
        : 'No capture labels in the DGD grammar.')
    );
  }
  function interpretation(m) {
    if (m.error) return (m.recognized ? 'Syntax succeeded. ' : '') + m.error;
    if (m.output !== null)
      return m.input.family === 'dgd'
        ? 'These are strings from the tree. Scope did not participate.'
        : m.input.family === 'parser'
          ? 'Object selection and the all-filled rechecks finished before do_ ran.'
          : 'The handler received resolved objects or captured text.';
    if (m.derivations.length)
      return 'Recognition succeeded. The frontend still needs to interpret the derivation.';
    if (m.chart)
      return 'The chart knows token classes and productions. It has no knowledge of objects or permissions.';
    return 'The frontend defines the language and keeps its result contract outside the shared engine.';
  }
  function paint(force) {
    var now = Date.now();
    if (!force && now - lastPaint < 100) return;
    lastPaint = now;
    var s = Sim.state,
      m = s.model;
    if (!m) return;
    var id = activeDistrict(),
      d = World.byId[id],
      c = m.chart;
    if (lastTopic !== id) {
      text(
        'stage-chip',
        ['native', 'parser', 'dgd'].indexOf(id) >= 0
          ? 'Entrance'
          : id === 'reject'
            ? 'Return'
            : id === 'receipt'
              ? 'Result'
              : 'Station'
      );
      text('stage-tag', d.tag);
      text('stage-name', d.name);
      text('stage-short', d.short);
      text('stage-body', d.body);
      el('stage-source').href = '../../src/command/' + sources[id];
      el('stage-source').textContent =
        'Driver source · ' + sources[id].split('/').slice(-2).join('/') + ' ↗';
      el('inspector').style.setProperty('--accent', d.color);
      lastTopic = id;
    }
    document.querySelectorAll('[data-family]').forEach(function (b) {
      b.setAttribute(
        'aria-pressed',
        String(b.dataset.family === s.input.family)
      );
    });
    document.querySelectorAll('[data-district]').forEach(function (b) {
      b.classList.toggle('on', b.dataset.district === id);
    });
    el('btn-live').hidden = !pin;
    el('btn-play').textContent = s.paused ? '▶' : '❚❚';
    el('btn-play').setAttribute('aria-label', s.paused ? 'Play' : 'Pause');
    text('v-speed', s.speed.toFixed(1) + '×');
    text('hud-tokens', m.scan ? m.scan.tokens.length : '—');
    text(
      'hud-column',
      c ? (c.column < 0 ? 'seeded' : c.column + ' / ' + c.tokens.length) : '—'
    );
    text(
      'hud-items',
      c
        ? c.sets.reduce(function (sum, set) {
            return sum + set.length;
          }, 0)
        : '—'
    );
    text(
      'hud-note',
      s.finished
        ? m.error
          ? 'Return road: ' + m.error
          : 'Run complete. Try the same sentence through another entrance.'
        : s.paused
          ? 'Paused · Next advances one station; Play resumes.'
          : s.reading
            ? 'Reading stop · Space holds here; Next skips ahead.'
            : s.fastForward
              ? 'Circling to the next chart column; the repeated explanation takes a short stop.'
              : 'Following the cart · each station performs a real model operation.'
    );
    el('dwell').hidden = !!pin || !s.dwellTotal || s.finished;
    el('dwell-bar').style.width =
      (s.dwellTotal ? (100 * s.dwellLeft) / s.dwellTotal : 0) + '%';
    text(
      'dwell-hint',
      s.paused
        ? 'Held here · Play resumes'
        : s.reading
          ? 'Reading stop · Space holds here'
          : 'Repeat visit · continuing shortly'
    );
    text('family-name', M.names[s.input.family]);
    text('rule-text', m.spec.display);
    text(
      'input-text',
      'Line: ' +
        JSON.stringify(m.input.line) +
        (m.input.family === 'parser' && m.scan
          ? ' · Engine input: ' + JSON.stringify(m.engineInput)
          : '')
    );
    el('token-list').innerHTML = m.scan
      ? m.scan.tokens
          .map(function (t, i) {
            return (
              '<span class="token' +
              (c && i <= c.column && c.sets[i + 1].length ? ' read' : '') +
              '">' +
              esc(t.text) +
              '<small>' +
              esc(t.className) +
              ' · [' +
              t.start +
              ',' +
              t.end +
              ')</small></span>'
            );
          })
          .join('')
      : '';
    text(
      'token-note',
      m.scan
        ? m.scan.tokens.length +
            ' retained tokens · ' +
            m.scan.skipped.length +
            ' whitespace spans skipped. Offsets use JavaScript characters.'
        : 'Token spans appear at Token Works.'
    );
    if (c) {
      var max = Math.max.apply(
        null,
        c.sets
          .map(function (set) {
            return set.length;
          })
          .concat([1])
      );
      el('chart-bars').innerHTML = c.sets
        .map(function (set, i) {
          return (
            '<div class="chart-bar' +
            (i === c.column ? ' on' : '') +
            '" title="Column ' +
            i +
            ': ' +
            set.length +
            ' items"><span>' +
            set.length +
            '</span><i style="height:' +
            Math.round((48 * set.length) / max) +
            'px"></i><span>' +
            i +
            '</span></div>'
          );
        })
        .join('');
      text(
        'chart-note',
        'Unique additions: ' +
          c.counts.predict +
          ' predict · ' +
          c.counts.scan +
          ' scan · ' +
          c.counts.complete +
          ' complete · ' +
          c.counts.nullable +
          ' nullable. Column ' +
          Math.max(0, c.column) +
          ' of ' +
          c.tokens.length +
          '; seeded items are included in the total.'
      );
      text(
        'chart-text',
        c.column < 0
          ? 'Seeded; no column closed yet.'
          : c.sets[c.column]
              .map(function (item) {
                return M.itemText(m.g, item);
              })
              .join('\n') || '(empty column)'
      );
    } else {
      el('chart-bars').innerHTML = '';
      text('chart-note', 'A chart has one column for every token boundary.');
      text('chart-text', 'Waiting for Earley Yard.');
    }
    text('rule-count', m.g ? '(' + m.g.rules.length + ')' : '');
    text(
      'grammar-text',
      m.g ? grammarText(m.g) : 'Waiting for Grammar Foundry.'
    );
    text('tree-text', treeText(m));
    text('interpretation', interpretation(m));
    text(
      'call-text',
      m.calls.length
        ? m.calls.join('\n')
        : 'No frontend handler has been called.'
    );
    var result = 'Result pending.';
    if (m.error) {
      result =
        m.stopKind === 'limit'
          ? 'Teaching model limit; no driver result inferred.'
          : m.input.family === 'dgd'
            ? 'parse_string → 0'
            : m.input.family === 'native'
              ? 'This rule did not handle the line.'
              : !m.chart
                ? 'parse_sentence → 0 (unknown verb)'
                : !m.recognized
                  ? 'parse_sentence → -1 (syntax)'
                  : m.failureKind === 'noun'
                    ? 'parse_sentence → -3 (noun)'
                    : 'parse_sentence → -2 (handler)';
    } else if (m.output !== null)
      result =
        m.input.family === 'native'
          ? 'Handler arguments: ' + m.output.map(M.format).join(', ')
          : m.input.family === 'parser'
            ? 'parse_sentence → 1 (do_ ran)'
            : 'parse_string → ' + JSON.stringify(m.output);
    text('result-text', result);
    el('result-text').classList.toggle('error', !!m.error);
    var resultSection = el('result-section');
    var following = el(
      m.calls.length || m.error ? 'entrance-section' : 'stations-section'
    );
    if (resultSection.nextElementSibling !== following)
      el('inspector').insertBefore(resultSection, following);
  }
  global.UI = {
    init: init,
    run: run,
    resetAll: resetAll,
    showDistrict: showDistrict,
    unpin: unpin,
    paint: paint,
    activeDistrict: activeDistrict,
    closeAbout: closeAbout,
    takeFlyTo: function () {
      var p = fly;
      fly = null;
      return p;
    },
  };
})(window);
