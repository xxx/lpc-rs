(function (global) {
  'use strict';
  var Sim = global.Sim, World = global.World, GC = global.GC;
  var pinned = null, flyTo = null, lastPaint = 0, cache = {}, aboutWasPlaying = false, focusBeforeAbout = null, pinWasPlaying = false;
  function el(id) { return document.getElementById(id); }
  function text(id, value) { value = String(value); if (cache[id] !== value) { el(id).textContent = value; cache[id] = value; } }
  function html(id, value) { if (cache[id] !== value) { el(id).innerHTML = value; cache[id] = value; } }
  function escape(value) { return String(value).replace(/[&<>"']/g, function (c) { return { '&':'&amp;', '<':'&lt;', '>':'&gt;', '"':'&quot;', "'":'&#39;' }[c]; }); }
  function active() { return pinned || (Sim.state.station === 'done' ? (Sim.state.model.admitted ? 'report' : 'refuse') : Sim.state.station) || 'request'; }
  function interpretation(m) {
    var c = GC.counts(m);
    if (m.admitted === false) return 'Refused: ' + m.options.pins + ' live snapshot pin(s). All ' + c.before + ' cells remain unchanged.';
    if (m.swept) return c.marked + ' marked + ' + c.protected + ' outside the removal filter = ' + c.remaining + ' remaining cells. ' + c.reclaimed + ' reclaimed.';
    if (!m.seeded) return 'The world has ' + c.before + ' cells. Reclamation starts only after admission and a complete trace.';
    return c.marked + ' cells reached so far. ' + c.work + ' work item(s) still need tracing; no cell has been removed.';
  }
  function paint(force) {
    var now = Date.now(); if (!force && now - lastPaint < 90) return; lastPaint = now;
    var s = Sim.state, m = s.model; if (!m) return;
    var c = GC.counts(m), d = World.byId[active()];
    text('chapter-tag', s.finished && !pinned ? (m.admitted ? 'THE COLLECTION RECEIPT' : 'REQUEST REFUSED') : d.tag.toUpperCase());
    text('district-title', s.finished && !pinned ? (m.admitted ? c.reclaimed + ' cells reclaimed.' : 'The world stays intact.') : d.name);
    text('district-short', d.short); text('district-body', d.body);
    text('hud-stage', s.finished ? (m.admitted ? 'Collection complete' : 'Collection refused') : (World.byId[s.station] || World.byId.request).name);
    text('count-work', c.work); text('count-marked', c.marked); text('count-freed', c.reclaimed); text('count-world', c.remaining);
    var mode = s.finished ? 'Change the world to compare another collection.' : s.paused ? 'Paused · Space to continue; S for one stop.' : s.reading ? 'Reading stop · Space holds here; S skips ahead.' : s.fastForward ? (Sim.van.routeName === 'trace' ? 'Repeat laps at 2.4× travel · same algorithm, new work.' : 'Following the result · faster travel between stops.') : 'Following one request · time stretched for reading.';
    text('hud-note', mode);
    text('btn-play', s.paused ? 'Play' : 'Pause'); el('btn-play').setAttribute('aria-label', s.paused ? 'Play tour' : 'Pause tour');
    el('btn-play').disabled = s.finished; el('btn-step').disabled = s.finished;
    text('reading-note', pinned ? 'Browsing this station · the live collection is paused.' : s.finished ? 'The result counts world cells, not bytes.' : s.reading ? (s.paused ? 'Reading stop held. ' : Math.ceil(s.dwellLeft / s.speed) + ' seconds to read. ') + 'Space holds · S continues.' : 'Next stop advances one operation, then pauses.');
    var progress = s.dwellTotal ? Math.max(0, s.dwellLeft / s.dwellTotal) : 0;
    el('reading-fill').style.transform = 'scaleX(' + progress + ')';
    el('reading-fill').parentNode.setAttribute('aria-valuenow', Math.round(progress * 100));
    text('live-message', m.message);
    text('stack-count', c.work + ' pending');
    var work = [];
    if (m.current) work.push('<span class="work-item selected">selected: ' + escape(GC.describe(m.current)) + '</span>');
    m.stack.slice().reverse().forEach(function (item, i) { work.push('<span class="work-item">' + (i === 0 ? 'top: ' : '') + escape(GC.describe(item)) + '</span>'); });
    html('work-stack', work.length ? work.join('') : '<span class="fine">' + (m.seeded ? 'Empty: no pending references.' : 'Not seeded yet.') + '</span>');
    text('world-count', c.remaining + ' / ' + c.before + ' remain');
    var current = m.current || (s.station === 'trace' && m.last && m.last.item);
    html('cell-list', m.order.map(function (id, index) {
      var cell = m.initial[id], status = GC.status(m, id), selected = current && current.kind === 'Var' && current.id === id;
      return '<div class="cell ' + status + (selected ? ' current' : '') + '" title="' + escape(id + ': ' + status) + '"><small>' + (index + 1) + '</small><div><b>' + escape(cell.label) + '</b><span class="cell-kind">' + cell.kind + ' · ' + status + '</span></div></div>';
    }).join(''));
    text('interpretation', interpretation(m));
    el('btn-unpin').hidden = !pinned;
  }
  function run() {
    pinned = null; Sim.run();
    if (global.matchMedia('(prefers-reduced-motion: reduce)').matches) Sim.pause();
    paint(true);
  }
  function resetAll() { Sim.replayTour(); run(); }
  function showDistrict(d, move) {
    if (!pinned) pinWasPlaying = !Sim.state.paused;
    pinned = d.id; Sim.pause();
    if (move) flyTo = d;
    el('inspector').classList.remove('hidden');
    if (global.innerWidth <= 900 && global.innerHeight > global.innerWidth) {
      el('inspector').classList.add('expanded'); el('sheet-toggle').setAttribute('aria-expanded', 'true');
    }
    el('inspector').querySelector('.inspector-content').scrollTop = 0;
    text('btn-panel', 'Hide notes'); paint(true);
  }
  function unpin() { pinned = null; if (pinWasPlaying) Sim.play(); pinWasPlaying = false; paint(true); }
  function openAbout() {
    focusBeforeAbout = document.activeElement; aboutWasPlaying = !Sim.state.paused;
    Sim.pause(); el('about').hidden = false; el('about-close').focus(); paint(true);
  }
  function closeAbout() {
    if (el('about').hidden) return;
    el('about').hidden = true; if (aboutWasPlaying) Sim.play();
    if (focusBeforeAbout) focusBeforeAbout.focus(); paint(true);
  }
  function newWorld() {
    Sim.state.options = { linked: el('linked').checked, callback: el('callback').value,
      pins: Number(el('pins').value), cycles: Number(el('cycles').value) };
    text('pins-value', el('pins').value); text('cycles-value', el('cycles').value);
    var wasPaused = Sim.state.paused && !Sim.state.finished;
    run();
    if (wasPaused || el('dock').classList.contains('settings-open')) Sim.pause();
    paint(true);
  }
  function init() {
    el('btn-play').addEventListener('click', function () { pinned = null; Sim.toggle(); paint(true); });
    el('btn-step').addEventListener('click', function () { pinned = null; Sim.step(); paint(true); });
    el('btn-run').addEventListener('click', run); el('btn-reset').addEventListener('click', resetAll);
    el('btn-unpin').addEventListener('click', unpin);
    el('speed').addEventListener('input', function () { Sim.state.speed = Number(this.value); text('speed-value', this.value + '×'); paint(true); });
    ['linked', 'callback'].forEach(function (id) { el(id).addEventListener('change', newWorld); });
    ['pins', 'cycles'].forEach(function (id) { el(id).addEventListener('input', newWorld); });
    el('labels').addEventListener('change', function () { global.Renderer.setLabels(this.checked); });
    el('btn-panel').addEventListener('click', function () { var hidden = el('inspector').classList.toggle('hidden'); text('btn-panel', hidden ? 'Show notes' : 'Hide notes'); });
    el('sheet-toggle').addEventListener('click', function () {
      var expanded = el('inspector').classList.toggle('expanded'); this.setAttribute('aria-expanded', String(expanded));
      if (expanded) Sim.pause(); paint(true);
    });
    el('btn-settings').addEventListener('click', function () {
      var open = el('dock').classList.toggle('settings-open'); this.setAttribute('aria-expanded', String(open));
      if (open) Sim.pause(); paint(true);
    });
    el('btn-about').addEventListener('click', openAbout); el('about-close').addEventListener('click', closeAbout);
    el('about').addEventListener('click', function (e) { if (e.target === this) closeAbout(); });
    el('about').addEventListener('keydown', function (e) {
      if (e.key === 'Escape') { e.preventDefault(); e.stopPropagation(); closeAbout(); }
      if (e.key !== 'Tab') return;
      var nodes = this.querySelectorAll('button, a[href], [tabindex="0"]');
      var first = nodes[0], last = nodes[nodes.length - 1];
      if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
      else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
    });
    html('station-nav', World.districts.map(function (d) { return '<button data-station="' + d.id + '" title="Pause and read about ' + d.name + '">' + d.name + '</button>'; }).join(''));
    el('station-nav').addEventListener('click', function (e) { var id = e.target.getAttribute('data-station'); if (id) showDistrict(World.byId[id], true); });
    Sim.on(function () { paint(true); });
  }
  global.UI = { init: init, paint: paint, run: run, resetAll: resetAll, showDistrict: showDistrict,
    unpin: unpin, activeDistrict: active, closeAbout: closeAbout,
    takeFlyTo: function () { var target = flyTo; flyTo = null; return target; } };
})(window);
