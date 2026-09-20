(function (global) {
  'use strict';
  var GC = global.GC, World = global.World, Iso = global.Iso;
  var tour = { seen: Object.create(null), done: false };
  var state = { running: false, paused: true, finished: false, station: null, stationT: 0,
    stepMode: false, speed: 1, options: { linked: true, callback: 'none', pins: 0, cycles: 1 },
    model: null, laps: 0, reading: false, dwellLeft: 0, dwellTotal: 0, fastForward: false, tourDone: false };
  var van = { routeName: 'approach', dist: 0, dwell: 0, stationIdx: 0 };
  var listeners = [];
  function emit(name, value) { listeners.forEach(function (fn) { fn(name, value); }); }
  function reset() {
    state.model = GC.create(state.options);
    state.running = false; state.paused = true; state.finished = false; state.stepMode = false;
    state.station = null; state.stationT = 0; state.laps = 0;
    state.reading = false; state.dwellLeft = 0; state.dwellTotal = 0;
    state.fastForward = false; state.tourDone = tour.done;
    van.routeName = 'approach'; van.dist = 0; van.dwell = 0; van.stationIdx = 0;
    emit('reset');
  }
  function run() { reset(); state.running = true; state.paused = false; }
  var OPS = {
    request: function () { GC.roots(state.model); },
    gate: function () { GC.admit(state.model); },
    roots: function () { GC.seed(state.model); },
    take: function () { GC.take(state.model); state.laps++; state.fastForward = state.laps > 1; },
    trace: function () { GC.trace(state.model); },
    sweep: function () { GC.sweep(state.model); },
    report: function () { GC.report(state.model); },
    refuse: function () { GC.report(state.model); }
  };
  function fire(st) {
    state.station = st.id; state.stationT = 0;
    OPS[st.id]();
    emit('station', st.id);
  }
  function advanceRoute() {
    var next;
    if (van.routeName === 'approach') next = state.model.admitted ? 'admit' : 'refuse';
    else if (van.routeName === 'admit') next = state.model.stack.length ? 'trace' : 'sweep';
    else if (van.routeName === 'trace') next = state.model.stack.length ? 'trace' : 'sweep';
    else {
      state.finished = true; state.paused = true; state.station = 'done';
      state.stepMode = false; state.reading = false; state.dwellLeft = 0; state.dwellTotal = 0;
      tour.done = World.districts.every(function (d) { return !!tour.seen[d.id]; });
      state.tourDone = tour.done;
      emit('station', 'done');
      return;
    }
    van.routeName = next; van.dist = 0; van.stationIdx = 0;
  }
  function update(dt) {
    state.stationT += dt;
    if (!state.running || state.paused || state.finished) return;
    if (van.dwell > 0) {
      // Travel acceleration must never shorten a first reading stop.
      van.dwell = Math.max(0, van.dwell - dt * state.speed);
      state.dwellLeft = van.dwell;
      if (!van.dwell) { state.reading = false; state.dwellTotal = 0; }
      return;
    }
    var boost = (state.fastForward ? 2.4 : 1) * (state.tourDone ? 3 : 1);
    var route = World.routes[van.routeName], stations = World.stations[van.routeName];
    van.dist += 6 * dt * state.speed * boost;
    if (van.stationIdx < stations.length) {
      var st = stations[van.stationIdx];
      if (van.dist >= st.dist) {
        van.dist = st.dist; van.stationIdx++;
        var topic = World.stationToDistrict[st.id], first = !tour.seen[topic];
        fire(st); tour.seen[topic] = true;
        van.dwell = first ? World.readSeconds(st.id) : st.dwell / (state.fastForward ? 2.2 : 1);
        state.reading = first; state.dwellTotal = van.dwell; state.dwellLeft = van.dwell;
        if (state.stepMode) { state.paused = true; state.stepMode = false; }
        return;
      }
    }
    if (van.dist >= route.total) advanceRoute();
  }
  global.Sim = { state: state, van: van, run: run, reset: reset, update: update,
    vanPosition: function () { return Iso.smoothAt(World.routes[van.routeName], van.dist, 0.8); },
    replayTour: function () { tour.seen = Object.create(null); tour.done = false; },
    on: function (fn) { listeners.push(fn); },
    play: function () { if (!state.finished) { state.paused = false; state.running = true; } },
    pause: function () { state.paused = true; },
    toggle: function () { if (state.paused) this.play(); else this.pause(); },
    step: function () {
      if (state.finished) return;
      state.running = true; state.stepMode = true; state.paused = false;
      van.dwell = 0; state.reading = false; state.dwellLeft = 0; state.dwellTotal = 0;
    }
  };
})(window);
