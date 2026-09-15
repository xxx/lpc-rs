(function (global) {
  'use strict';
  var World = global.World,
    Iso = global.Iso,
    Txn = global.Txn;
  var BASE_SPEED = 4;
  // Reading history survives a new run until the reader explicitly replays the tour.
  var tour = { seen: Object.create(null), done: false };
  var state = {
    running: false,
    paused: true,
    finished: false,
    station: null,
    stationT: 0,
    stepMode: false,
    speed: 1,
    reading: false,
    dwellLeft: 0,
    dwellTotal: 0,
    fastForward: false,
    tourDone: false,
    model: null,
    input: {
      mode: 'read',
      amount: 4,
      rivalAmount: 3,
      rounds: 1,
      sameCell: true
    }
  };
  var van = { routeName: 'out', dist: 0, dwell: 0, stationIdx: 0 };
  var listeners = [];
  function emit(name, payload) {
    listeners.forEach(function (fn) {
      fn(name, payload);
    });
  }
  function reset() {
    state.model = Txn.lesson();
    state.running = true;
    state.paused = false;
    state.finished = false;
    state.station = null;
    state.stationT = 0;
    state.stepMode = false;
    state.reading = false;
    state.dwellLeft = state.dwellTotal = 0;
    state.fastForward = false;
    state.tourDone = tour.done;
    van.routeName = 'out';
    van.dist = van.dwell = van.stationIdx = 0;
    emit('reset');
  }
  var OPS = {};
  Object.keys(Txn.operations).forEach(function (id) {
    OPS[id] = function () {
      Txn.operations[id](state.model, state.input);
    };
  });

  /* ---- update ------------------------------------------------------------ */

  function routeOf(name) {
    return World.routes[name];
  }

  /* Once every district has been explained there is nothing left to read, so
     the remaining trips run at a watchable pace instead of a readable one. */
  function travelBoost() {
    return (state.fastForward ? 2.4 : 1) * (state.tourDone ? 3.0 : 1);
  }
  function dwellBoost() {
    /* Stops stay generous even after the tour, because their numbers change. */
    return (state.fastForward ? 2.2 : 1) * (state.tourDone ? 1.4 : 1);
  }

  function fire(st) {
    state.station = st.id;
    state.stationT = 0;
    var op = OPS[st.id];
    if (op) op();
    emit('station', st.id);
  }

  function advanceRoute() {
    if (van.routeName === 'out') {
      van.routeName = state.model.result.ok ? 'success' : 'retry';
    } else if (van.routeName === 'retry') {
      van.routeName = 'out';
      state.fastForward = true;
    } else {
      state.finished = true;
      state.paused = true;
      emit('finished');
      return;
    }
    van.dist = 0;
    van.stationIdx = 0;
    van.dwell = 0;
  }

  function update(dt) {
    state.stationT += dt;
    if (!state.running || state.paused || state.finished) return;

    var sdt = dt * state.speed * travelBoost();

    if (van.dwell > 0) {
      /* A stop is measured in reading seconds, so only the speed slider scales
         it; the travel boosts must never cut a first read short. */
      van.dwell -= dt * state.speed;
      state.dwellLeft = Math.max(0, van.dwell);
      if (van.dwell <= 0) {
        state.reading = false;
        state.dwellTotal = 0;
      }
      return;
    }

    var route = routeOf(van.routeName);
    van.dist += BASE_SPEED * sdt;

    var sts = World.stations[van.routeName];
    if (van.stationIdx < sts.length) {
      var st = sts[van.stationIdx];
      if (van.dist >= st.dist) {
        van.dist = st.dist;
        van.stationIdx++;
        /* Keyed by district, not by station: two stations that show the same
           write-up must not charge the reader for a second read. */
        var topic = World.stationToDistrict[st.id] || st.id;
        var firstTime = !tour.seen[topic];
        fire(st);
        tour.seen[topic] = true;
        tour.done = World.districts.every(function (d) {
          return tour.seen[d.id];
        });
        state.tourDone = tour.done;
        van.dwell = firstTime
          ? World.readSeconds(st.id)
          : st.dwell / dwellBoost();
        state.reading = firstTime;
        state.dwellTotal = van.dwell;
        state.dwellLeft = van.dwell;
        if (state.stepMode) {
          state.paused = true;
          state.stepMode = false;
        }
        return;
      }
    }

    if (van.dist >= route.total) advanceRoute();
  }

  function vanPosition() {
    return Iso.smoothAt(World.routes[van.routeName], van.dist, 0.6);
  }
  global.Sim = {
    state: state,
    van: van,
    run: reset,
    reset: reset,
    update: update,
    vanPosition: vanPosition,
    on: function (fn) {
      listeners.push(fn);
    },
    replayTour: function () {
      tour.seen = Object.create(null);
      tour.done = false;
    },
    play: function () {
      if (!state.finished) {
        state.paused = false;
        state.running = true;
      }
    },
    pause: function () {
      state.paused = true;
    },
    toggle: function () {
      if (state.paused) this.play();
      else this.pause();
    },
    step: function () {
      if (state.finished) return;
      state.running = true;
      state.stepMode = true;
      state.paused = false;
      van.dwell = 0;
      state.reading = false;
      state.dwellLeft = state.dwellTotal = 0;
    }
  };
})(window);
