(function (global) {
  'use strict';
  var Model = global.ParserModel,
    World = global.World,
    Iso = global.Iso;
  var BASE_SPEED = 6;
  var tour = { seen: Object.create(null), done: false };
  var state = {
    running: false,
    paused: true,
    finished: false,
    station: null,
    stationT: 0,
    stepMode: false,
    speed: 1,
    input: {
      family: 'native',
      lesson: 'give',
      line: 'give red sword to bob',
      sword: true,
      allow: true,
    },
    model: null,
    reading: false,
    dwellLeft: 0,
    dwellTotal: 0,
    fastForward: false,
    tourDone: false,
  };
  var van = { routeName: 'native', dist: 0, dwell: 0, stationIdx: 0 },
    listeners = [];
  function emit(name, payload) {
    listeners.forEach(function (fn) {
      fn(name, payload);
    });
  }
  function routeOf(name) {
    return World.routes[name];
  }
  function useRoute(name) {
    van.routeName = name;
    van.dist = 0;
    van.stationIdx = 0;
    van.dwell = 0;
  }
  function reset() {
    state.model = Model.create(state.input);
    state.finished = false;
    state.station = null;
    state.stationT = 0;
    state.stepMode = false;
    state.reading = false;
    state.dwellLeft = 0;
    state.dwellTotal = 0;
    state.fastForward = false;
    state.tourDone = tour.done;
    useRoute(state.input.family);
  }
  function run() {
    reset();
    state.running = true;
    state.paused = false;
    emit('reset');
  }
  var OPS = {
    native: function () {
      state.model.status = 'Native pattern waiting for group lowering';
    },
    parser: function () {
      state.model.status = 'Verb and argument rule are registered separately';
    },
    dgd: function () {
      state.model.status = 'DGD grammar text waiting for the shared builder';
    },
    groups: function () {
      Model.lower(state.model);
    },
    grammar: function () {
      Model.compile(state.model);
    },
    tokens: function () {
      Model.scan(state.model);
    },
    chart: function () {
      Model.recognize(state.model);
    },
    trees: function () {
      Model.derive(state.model);
    },
    nativeOut: function () {
      Model.interpret(state.model);
    },
    parserOut: function () {
      Model.interpret(state.model);
    },
    dgdOut: function () {
      Model.interpret(state.model);
    },
    receipt: function () {},
    reject: function () {},
  };
  function fire(st) {
    state.station = st.id;
    state.stationT = 0;
    if (OPS[st.id]) OPS[st.id]();
    emit('station', st.id);
  }
  function travelBoost() {
    return (state.fastForward ? 2.4 : 1) * (state.tourDone ? 3 : 1);
  }
  function dwellBoost() {
    return (state.fastForward ? 2.2 : 1) * (state.tourDone ? 1.4 : 1);
  }
  function advanceRoute() {
    var name = van.routeName,
      m = state.model,
      f = state.input.family;
    if (name === 'native' || name === 'parser') useRoute('lowered');
    else if (name === 'lowered' || name === 'dgd') useRoute('forge');
    else if (name === 'forge') useRoute(m.error ? 'lexFail' : 'chartIn');
    else if (name === 'chartIn' || name === 'loop') {
      if (m.error) useRoute('chartFail');
      else if (m.chart.column < m.chart.tokens.length) {
        state.fastForward = true;
        useRoute('loop');
      } else {
        state.fastForward = false;
        useRoute('forest');
      }
    } else if (name === 'forest') useRoute(m.error ? 'treeFail' : f + 'Out');
    else if (/Out$/.test(name)) useRoute(f + (m.error ? 'Fail' : 'Receipt'));
    else {
      state.finished = true;
      state.paused = true;
      tour.done = World.districts.every(function (d) {
        return tour.seen[d.id];
      });
      state.tourDone = tour.done;
      emit('finished');
    }
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

  /* ---- queries used by the renderer and the camera ----------------------- */

  function vanPosition() {
    return Iso.smoothAt(routeOf(van.routeName), van.dist, 0.8);
  }

  global.Sim = {
    state: state,
    van: van,
    run: run,
    reset: function () {
      reset();
      emit('reset');
    },
    replayTour: function () {
      tour.seen = Object.create(null);
      tour.done = false;
    },
    update: update,
    vanPosition: vanPosition,
    on: function (fn) {
      listeners.push(fn);
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
      state.dwellLeft = 0;
      state.reading = false;
    },
  };
})(window);
