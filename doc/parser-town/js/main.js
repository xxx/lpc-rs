/* main.js: canvas, camera, input, and the frame loop.
 *
 * Copy this file into a new explainer almost unchanged. The only per-project
 * parts are the DOM ids it measures and the default framing at the bottom.
 */
(function (global) {
  'use strict';

  var Iso = global.Iso,
    World = global.World,
    Sim = global.Sim,
    Renderer = global.Renderer,
    UI = global.UI;

  var canvas = document.getElementById('stage');
  var tooltip = document.getElementById('tooltip');

  var cam = { x: 180, y: 560, scale: 0.8, ox: 0, oy: 0, dpr: 1 };
  var follow = true;
  var hoverDistrict = null;
  var pointer = { down: false, x: 0, y: 0, moved: 0 };
  var pinch = null;
  var viewW = 0,
    viewH = 0;

  World.build();
  UI.init();

  /* ------------------------------------------------------------------ view */

  /* Chrome furniture measured from the DOM, so the camera can aim at the part
     of the screen the town is actually visible in rather than the whole
     window. Hard-coding these is the fastest way to get a subject that hides
     behind a panel on someone else's screen. */
  var layout = { top: 58, dock: 120, sheet: 0, panel: 0 };
  var elDock = document.getElementById('dock');
  var elPanel = document.getElementById('inspector');
  var elHud = document.getElementById('hud');
  var elTopbar = document.querySelector('.topbar');

  var lastVars = {};
  function setVar(name, value) {
    if (lastVars[name] === value) return; // avoid a ResizeObserver loop
    lastVars[name] = value;
    document.documentElement.style.setProperty(name, value);
  }

  function measureLayout() {
    var mobile = viewW <= 900;

    if (elTopbar) {
      setVar(
        '--hud-top',
        Math.round(elTopbar.getBoundingClientRect().height - 6) + 'px',
      );
    }

    var hud = elHud ? elHud.getBoundingClientRect() : null;
    layout.top = hud ? Math.max(58, hud.bottom + 8) : 58;
    layout.dock = elDock ? elDock.getBoundingClientRect().height + 12 : 120;

    var panelHidden = !elPanel || elPanel.classList.contains('hidden');
    if (mobile) {
      layout.panel = 0;
      layout.sheet = panelHidden
        ? 0
        : elPanel.getBoundingClientRect().height || 0;
      setVar('--dock-h', Math.round(layout.dock - 12) + 'px');
    } else {
      layout.sheet = 0;
      layout.panel = panelHidden
        ? 0
        : elPanel.getBoundingClientRect().width + 28;
    }
  }

  /* The rectangle of screen not covered by panels, where the town should sit. */
  function availableRect() {
    return {
      x: 0,
      w: Math.max(200, viewW - layout.panel),
      y: layout.top,
      h: Math.max(160, viewH - layout.top - layout.dock - layout.sheet),
    };
  }

  function focusPoint() {
    var r = availableRect();
    return { x: r.x + r.w / 2, y: r.y + r.h / 2 };
  }

  function resize() {
    var dpr = Math.min(2, global.devicePixelRatio || 1);
    viewW = global.innerWidth;
    viewH = global.innerHeight;
    canvas.width = Math.round(viewW * dpr);
    canvas.height = Math.round(viewH * dpr);
    canvas.style.width = viewW + 'px';
    canvas.style.height = viewH + 'px';
    cam.dpr = dpr;
  }

  function syncOffsets() {
    var f = focusPoint();
    cam.ox = f.x - cam.x * cam.scale;
    cam.oy = f.y - cam.y * cam.scale;
  }

  function screenToWorld(sx, sy) {
    return Iso.unproject((sx - cam.ox) / cam.scale, (sy - cam.oy) / cam.scale);
  }

  /* The default view rides with the van rather than showing the whole town,
     because the tour is about what the van is doing. Zooming out to the whole
     place is one click, or a double-click on the map. */
  function defaultScale() {
    return viewW <= 900 ? 0.6 : 0.9;
  }

  function fitWorld() {
    var w = (World.GW + World.GH) * Iso.TW;
    var h = (World.GW + World.GH) * Iso.TH + 160;
    var r = availableRect();
    cam.scale = clamp(Math.min((r.w - 32) / w, (r.h - 32) / h), 0.12, 1.4);
    var c = Iso.project(World.GW / 2, World.GH / 2, 0);
    cam.x = c.x;
    cam.y = c.y;
  }

  /* Aim slightly along the direction of travel, so you see where the van is
     heading rather than where it has been. Small enough that a stopped van
     still sits essentially centred on its district. */
  var LOOKAHEAD = 2.5;

  function vanTarget() {
    var v = Sim.vanPosition();
    return Iso.project(
      v.x + (v.dx || 0) * LOOKAHEAD,
      v.y + (v.dy || 0) * LOOKAHEAD - 1.2,
      v.z,
    );
  }

  function centreOnVan() {
    var p = vanTarget();
    cam.x = p.x;
    cam.y = p.y;
  }

  function zoomStep(factor) {
    var r = availableRect();
    zoomAt(r.x + r.w / 2, r.y + r.h / 2, factor);
  }

  function zoomAt(mx, my, factor) {
    var px = (mx - cam.ox) / cam.scale,
      py = (my - cam.oy) / cam.scale;
    /* the low end has to reach far enough for the whole town to fit a phone */
    cam.scale = clamp(cam.scale * factor, 0.12, 2.4);
    var f = focusPoint();
    cam.x = px + (f.x - mx) / cam.scale;
    cam.y = py + (f.y - my) / cam.scale;
  }

  function clamp(v, a, b) {
    return v < a ? a : v > b ? b : v;
  }

  /* --------------------------------------------------------------- picking */

  /* Hit-testing happens in grid space against each district's radius, not
     against pixels, so it costs one unproject and a few distance checks. */
  function pick(sx, sy) {
    var w = screenToWorld(sx, sy);
    var best = null,
      bestD = 1e9;
    for (var i = 0; i < World.districts.length; i++) {
      var d = World.districts[i];
      var dist = Math.hypot(w.x - d.x, w.y - d.y);
      if (dist < d.r && dist < bestD) {
        bestD = dist;
        best = d;
      }
    }
    return best;
  }

  /* ----------------------------------------------------------------- input */

  canvas.addEventListener('pointerdown', function (e) {
    canvas.setPointerCapture(e.pointerId);
    pointer.down = true;
    pointer.x = e.clientX;
    pointer.y = e.clientY;
    pointer.moved = 0;
    canvas.classList.add('dragging');
  });

  canvas.addEventListener('pointermove', function (e) {
    if (pointer.down) {
      var dx = e.clientX - pointer.x,
        dy = e.clientY - pointer.y;
      pointer.moved += Math.abs(dx) + Math.abs(dy);
      cam.x -= dx / cam.scale;
      cam.y -= dy / cam.scale;
      pointer.x = e.clientX;
      pointer.y = e.clientY;
      /* a deliberate pan is a request to stop being dragged around */
      if (pointer.moved > 6) setFollow(false);
      hideTooltip();
      return;
    }
    var d = pick(e.clientX, e.clientY);
    hoverDistrict = d ? d.id : null;
    if (d) showTooltip(e.clientX, e.clientY, d);
    else hideTooltip();
    canvas.style.cursor = d ? 'pointer' : 'grab';
  });

  function endPointer(e) {
    if (!pointer.down) return;
    pointer.down = false;
    canvas.classList.remove('dragging');
    if (pointer.moved < 6) {
      var d = pick(e.clientX, e.clientY);
      if (d) UI.showDistrict(d, true);
      else UI.unpin();
    }
  }
  canvas.addEventListener('pointerup', endPointer);
  canvas.addEventListener('pointercancel', function () {
    pointer.down = false;
    canvas.classList.remove('dragging');
  });
  canvas.addEventListener('pointerleave', hideTooltip);

  canvas.addEventListener(
    'wheel',
    function (e) {
      e.preventDefault();
      zoomAt(e.clientX, e.clientY, Math.exp(-e.deltaY * 0.0016));
    },
    { passive: false },
  );

  canvas.addEventListener('dblclick', function () {
    fitWorld();
    setFollow(false);
  });

  canvas.addEventListener(
    'touchstart',
    function (e) {
      if (e.touches.length === 2) {
        pinch = {
          d: touchDist(e.touches),
          x: (e.touches[0].clientX + e.touches[1].clientX) / 2,
          y: (e.touches[0].clientY + e.touches[1].clientY) / 2,
        };
      }
    },
    { passive: true },
  );
  canvas.addEventListener(
    'touchmove',
    function (e) {
      if (e.touches.length === 2 && pinch) {
        var d = touchDist(e.touches);
        zoomAt(pinch.x, pinch.y, d / pinch.d);
        pinch.d = d;
        setFollow(false);
      }
    },
    { passive: true },
  );
  canvas.addEventListener(
    'touchend',
    function () {
      pinch = null;
    },
    { passive: true },
  );

  function touchDist(tt) {
    return Math.hypot(
      tt[0].clientX - tt[1].clientX,
      tt[0].clientY - tt[1].clientY,
    );
  }

  function showTooltip(x, y, d) {
    tooltip.hidden = false;
    tooltip.innerHTML = '<b>' + d.name + '</b>' + d.short;
    var r = tooltip.getBoundingClientRect();
    tooltip.style.left = Math.min(x + 14, viewW - r.width - 12) + 'px';
    tooltip.style.top = Math.min(y + 14, viewH - r.height - 12) + 'px';
  }
  function hideTooltip() {
    tooltip.hidden = true;
  }

  document.getElementById('zoom-in').addEventListener('click', function () {
    zoomStep(1.35);
  });
  document.getElementById('zoom-out').addEventListener('click', function () {
    zoomStep(1 / 1.35);
  });
  document.getElementById('zoom-fit').addEventListener('click', function () {
    /* following would immediately re-centre on the van and undo the fit */
    setFollow(false);
    fitWorld();
  });

  var followBox = document.getElementById('follow');
  followBox.addEventListener('change', function () {
    follow = followBox.checked;
  });
  function setFollow(v) {
    follow = v;
    followBox.checked = v;
  }

  document.addEventListener('keydown', function (e) {
    if (
      /^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName) ||
      !document.getElementById('about').hidden
    )
      return;
    if (e.target.tagName === 'BUTTON' && e.key === ' ') return;
    switch (e.key.toLowerCase()) {
      case ' ':
        e.preventDefault();
        if (Sim.state.finished) UI.run();
        else Sim.toggle();
        UI.paint(true);
        break;
      case 's':
        Sim.step();
        break;
      case 'r':
        UI.resetAll();
        break;
      case 'f':
        setFollow(!follow);
        break;
      case 'l':
        var lb = document.getElementById('labels');
        lb.checked = !lb.checked;
        Renderer.setLabels(lb.checked);
        break;
      case 'escape':
        UI.closeAbout();
        break;
    }
  });

  global.addEventListener('resize', function () {
    resize();
    measureLayout();
  });
  /* keep whatever zoom the reader chose across a rotation; re-fitting here
     would silently throw away their framing */
  global.addEventListener('orientationchange', function () {
    resize();
    measureLayout();
    syncOffsets();
  });

  /* The sheet and the dock change height as they open, wrap or gain a note, so
     re-measure when they actually resize instead of polling every frame. */
  if (global.ResizeObserver) {
    var ro = new global.ResizeObserver(function () {
      measureLayout();
    });
    [elDock, elPanel, elHud, elTopbar].forEach(function (n) {
      if (n) ro.observe(n);
    });
  }

  /* ------------------------------------------------------------------ loop */

  var last = performance.now();
  var clock = 0;

  function frame(now) {
    /* Schedule the next frame first. If anything below throws, the loop keeps
       running instead of the whole map freezing on one panel error. */
    requestAnimationFrame(frame);

    var dt = Math.min(0.05, (now - last) / 1000);
    last = now;
    clock += dt;

    Sim.update(dt);

    var target = UI.takeFlyTo();
    if (target) {
      var p = Iso.project(target.x, target.y, 0);
      cam.x += (p.x - cam.x) * 0.5;
      cam.y += (p.y - cam.y) * 0.5;
      cam.scale += (defaultScale() - cam.scale) * 0.5;
      setFollow(false);
    } else if (follow) {
      var lp = vanTarget();
      /* ~0.3 s time constant: reads as a camera easing along rather than a
         rigid lock to the van. Frame-rate independent, which a plain
         cam.x += (target - cam.x) * 0.1 is not. */
      var k = 1 - Math.pow(0.05, dt);
      cam.x += (lp.x - cam.x) * k;
      cam.y += (lp.y - cam.y) * k;
    }

    syncOffsets();
    Renderer.draw(canvas, cam, clock, UI.activeDistrict(), hoverDistrict);
    UI.paint(false);
  }

  /* ------------------------------------------------------------------ boot */

  resize();
  measureLayout();
  UI.run(); /* puts a van on the road, so it can be framed */
  cam.scale = defaultScale();
  centreOnVan();
  syncOffsets();
  requestAnimationFrame(frame);
})(window);
