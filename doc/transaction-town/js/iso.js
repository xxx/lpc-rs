/* iso.js: isometric projection + primitive drawing helpers.
 *
 * THIS FILE IS THE ENGINE. Copy it into a new explainer unchanged.
 * Grid space: x grows toward the lower-right, y toward the lower-left, z up.
 * Every drawing helper takes grid coordinates and projects internally, so
 * nothing above this file needs to know about pixels.
 */
(function (global) {
  'use strict';

  var TW = 30;   // half tile width  (px per grid unit on screen-x)
  var TH = 15;   // half tile height (px per grid unit on screen-y)
  var TZ = 20;   // px per grid unit of height

  function project(x, y, z) {
    return { x: (x - y) * TW, y: (x + y) * TH - (z || 0) * TZ };
  }

  /* Inverse projection onto the z = 0 ground plane. Used for hit-testing:
     picking is done in grid space against district radii, not in pixels. */
  function unproject(sx, sy) {
    var a = sx / TW, b = sy / TH;
    return { x: (a + b) / 2, y: (b - a) / 2 };
  }

  /* ---- colour helpers ---------------------------------------------------- */

  var shadeCache = Object.create(null);

  /* Accepts "#rgb", "#rrggbb" and "rgb(r,g,b)". The rgb() form is here as a
     guard: feeding one to parseInt(.., 16) yields NaN, which shades to solid
     black, and that failure is silent and easy to miss on a dark surface. */
  function parseHex(hex) {
    if (hex.charCodeAt(0) !== 35) {
      var m = /(\d+)\D+(\d+)\D+(\d+)/.exec(hex);
      if (m) return [+m[1], +m[2], +m[3]];
    }
    var h = hex.replace('#', '');
    if (h.length === 3) h = h[0] + h[0] + h[1] + h[1] + h[2] + h[2];
    var n = parseInt(h, 16);
    if (isNaN(n)) return [255, 0, 255];      /* loud magenta, not silent black */
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
  }

  function shade(hex, f) {
    /* Quantise before caching. Anything animated feeds this a continuously
       varying factor, which would otherwise miss the cache on every call and
       grow it without bound for as long as the page is open. */
    f = Math.round(f * 64) / 64;
    var key = hex + '|' + f;
    var hit = shadeCache[key];
    if (hit) return hit;
    var c = parseHex(hex);
    var out = 'rgb(' +
      Math.min(255, Math.round(c[0] * f)) + ',' +
      Math.min(255, Math.round(c[1] * f)) + ',' +
      Math.min(255, Math.round(c[2] * f)) + ')';
    shadeCache[key] = out;
    return out;
  }

  function rgba(hex, a) {
    var c = parseHex(hex);
    return 'rgba(' + c[0] + ',' + c[1] + ',' + c[2] + ',' + a + ')';
  }

  /* Returns hex, not rgb(), because the result is routinely fed back into
     shade(), which only parses hex and would otherwise produce black. */
  function mix(hexA, hexB, t) {
    var a = parseHex(hexA), b = parseHex(hexB);
    var out = '#';
    for (var i = 0; i < 3; i++) {
      var v = Math.max(0, Math.min(255, Math.round(a[i] + (b[i] - a[i]) * t)));
      out += (v < 16 ? '0' : '') + v.toString(16);
    }
    return out;
  }

  /* ---- deterministic noise ----------------------------------------------- */

  /* Scenery must not shimmer. Anything decorative (window lights, grass
     tufts, crate colours) picks its variation from this, not Math.random(),
     so it is identical on every frame and across reloads. */
  function hash2(x, y, s) {
    var h = (Math.imul(x | 0, 374761393) ^ Math.imul(y | 0, 668265263) ^ Math.imul(s | 0, 2246822519)) >>> 0;
    h = Math.imul(h ^ (h >>> 13), 1274126177) >>> 0;
    return ((h ^ (h >>> 16)) >>> 0) / 4294967296;
  }

  /* ---- primitives -------------------------------------------------------- */

  function poly(ctx, pts) {
    ctx.beginPath();
    ctx.moveTo(pts[0].x, pts[0].y);
    for (var i = 1; i < pts.length; i++) ctx.lineTo(pts[i].x, pts[i].y);
    ctx.closePath();
    ctx.fill();
  }

  function polyLine(ctx, pts, close) {
    ctx.beginPath();
    ctx.moveTo(pts[0].x, pts[0].y);
    for (var i = 1; i < pts.length; i++) ctx.lineTo(pts[i].x, pts[i].y);
    if (close) ctx.closePath();
    ctx.stroke();
  }

  /* Daylight shading: gentle steps between faces so pale surfaces keep their
     colour instead of going muddy, the way a paper model reads. */
  var TOP = 1.0, RIGHT = 0.89, LEFT = 0.76;

  /* Every solid gets a soft pencil outline; it is what makes the scene read as
     a drawn diagram rather than a render. Pass edge:false to opt out. */
  var DEFAULT_EDGE = 'rgba(88,78,64,0.30)';

  /* An axis-aligned box. o = {x,y,z,w,d,h,color,windows,panels,alpha,edge} */
  function box(ctx, o) {
    var x = o.x, y = o.y, z = o.z || 0, w = o.w, d = o.d, h = o.h;
    var c = o.color, t = z + h;
    if (o.alpha != null) { ctx.save(); ctx.globalAlpha *= o.alpha; }

    var A = project(x, y, t), B = project(x + w, y, t),
        C = project(x + w, y + d, t), D = project(x, y + d, t);
    var Bb = project(x + w, y, z), Cb = project(x + w, y + d, z), Db = project(x, y + d, z);

    ctx.fillStyle = shade(c, RIGHT); poly(ctx, [B, C, Cb, Bb]);
    ctx.fillStyle = shade(c, LEFT);  poly(ctx, [D, C, Cb, Db]);

    if (o.windows) drawWindows(ctx, o);
    if (o.panels) drawPanels(ctx, o);

    ctx.fillStyle = shade(c, o.topShade != null ? o.topShade : TOP);
    poly(ctx, [A, B, C, D]);

    var edge = o.edge === false ? null : (o.edge || DEFAULT_EDGE);
    if (edge) {
      ctx.strokeStyle = edge;
      ctx.lineWidth = o.edgeWidth || 1;
      ctx.lineJoin = 'round';
      polyLine(ctx, [A, B, C, D], true);
      polyLine(ctx, [B, Bb], false);
      polyLine(ctx, [C, Cb], false);
      polyLine(ctx, [D, Db], false);
    }
    if (o.alpha != null) ctx.restore();
  }

  /* A grid of windows on the two camera-facing walls. Daytime: dark glass
     reflecting sky, not lit rooms. */
  function drawWindows(ctx, o) {
    var win = o.windows;
    var cols = win.cols || 3, rows = win.rows || Math.max(1, Math.round(o.h * 1.6));
    var x = o.x, y = o.y, z = o.z || 0, w = o.w, d = o.d, h = o.h;
    var seed = win.seed || 1;
    var lit = win.color || '#5d7182';
    var dark = win.dark || '#93a6b2';

    var X1 = x + w, Y1 = y + d, r, c, on, n;
    for (r = 0; r < rows; r++) {
      for (c = 0; c < cols; c++) {
        n = hash2(r * 31 + c, seed, 7);
        on = n > 0.52;
        ctx.fillStyle = on ? rgba(lit, 0.5) : rgba(dark, 0.5);
        var z0 = z + h * ((r + 0.28) / rows), z1 = z + h * ((r + 0.72) / rows);
        var v0 = y + d * ((c + 0.26) / cols), v1 = y + d * ((c + 0.74) / cols);
        poly(ctx, [project(X1, v0, z1), project(X1, v1, z1), project(X1, v1, z0), project(X1, v0, z0)]);

        n = hash2(r * 17 + c, seed + 3, 11);
        on = n > 0.55;
        ctx.fillStyle = on ? rgba(lit, 0.42) : rgba(dark, 0.42);
        var u0 = x + w * ((c + 0.26) / cols), u1 = x + w * ((c + 0.74) / cols);
        poly(ctx, [project(u0, Y1, z1), project(u1, Y1, z1), project(u1, Y1, z0), project(u0, Y1, z0)]);
      }
    }
  }

  /* Same idea as a window grid, but the rows read as sheet joints, which is
     what an industrial shed actually looks like. */
  function drawPanels(ctx, o) {
    var p = o.panels;
    var cols = p.cols || 4, rows = p.rows || Math.max(1, Math.round(o.h * 1.1));
    var x = o.x, y = o.y, z = o.z || 0, w = o.w, d = o.d, h = o.h;
    var seed = p.seed || 1;
    var glass = p.color || '#8fa4b0';
    var joint = p.joint || 'rgba(70,62,52,0.16)';

    var X1 = x + w, Y1 = y + d, r, c;
    for (r = 0; r < rows; r++) {
      /* one glazed band, the rest ribbed sheet */
      var glazed = p.band != null ? r === p.band : (r === rows - 1 && rows > 1);
      var z0 = z + h * ((r + 0.22) / rows), z1 = z + h * ((r + 0.78) / rows);
      for (c = 0; c < cols; c++) {
        var v0 = y + d * ((c + 0.16) / cols), v1 = y + d * ((c + 0.84) / cols);
        ctx.fillStyle = glazed
          ? rgba(glass, 0.42 + 0.18 * hash2(r * 31 + c, seed, 7))
          : joint;
        poly(ctx, [project(X1, v0, z1), project(X1, v1, z1), project(X1, v1, z0), project(X1, v0, z0)]);

        var u0 = x + w * ((c + 0.16) / cols), u1 = x + w * ((c + 0.84) / cols);
        ctx.fillStyle = glazed
          ? rgba(glass, 0.34 + 0.16 * hash2(r * 17 + c, seed + 3, 11))
          : joint;
        poly(ctx, [project(u0, Y1, z1), project(u1, Y1, z1), project(u1, Y1, z0), project(u0, Y1, z0)]);
      }
    }
  }

  /* Extrude a ground polygon upwards. Unlike box(), the footprint can sit at
     any angle, so the side faces are shaded from their own normals and painted
     back to front rather than assuming which two are visible. */
  function prism(ctx, base, z, h, color, edge) {
    var i, top = [], bot = [], n = base.length;
    for (i = 0; i < n; i++) {
      top.push(project(base[i].x, base[i].y, z + h));
      bot.push(project(base[i].x, base[i].y, z));
    }

    var faces = [];
    for (i = 0; i < n; i++) {
      var j = (i + 1) % n;
      var ex = base[j].x - base[i].x, ey = base[j].y - base[i].y;
      var el = Math.hypot(ex, ey) || 1;
      var nx = -ey / el, ny = ex / el;                 /* outward normal */
      /* Both +x and +y lean toward the camera, so a face is only visible when
         nx + ny > 0. Culling the other two halves the work per solid. */
      if (nx + ny <= 0) continue;
      faces.push({
        depth: base[i].x + base[i].y + base[j].x + base[j].y,
        shade: 0.80 + 0.09 * nx - 0.06 * ny,           /* matches box() faces */
        quad: [top[i], top[j], bot[j], bot[i]]
      });
    }
    faces.sort(function (a, b) { return a.depth - b.depth; });
    for (i = 0; i < faces.length; i++) {
      ctx.fillStyle = shade(color, faces[i].shade);
      poly(ctx, faces[i].quad);
    }

    ctx.fillStyle = shade(color, 1.0);
    poly(ctx, top);

    var e = edge === false ? null : (edge || DEFAULT_EDGE);
    if (e) {
      ctx.strokeStyle = e;
      ctx.lineWidth = 1;
      ctx.lineJoin = 'round';
      polyLine(ctx, top, true);
    }
  }

  /* A box turned to face (hx, hy) along the ground. `len` runs along the
     heading, `wid` across it. This is what every vehicle is built from. */
  function orientedBox(ctx, o) {
    var m = Math.hypot(o.hx, o.hy) || 1;
    var hx = o.hx / m, hy = o.hy / m;
    var px = -hy, py = hx;
    var L = o.len / 2, W = o.wid / 2;
    prism(ctx, [
      { x: o.x + hx * L + px * W, y: o.y + hy * L + py * W },
      { x: o.x + hx * L - px * W, y: o.y + hy * L - py * W },
      { x: o.x - hx * L - px * W, y: o.y - hy * L - py * W },
      { x: o.x - hx * L + px * W, y: o.y - hy * L + py * W }
    ], o.z || 0, o.h, o.color, o.edge);
  }

  /* A pitched roof sitting on a box footprint, ridge running along +x.
     Both slopes are visible from an isometric camera, so both are drawn. */
  function gableRoof(ctx, o) {
    var x = o.x, y = o.y, z = o.z, w = o.w, d = o.d, h = o.h, c = o.color;
    var my = y + d / 2, tz = z + h;
    var A = project(x, y, z), B = project(x + w, y, z);
    var C = project(x + w, y + d, z), D = project(x, y + d, z);
    var R1 = project(x, my, tz), R2 = project(x + w, my, tz);

    ctx.fillStyle = shade(c, 1.04);          // slope facing away, catches sky
    poly(ctx, [A, B, R2, R1]);
    ctx.fillStyle = shade(c, 0.88);          // gable end
    poly(ctx, [B, R2, C]);
    ctx.fillStyle = shade(c, 0.78);          // slope facing the camera
    poly(ctx, [D, C, R2, R1]);

    var edge = o.edge === false ? null : (o.edge || DEFAULT_EDGE);
    if (edge) {
      ctx.strokeStyle = edge;
      ctx.lineWidth = 1;
      ctx.lineJoin = 'round';
      polyLine(ctx, [A, B, R2, R1], true);
      polyLine(ctx, [D, C, R2, R1], true);
      polyLine(ctx, [B, R2, C], true);
    }
  }

  /* A vertical cylinder centred on (x,y). o = {x,y,z,r,h,color,ring} */
  function cylinder(ctx, o) {
    var r = o.r, z = o.z || 0, h = o.h, c = o.color;
    var a = r * TW * 1.41421, b = r * TH * 1.41421;
    var top = project(o.x, o.y, z + h);
    var bot = project(o.x, o.y, z);

    ctx.fillStyle = shade(c, 0.74);
    ctx.beginPath();
    ctx.ellipse(bot.x, bot.y, a, b, 0, 0, Math.PI);
    ctx.lineTo(top.x - a, top.y);
    ctx.lineTo(bot.x - a, bot.y);
    ctx.closePath();
    ctx.fill();

    ctx.fillStyle = shade(c, 0.87);
    ctx.fillRect(top.x - a, top.y, a * 2, bot.y - top.y);

    if (o.ring) {
      ctx.fillStyle = shade(c, 0.95);
      var ry = top.y + (bot.y - top.y) * (o.ring);
      ctx.fillRect(top.x - a, ry, a * 2, Math.max(2, b * 0.35));
    }

    ctx.fillStyle = shade(c, o.topShade != null ? o.topShade : 1.05);
    ctx.beginPath();
    ctx.ellipse(top.x, top.y, a, b, 0, 0, Math.PI * 2);
    ctx.fill();

    var cedge = o.edge === false ? null : (o.edge || DEFAULT_EDGE);
    if (cedge) {
      ctx.strokeStyle = cedge;
      ctx.lineWidth = 1;
      ctx.stroke();                                   /* top rim */
      ctx.beginPath();                                /* silhouette sides */
      ctx.moveTo(top.x - a, top.y);
      ctx.lineTo(bot.x - a, bot.y);
      ctx.moveTo(top.x + a, top.y);
      ctx.lineTo(bot.x + a, bot.y);
      ctx.stroke();
    }
  }

  /* Flat quad on the ground between two grid points, given a width. Roads,
     belts, conveyors and pipes are all this. */
  function ribbon(ctx, ax, ay, bx, by, width, z) {
    var dx = bx - ax, dy = by - ay;
    var len = Math.hypot(dx, dy) || 1;
    var nx = -dy / len * width / 2, ny = dx / len * width / 2;
    poly(ctx, [
      project(ax + nx, ay + ny, z || 0),
      project(bx + nx, by + ny, z || 0),
      project(bx - nx, by - ny, z || 0),
      project(ax - nx, ay - ny, z || 0)
    ]);
  }

  function disc(ctx, x, y, z, r) {
    var p = project(x, y, z || 0);
    ctx.beginPath();
    ctx.ellipse(p.x, p.y, r * TW * 1.41421, r * TH * 1.41421, 0, 0, Math.PI * 2);
    ctx.fill();
  }

  /* A toothed wheel lying flat, seen in projection. Half the reason a machine
     reads as a machine is that something is visibly turning. */
  function gear(ctx, x, y, z, r, teeth, ang, color) {
    var p = project(x, y, z || 0);
    var i, a, rr;
    ctx.fillStyle = color;
    ctx.beginPath();
    for (i = 0; i < teeth * 2; i++) {
      a = ang + (i / (teeth * 2)) * Math.PI * 2;
      rr = r * (i % 2 ? 0.78 : 1);
      /* rotate in grid space, then project, so the wheel sits on the floor */
      var q = project(x + Math.cos(a) * rr, y + Math.sin(a) * rr, z || 0);
      if (i === 0) ctx.moveTo(q.x, q.y); else ctx.lineTo(q.x, q.y);
    }
    ctx.closePath();
    ctx.fill();
    ctx.fillStyle = shade(color, 0.72);
    ctx.beginPath();
    ctx.ellipse(p.x, p.y, r * 0.3 * TW * 1.41421, r * 0.3 * TH * 1.41421, 0, 0, Math.PI * 2);
    ctx.fill();
  }

  /* ---- routes ------------------------------------------------------------ */

  /* A polyline the travelling thing rides along, parameterised by distance.
     `cum[i]` is the distance to waypoint i, which is how stations are anchored
     ("station 3 sits at waypoint 5") instead of by hand-measured length. */
  function makeRoute(raw) {
    var pts = raw.map(function (p) { return { x: p[0], y: p[1], z: p[2] || 0 }; });
    var segs = [], total = 0, cum = [0];
    for (var i = 0; i < pts.length - 1; i++) {
      var a = pts[i], b = pts[i + 1];
      var len = Math.hypot(b.x - a.x, b.y - a.y) || 0.001;
      segs.push({ a: a, b: b, len: len, cum: total });
      total += len;
      cum.push(total);
    }
    return {
      pts: pts, segs: segs, total: total, cum: cum,
      at: function (d) {
        var s0, sn, i, s, t;
        if (d <= 0) {
          s0 = segs[0];
          return { x: s0.a.x, y: s0.a.y, z: s0.a.z,
                   dx: (s0.b.x - s0.a.x) / s0.len, dy: (s0.b.y - s0.a.y) / s0.len };
        }
        if (d >= total) {
          sn = segs[segs.length - 1];
          return { x: sn.b.x, y: sn.b.y, z: sn.b.z,
                   dx: (sn.b.x - sn.a.x) / sn.len, dy: (sn.b.y - sn.a.y) / sn.len };
        }
        for (i = 0; i < segs.length; i++) {
          s = segs[i];
          if (d <= s.cum + s.len) {
            t = (d - s.cum) / s.len;
            return {
              x: s.a.x + (s.b.x - s.a.x) * t,
              y: s.a.y + (s.b.y - s.a.y) * t,
              z: s.a.z + (s.b.z - s.a.z) * t,
              dx: (s.b.x - s.a.x) / s.len,
              dy: (s.b.y - s.a.y) / s.len
            };
          }
        }
      }
    };
  }

  /* Routes are polylines with hard corners, so reading one directly makes a
     vehicle snap to a new heading the instant it crosses a waypoint. Averaging
     a sample either side rounds the turn and swings the heading through it. */
  function smoothAt(route, d, look) {
    /* Clamp first. A vehicle routinely overshoots the end of a route by a frame
       before the state machine moves it on, and one parked at the end sits past
       it indefinitely — in both cases an unclamped forward sample lands on the
       endpoint too. */
    d = Math.max(0, Math.min(route.total, d));
    var a = route.at(Math.max(0, d - look));
    var m = route.at(d);
    var b = route.at(Math.min(route.total, d + look));
    var hx = b.x - a.x, hy = b.y - a.y;
    var len = Math.hypot(hx, hy);
    /* At either end of the route both samples can land on the same point, which
       gives a zero-length heading. Fall back to the segment's own direction:
       a heading of (0, 0) collapses every oriented box built from it to zero
       area, so the vehicle silently vanishes and leaves only its shadow. */
    if (len < 1e-6) { hx = m.dx; hy = m.dy; len = Math.hypot(hx, hy) || 1; }
    return {
      x: (a.x + 2 * m.x + b.x) / 4,
      y: (a.y + 2 * m.y + b.y) / 4,
      z: (a.z + 2 * m.z + b.z) / 4,
      dx: hx / len, dy: hy / len
    };
  }

  global.Iso = {
    TW: TW, TH: TH, TZ: TZ,
    project: project, unproject: unproject,
    shade: shade, rgba: rgba, mix: mix, parseHex: parseHex,
    hash2: hash2,
    poly: poly, polyLine: polyLine,
    box: box, prism: prism, orientedBox: orientedBox, gableRoof: gableRoof,
    cylinder: cylinder, ribbon: ribbon, disc: disc, gear: gear,
    makeRoute: makeRoute, smoothAt: smoothAt
  };
})(window);
