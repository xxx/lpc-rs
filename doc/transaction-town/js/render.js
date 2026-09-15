(function (global) {
  'use strict';
  var Iso = global.Iso,
    World = global.World,
    Sim = global.Sim,
    P = Iso.project,
    C = World.palette;
  var ctx,
    cam,
    labels = [],
    showLabels = true;
  function box(x, y, z, w, d, h, color) {
    Iso.box(ctx, { x: x, y: y, z: z, w: w, d: d, h: h, color: color });
  }
  function ground() {
    ctx.fillStyle = '#e8e4d9';
    Iso.poly(ctx, [
      P(-1, -1, -0.5),
      P(51, -1, -0.5),
      P(51, 43, -0.5),
      P(-1, 43, -0.5)
    ]);
    box(0, 0, -0.6, World.GW, World.GH, 0.6, '#cad2b2');
    ctx.strokeStyle = 'rgba(70,91,63,.065)';
    ctx.lineWidth = 1;
    for (var x = 0; x < World.GW; x += 2)
      Iso.polyLine(ctx, [P(x, 0, 0), P(x, World.GH, 0)], false);
    for (var y = 0; y < World.GH; y += 2)
      Iso.polyLine(ctx, [P(0, y, 0), P(World.GW, y, 0)], false);
    World.districts.forEach(function (d) {
      ctx.fillStyle = Iso.mix('#e9e5d4', d.color, 0.07);
      Iso.poly(ctx, [
        P(d.bx - 2.8, d.by - 2.4, 0),
        P(d.bx + 2.8, d.by - 2.4, 0),
        P(d.bx + 2.8, d.by + 2.4, 0),
        P(d.bx - 2.8, d.by + 2.4, 0)
      ]);
    });
    Object.keys(World.routes).forEach(function (name) {
      var route = World.routes[name],
        color =
          name === 'retry' ? C.rust : name === 'success' ? C.teal : '#98947f';
      route.segs.forEach(function (s) {
        ctx.fillStyle = '#b3b49d';
        Iso.ribbon(ctx, s.a.x, s.a.y, s.b.x, s.b.y, 2.6, 0.01);
        ctx.fillStyle = '#ede8d7';
        Iso.ribbon(ctx, s.a.x, s.a.y, s.b.x, s.b.y, 2.28, 0.02);
      });
      for (var i = 2; i < route.total; i += 3) {
        var p = route.at(i);
        ctx.fillStyle = Iso.rgba(color, 0.65);
        Iso.ribbon(
          ctx,
          p.x,
          p.y,
          p.x + p.dx * 0.75,
          p.y + p.dy * 0.75,
          0.08,
          0.04
        );
      }
      for (var j = 5; j < route.total; j += 12) {
        var q = route.at(j),
          px = -q.dy,
          py = q.dx;
        ctx.fillStyle = color;
        Iso.poly(ctx, [
          P(q.x + q.dx * 0.45, q.y + q.dy * 0.45, 0.04),
          P(q.x - q.dx * 0.22 + px * 0.27, q.y - q.dy * 0.22 + py * 0.27, 0.04),
          P(q.x - q.dx * 0.22 - px * 0.27, q.y - q.dy * 0.22 - py * 0.27, 0.04)
        ]);
      }
    });
  }
  function roof(x, y, z, w, d, h, color) {
    Iso.gableRoof(ctx, { x: x, y: y, z: z, w: w, d: d, h: h, color: color });
  }
  function tree(o) {
    box(o.x - 0.1, o.y - 0.1, 0, 0.2, 0.2, 1.3, '#9e9170');
    Iso.cylinder(ctx, {
      x: o.x,
      y: o.y,
      z: 0.9,
      r: 0.64 * o.size,
      h: 1.1,
      color: '#899f78'
    });
    Iso.cylinder(ctx, {
      x: o.x,
      y: o.y,
      z: 1.8,
      r: 0.45 * o.size,
      h: 0.7,
      color: '#a5b48b'
    });
  }
  function landmark(d) {
    var x = d.bx,
      y = d.by,
      c = d.color,
      m = Sim.state.model,
      i;
    box(x - 2, y - 1.6, 0.02, 4, 3.2, 0.24, '#e6ddc8');
    switch (d.id) {
      case 'snapshot':
        box(x - 1.65, y - 1.25, 0.26, 3.3, 2.5, 2.5, '#d9ded1');
        for (i = 0; i < 3; i++)
          box(
            x - 1.7,
            y - 1.3,
            2.8 + i * 0.3,
            3.4,
            2.6,
            0.16,
            i % 2 ? '#acbbc0' : '#edf0e6'
          );
        box(x - 0.65, y + 1.26, 0.35, 1.3, 0.06, 1.8, c);
        break;
      case 'read':
        box(x - 1.7, y - 1.25, 0.26, 3.4, 2.5, 2.6, '#d4dfd7');
        for (i = 0; i < 2; i++) {
          box(x - 1.35 + i * 1.45, y + 1.27, 0.65, 1.15, 0.1, 1.7, c);
          box(x - 1 + i * 1.45, y + 1.39, 1.45, 0.45, 0.05, 0.12, '#e5c47c');
        }
        roof(x - 1.85, y - 1.4, 2.86, 3.7, 2.8, 0.9, c);
        break;
      case 'write':
        box(x - 1.5, y - 1, 0.26, 0.3, 2, 1.25, '#a7987a');
        box(x + 1.2, y - 1, 0.26, 0.3, 2, 1.25, '#a7987a');
        box(x - 1.8, y - 1.3, 1.5, 3.6, 2.6, 0.24, '#d3ad68');
        if (
          m.tx &&
          (Object.keys(m.tx.writes).length || Object.keys(m.tx.merges).length)
        )
          box(x - 0.9, y - 0.5, 1.75, 1.5, 1, 0.16, '#fff5d8');
        Iso.gear(ctx, x + 1, y - 0.4, 1.8, 0.5, 10, 0, c);
        break;
      case 'effects':
        box(x - 1.5, y - 0.9, 0.26, 3, 1.8, 2.1, '#c9c2d2');
        box(x - 1.6, y - 1, 2.36, 3.2, 2, 0.3, c);
        box(x - 0.9, y + 0.91, 1.7, 1.8, 0.08, 0.25, '#65586e');
        if (m.tx && m.tx.effects.length)
          box(x - 0.5, y + 0.97, 1.7, 1, 0.12, 0.48, '#f8ead6');
        break;
      case 'rival':
        box(x - 1.6, y - 1.25, 0.26, 3.2, 2.5, 2.1, '#ddc6b0');
        roof(x - 1.75, y - 1.4, 2.36, 3.5, 2.8, 1, c);
        box(x - 0.45, y + 1.26, 0.26, 0.9, 0.1, 1.5, '#79695a');
        break;
      case 'commit':
        box(x - 1.75, y - 1.4, 0.26, 3.5, 2.8, 2.5, '#c5d3c0');
        roof(x - 2, y - 1.65, 2.76, 4, 3.3, 1.25, c);
        box(x - 0.6, y + 1.42, 0.26, 1.2, 0.08, 1.9, '#33645e');
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: 4.02,
          r: 0.62,
          h: 0.4,
          color: '#d3aa55'
        });
        for (i = 0; i < 2; i++)
          box(x - 1.5 + i * 2.7, y + 1.52, 0.26, 0.3, 0.3, 2.6, '#eae7d7');
        break;
      case 'retry':
        box(x - 1.4, y - 1.1, 0.26, 2.8, 2.2, 1.4, '#b18e7a');
        box(x - 1.55, y - 1.25, 1.66, 3.1, 2.5, 0.25, c);
        box(x - 1, y - 0.8, 1.92, 2, 1.6, 0.05, '#665b4e');
        break;
      case 'admission':
        box(x - 1.8, y - 0.6, 0.26, 1.2, 1.4, 1.7, '#d0c5a4');
        roof(x - 1.9, y - 0.7, 1.96, 1.4, 1.6, 0.55, c);
        box(x + 0.6, y - 0.6, 0.26, 0.22, 1.4, 1.1, c);
        box(x + 0.6, y - 0.6, 1.36, 1.2, 0.16, 0.15, '#ede5ce');
        if (m.turn) box(x + 0.9, y - 0.1, 0.27, 0.5, 0.5, 0.25, C.teal);
        break;
      case 'deliver':
        box(x - 1.4, y - 1.1, 0.26, 2.8, 2.2, 2, '#ddd3ce');
        roof(x - 1.55, y - 1.25, 2.26, 3.1, 2.5, 0.7, c);
        box(
          x - 0.65,
          y + 1.12,
          1,
          0.95,
          0.1,
          0.6,
          m.world.output.length ? '#a1b590' : '#8b8092'
        );
        box(x + 1, y - 0.5, 2.9, 0.5, 0.5, 1.1, '#e5d8b7');
        Iso.cylinder(ctx, {
          x: x + 1.25,
          y: y - 0.25,
          z: 4,
          r: 0.35,
          h: 0.3,
          color: C.gold
        });
        break;
      case 'finish':
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: 0.26,
          r: 1.5,
          h: 0.4,
          color: '#b6c9c3'
        });
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: 0.68,
          r: 1.22,
          h: 0.03,
          color: '#84b6b3'
        });
        box(x - 0.45, y - 0.45, 0.7, 0.9, 0.9, 1.8, '#e5dec7');
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: 2.5,
          r: 0.65,
          h: 0.25,
          color: C.gold
        });
        break;
    }
  }
  function vehicle(v, tx, color) {
    var hx = v.dx,
      hy = v.dy,
      px = -hy,
      py = hx;
    function part(along, across, z, len, wid, h, c) {
      Iso.orientedBox(ctx, {
        x: v.x + hx * along + px * across,
        y: v.y + hy * along + py * across,
        z: z,
        hx: hx,
        hy: hy,
        len: len,
        wid: wid,
        h: h,
        color: c
      });
    }
    ctx.fillStyle = 'rgba(57,65,48,.16)';
    Iso.disc(ctx, v.x + 0.1, v.y + 0.1, 0.05, 1.5);
    [
      [0.9, 0.64],
      [0.9, -0.64],
      [-0.9, 0.64],
      [-0.9, -0.64]
    ].forEach(function (o) {
      ctx.fillStyle = '#4e534c';
      Iso.disc(
        ctx,
        v.x + hx * o[0] + px * o[1],
        v.y + hy * o[0] + py * o[1],
        0.25,
        0.25
      );
    });
    part(0, 0, 0.4, 3.1, 1.35, 0.45, '#ebe5d5');
    part(0.95, 0, 0.85, 1.05, 1.3, 0.9, color);
    part(1.05, 0, 1.76, 0.68, 1.05, 0.1, '#d8e7df');
    part(-0.6, 0, 0.85, 1.9, 1.3, 0.12, '#777c67');
    var counts = tx
      ? [
          Object.keys(tx.reads).length,
          Object.keys(tx.writes).length + Object.keys(tx.merges).length,
          tx.effects.length
        ]
      : [0, 0, 0];
    counts.forEach(function (n, i) {
      for (var j = 0; j < n; j++)
        part(
          -1.15 + i * 0.57,
          0,
          0.98 + j * 0.25,
          0.46,
          1,
          0.23,
          [C.blue, C.gold, C.violet][i]
        );
    });
  }
  function drawLabels() {
    /* Screen space, but still dpr-scaled: cam.ox and cam.scale are in CSS
       pixels, so an identity transform would read them as device pixels and
       every plate would land at half its true position on a 2x display. */
    ctx.setTransform(cam.dpr, 0, 0, cam.dpr, 0, 0);
    ctx.textBaseline = 'middle';

    /* Measure every plate first, then place them, because placing needs to
       know what is already on screen. Highest priority is measured first and
       keeps its natural position; the rest give way. */
    labels.sort(function (a, b) {
      return (b.pri || 0) - (a.pri || 0);
    });

    var placed = [];
    var i;
    for (i = 0; i < labels.length; i++) {
      var L = labels[i];
      var p = P(L.x, L.y, L.z);
      L.ax = p.x * cam.scale + cam.ox;
      L.ay = p.y * cam.scale + cam.oy;

      /* Plates stay legible instead of shrinking with the town, so they are
         always somewhat oversized when zoomed out. */
      L.px = (L.size || 12) * Math.min(1.15, Math.max(0.92, cam.scale));
      ctx.font = (L.bold ? '600 ' : '') + L.px + 'px ' + fontOf(L);
      var wpx = ctx.measureText(L.text).width;
      var subw = L.sub ? ctx.measureText(L.sub).width * 0.85 : 0;
      L.boxW = Math.max(wpx, subw) + 16;
      L.boxH = L.sub ? L.px * 2.4 : L.px * 1.75;

      /* Because plates are oversized, one centred on its anchor swallows the
         landmark underneath it at low zoom. Sit it on its bottom edge instead,
         a constant gap above the anchor, which reads the same at every zoom. */
      L.sy = L.lift ? L.ay - L.lift - L.boxH / 2 : L.ay;

      /* Two plates on top of each other are worse than one plate slightly out
         of place, so nudge upward until this one is clear. Ten steps, then give
         up and draw it anyway — a stuck loop is worse than an overlap. */
      for (var tries = 0; tries < 10 && overlaps(L, placed); tries++) {
        L.sy -= L.boxH * 0.92;
      }
      placed.push(L);
    }

    for (i = 0; i < labels.length; i++) drawPlate(labels[i]);
  }

  function fontOf(L) {
    return L.mono
      ? 'ui-monospace, Menlo, Consolas, monospace'
      : '"Iowan Old Style", Palatino, "Palatino Linotype", Georgia, serif';
  }

  function overlaps(L, placed) {
    for (var i = 0; i < placed.length; i++) {
      var o = placed[i];
      if (
        Math.abs(L.ax - o.ax) < (L.boxW + o.boxW) / 2 + 2 &&
        Math.abs(L.sy - o.sy) < (L.boxH + o.boxH) / 2 + 2
      )
        return true;
    }
    return false;
  }

  function drawPlate(L) {
    var ax = L.ax,
      ay = L.ay,
      sy = L.sy,
      size = L.px;
    var boxW = L.boxW,
      boxH = L.boxH;
    ctx.textAlign = 'center';
    ctx.font = (L.bold ? '600 ' : '') + size + 'px ' + fontOf(L);

    /* A leader down to the anchor, so a plate that had to be nudged out of the
       way is still visibly attached to the thing it names. */
    if (L.lift) {
      ctx.strokeStyle = Iso.rgba(L.tint || '#6e6250', 0.6);
      ctx.lineWidth = 1.2;
      ctx.beginPath();
      ctx.moveTo(ax, sy + boxH / 2);
      ctx.lineTo(ax, ay);
      ctx.stroke();
      ctx.fillStyle = Iso.rgba(L.tint || '#6e6250', 0.85);
      ctx.beginPath();
      ctx.arc(ax, ay, 2.4, 0, 6.2832);
      ctx.fill();
    }

    /* a drop shadow lifts the plate off pale roofs and grass alike */
    ctx.fillStyle = 'rgba(96,84,66,0.26)';
    roundRect(ax - boxW / 2 + 1, sy - boxH / 2 + 2.5, boxW, boxH, 5);
    ctx.fill();

    /* Washed with the district's own colour rather than plain white, so a
       plate reads as belonging to the building it points at. */
    ctx.fillStyle = L.tint ? Iso.mix('#fffdf7', L.tint, 0.14) : '#fffdf7';
    roundRect(ax - boxW / 2, sy - boxH / 2, boxW, boxH, 5);
    ctx.fill();
    ctx.strokeStyle = Iso.rgba(L.tint || '#6e6250', 0.85);
    ctx.lineWidth = L.bold ? 1.7 : 1.2;
    roundRect(ax - boxW / 2, sy - boxH / 2, boxW, boxH, 5);
    ctx.stroke();

    ctx.fillStyle = L.color || '#3a352e';
    ctx.fillText(L.text, ax, sy + (L.sub ? -size * 0.42 : 0));
    if (L.sub) {
      ctx.font = size * 0.85 + 'px ui-monospace, Menlo, Consolas, monospace';
      ctx.fillStyle = 'rgba(88,80,68,0.75)';
      ctx.fillText(L.sub, ax, sy + size * 0.62);
    }
  }

  function roundRect(x, y, w, h, r) {
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.arcTo(x + w, y, x + w, y + h, r);
    ctx.arcTo(x + w, y + h, x, y + h, r);
    ctx.arcTo(x, y + h, x, y, r);
    ctx.arcTo(x, y, x + w, y, r);
    ctx.closePath();
  }

  function draw(canvas, camera, time, active, hover) {
    ctx = canvas.getContext('2d');
    cam = camera;
    labels = [];
    ctx.setTransform(cam.dpr, 0, 0, cam.dpr, 0, 0);
    ctx.fillStyle = '#f2f0e7';
    ctx.fillRect(0, 0, canvas.width / cam.dpr, canvas.height / cam.dpr);
    ctx.setTransform(
      cam.scale * cam.dpr,
      0,
      0,
      cam.scale * cam.dpr,
      cam.ox * cam.dpr,
      cam.oy * cam.dpr
    );
    ground();
    var m = Sim.state.model,
      v = Sim.vanPosition(),
      items = [];
    World.props.forEach(function (p) {
      items.push({
        k: p.x + p.y,
        draw: function () {
          tree(p);
        }
      });
    });
    World.districts.forEach(function (d) {
      items.push({
        k: d.bx + d.by,
        draw: function () {
          landmark(d);
        }
      });
    });
    items.push({
      k: v.x + v.y,
      draw: function () {
        vehicle(v, m.tx, C.blue);
      }
    });
    if (m.rival) {
      var b = { x: 41.2, y: 18.8, dx: 0, dy: 1 };
      items.push({
        k: b.x + b.y,
        draw: function () {
          vehicle(b, m.rival.tx, C.rust);
        }
      });
      if (showLabels)
        labels.push({
          x: b.x,
          y: b.y,
          z: 2,
          lift: 12,
          text: 'B · committed v' + m.rival.version,
          sub: m.rival.cell + ' ' + m.rival.before + ' → ' + m.rival.after,
          tint: C.rust,
          pri: 3,
          mono: true
        });
    }
    items.sort(function (a, b) {
      return a.k - b.k;
    });
    items.forEach(function (item) {
      item.draw();
    });
    if (showLabels)
      World.districts.forEach(function (d) {
        var on = d.id === active || d.id === hover;
        if (cam.scale < 0.34 && !on) return;
        labels.push({
          x: d.bx,
          y: d.by,
          z: d.id === 'write' ? 2 : 4.4,
          lift: 12,
          text: d.name,
          sub: on ? d.tag : null,
          tint: d.color,
          bold: on,
          size: on ? 15 : 13,
          pri: on ? 2 : 1
        });
      });
    if (m.tx)
      labels.push({
        x: v.x,
        y: v.y,
        z: 2,
        lift: 12,
        text: 'A · attempt ' + m.attempts + ' · base v' + m.tx.base,
        sub:
          'R ' +
          Object.keys(m.tx.reads).length +
          '   W ' +
          (Object.keys(m.tx.writes).length + Object.keys(m.tx.merges).length) +
          '   E ' +
          m.tx.effects.length,
        tint: m.tx.status === 'rejected' ? C.rust : C.blue,
        mono: true,
        bold: true,
        size: 13,
        pri: 5
      });
    labels = labels.filter(function (l) {
      var p = P(l.x, l.y, l.z),
        x = p.x * cam.scale + cam.ox,
        y = p.y * cam.scale + cam.oy;
      return (
        x > -100 &&
        x < canvas.width / cam.dpr + 100 &&
        y > -60 &&
        y < canvas.height / cam.dpr + 100
      );
    });
    drawLabels();
  }
  global.Renderer = {
    draw: draw,
    setLabels: function (value) {
      showLabels = value;
    }
  };
})(window);
