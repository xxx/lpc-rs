(function (global) {
  'use strict';
  var Iso = global.Iso,
    World = global.World,
    Sim = global.Sim,
    labels = true;
  var C = World.C;
  function box(items, x, y, z, w, d, h, color, extra) {
    var o = { x: x, y: y, z: z, w: w, d: d, h: h, color: color };
    if (extra)
      Object.keys(extra).forEach(function (k) {
        o[k] = extra[k];
      });
    items.push({
      depth: x + y + (w + d) / 2,
      draw: function (ctx) {
        Iso.box(ctx, o);
      },
    });
  }
  function cylinder(items, x, y, z, r, h, color) {
    items.push({
      depth: x + y,
      draw: function (ctx) {
        Iso.cylinder(ctx, { x: x, y: y, z: z, r: r, h: h, color: color });
      },
    });
  }
  function tree(items, x, y, h) {
    cylinder(items, x, y, 0, 0.12, h * 0.7, '#ad9c7d');
    items.push({
      depth: x + y + 0.01,
      draw: function (ctx) {
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: h * 0.52,
          r: 0.65,
          h: 0.35,
          color: '#a9b7a1',
        });
        Iso.cylinder(ctx, {
          x: x,
          y: y,
          z: h * 0.75,
          r: 0.45,
          h: 0.3,
          color: '#bac8ae',
        });
      },
    });
  }
  function landmark(items, d, m) {
    var x = d.bx,
      y = d.by,
      c = d.color,
      i;
    if (d.kind === 'entry') {
      box(items, x - 1.6, y - 1.2, 0, 3.2, 2.4, 0.28, '#d4cdbd');
      box(
        items,
        x - 1.25,
        y - 1,
        0.28,
        2.5,
        2,
        1.65,
        Iso.mix(c, '#ffffff', 0.5),
        { windows: { cols: 3, rows: 1 } },
      );
      box(items, x - 1.55, y - 1.25, 1.93, 3.1, 2.5, 0.2, c);
      for (i = 0; i < 3; i++)
        box(
          items,
          x - 0.9 + i * 0.65,
          y + 0.6,
          2.13,
          0.45,
          0.3,
          0.45,
          '#faf5e8',
        );
    } else if (d.kind === 'groups') {
      box(items, x - 1.7, y - 1, 0, 3.4, 2, 0.35, '#cfc8b6');
      var groups = m.groups || [];
      for (i = 0; i < groups.length; i++)
        box(
          items,
          x - 1.4 + (i % 4) * 0.72,
          y - 0.8 + Math.floor(i / 4) * 0.9,
          0.35,
          0.55,
          0.65,
          0.6,
          groups[i].kind ? '#e8bd74' : '#8da6be',
        );
      if (!groups.length)
        box(items, x - 1.3, y - 0.7, 0.35, 2.6, 1.4, 0.15, '#eee6d5');
    } else if (d.kind === 'grammar') {
      box(items, x - 2, y - 1.4, 0, 4, 2.8, 1.2, '#c9d4de', {
        panels: { cols: 4, rows: 2 },
      });
      var count = m.g ? m.g.rules.length : 0;
      for (i = 0; i < count; i++)
        box(
          items,
          x - 1.6 + (i % 3) * 1.05,
          y - 1 + Math.floor(i / 3) * 0.55,
          1.2,
          0.86,
          0.4,
          0.18,
          C.shared,
        );
      cylinder(items, x + 1.1, y - 1, 1.2, 0.27, 2.2, '#8396a7');
    } else if (d.kind === 'tokens') {
      box(items, x - 2, y - 1, 0, 4, 2, 0.6, '#c8c5b7');
      box(items, x - 1.8, y - 0.8, 0.6, 3.6, 1.6, 0.15, '#566669');
      var tokens = m.scan ? m.scan.tokens : [];
      for (i = 0; i < tokens.length; i++)
        box(
          items,
          x - 1.65 + (i % 6) * 0.54,
          y - 0.7 + Math.floor(i / 6) * 0.36,
          0.75,
          0.4,
          0.27,
          0.28,
          tokens[i].className === 'number' ? '#e8bd74' : '#bed6d5',
        );
      cylinder(items, x - 2, y, 0, 0.16, 2.2, C.shared);
      cylinder(items, x + 2, y, 0, 0.16, 2.2, C.shared);
    } else if (d.kind === 'chart') {
      var sets = m.chart ? m.chart.sets : [];
      box(items, x - 2, y - 1.2, 0, 4.7, 2.5, 0.15, '#d2cfc0');
      for (i = 0; i < sets.length; i++) {
        var xx = x - 1.75 + (i % 8) * 0.52,
          yy = y - 0.95 + Math.floor(i / 8) * 0.6;
        var height = sets[i].length * 0.16;
        box(
          items,
          xx,
          yy,
          0.15,
          0.38,
          0.4,
          Math.max(0.06, height),
          i === m.chart.column ? '#dcad5f' : '#9db2ca',
        );
      }
    } else if (d.kind === 'trees') {
      box(items, x - 1.8, y - 1.2, 0, 3.6, 2.4, 0.16, '#e1dccf');
      var t = m.derivations[0];
      if (t) {
        var leaves = Math.max(1, m.scan.tokens.length);
        function branch(node, depth) {
          var xx = x - 1.5 + ((node.start + node.end) / 2 / leaves) * 3,
            yy = y - 0.85 + depth * 0.33;
          cylinder(items, xx, yy, 0.16, 0.09, 0.65, '#9cab7e');
          cylinder(
            items,
            xx,
            yy,
            0.81,
            0.16,
            0.13,
            node.token != null ? '#cfb279' : '#7c97ac',
          );
          if (node.children)
            node.children.forEach(function (ch) {
              branch(ch, depth + 1);
            });
        }
        branch(t, 0);
      } else
        for (i = 0; i < 3; i++)
          cylinder(items, x - 1 + i, y, 0.16, 0.2, 0.4, '#c1c8b5');
    } else if (d.kind === 'objects') {
      box(items, x - 1.7, y - 1.2, 0, 3.4, 2.4, 0.4, '#ddd0b7');
      if (m.input.sword) {
        box(items, x - 1, y - 0.7, 0.4, 0.16, 1.5, 1.1, '#93a5ad');
        box(items, x - 1.3, y + 0.1, 1.2, 0.75, 0.17, 0.14, '#c3a36b');
      }
      cylinder(items, x + 0.75, y, 0.4, 0.35, 0.75, '#a8b9ba');
      cylinder(items, x + 0.75, y, 1.15, 0.25, 0.4, '#d6b797');
    } else if (d.kind === 'gates') {
      box(items, x - 1.6, y - 1.5, 0, 3.2, 3, 0.2, '#ced8ca');
      for (i = 0; i < 4; i++) {
        var h = m.calls.length > i ? 0.9 : 0.35;
        box(
          items,
          x - 1.3 + (i % 2) * 1.5,
          y - 1.2 + Math.floor(i / 2) * 1.5,
          0.2,
          1,
          1,
          h,
          i === 0 && !m.input.allow
            ? C.fail
            : Iso.mix(C.parser, '#ffffff', 0.3),
        );
      }
    } else if (d.kind === 'actions') {
      box(items, x - 1.7, y - 1.1, 0, 3.4, 2.2, 1, '#cfc4dd');
      box(items, x - 1.4, y - 0.9, 1, 2.8, 1.8, 0.2, C.dgd);
      for (
        i = 0;
        i < (m.output && m.input.family === 'dgd' ? m.output.length : 0);
        i++
      )
        box(
          items,
          x - 1.25 + i * 0.45,
          y - 0.65,
          1.2,
          0.35,
          1.2,
          0.15,
          '#f7efdd',
        );
    } else if (d.kind === 'receipt') {
      box(items, x - 1.4, y - 1.2, 0, 2.8, 2.4, 1.25, '#cedbc3', {
        windows: { cols: 2, rows: 1 },
      });
      box(items, x - 1.65, y - 1.4, 1.25, 3.3, 2.8, 0.2, C.success);
    } else {
      box(items, x - 1.5, y - 1, 0, 3, 2, 1.1, '#d8bfaf');
      box(items, x - 1.7, y - 1.2, 1.1, 3.4, 2.4, 0.2, C.fail);
    }
  }
  function road(ctx, route, color, width, dashed) {
    var pts = route.pts.map(function (p) {
      return Iso.project(p.x, p.y, p.z);
    });
    ctx.strokeStyle = color;
    ctx.lineWidth = width;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    if (dashed) ctx.setLineDash([9, 10]);
    Iso.polyLine(ctx, pts, false);
    ctx.setLineDash([]);
  }
  function drawCart(ctx, v, m) {
    var c = C[m.input.family];
    ctx.fillStyle = 'rgba(70,65,55,.14)';
    Iso.disc(ctx, v.x, v.y, 0, 1.35);
    Iso.orientedBox(ctx, {
      x: v.x,
      y: v.y,
      z: 0.12,
      len: 3,
      wid: 1.45,
      h: 0.32,
      hx: v.dx,
      hy: v.dy,
      color: '#58636a',
    });
    Iso.orientedBox(ctx, {
      x: v.x + v.dx * 0.98,
      y: v.y + v.dy * 0.98,
      z: 0.44,
      len: 0.85,
      wid: 1.4,
      h: 0.85,
      hx: v.dx,
      hy: v.dy,
      color: c,
    });
    Iso.orientedBox(ctx, {
      x: v.x + v.dx * 1.07,
      y: v.y + v.dy * 1.07,
      z: 1.29,
      len: 0.56,
      wid: 1.2,
      h: 0.08,
      hx: v.dx,
      hy: v.dy,
      color: '#d4e3e2',
    });
    var tokens = m.scan ? m.scan.tokens : [];
    for (var i = 0; i < tokens.length; i++) {
      var a = -1.03 + (i % 6) * 0.29,
        b = -0.46 + Math.floor(i / 6) * 0.31;
      Iso.orientedBox(ctx, {
        x: v.x + v.dx * a - v.dy * b,
        y: v.y + v.dy * a + v.dx * b,
        z: 0.44,
        len: 0.24,
        wid: 0.23,
        h: 0.35,
        hx: v.dx,
        hy: v.dy,
        color:
          m.chart && i <= m.chart.column && m.chart.sets[i + 1].length
            ? '#82b0ae'
            : '#f3deb6',
      });
    }
    if (!m.scan)
      Iso.orientedBox(ctx, {
        x: v.x - v.dx * 0.45,
        y: v.y - v.dy * 0.45,
        z: 0.44,
        len: 1.6,
        wid: 1.1,
        h: 0.12,
        hx: v.dx,
        hy: v.dy,
        color: '#f7edce',
      });
  }
  function plates(ctx, cam, active, v, m) {
    ctx.setTransform(cam.dpr, 0, 0, cam.dpr, 0, 0);
    var W = ctx.canvas.width / cam.dpr,
      H = ctx.canvas.height / cam.dpr,
      placed = [],
      queue = [];
    function add(x, y, z, title, sub, color, priority) {
      var p = Iso.project(x, y, z);
      queue.push({
        x: p.x * cam.scale + cam.ox,
        y: p.y * cam.scale + cam.oy,
        title: title,
        sub: sub,
        color: color,
        priority: priority,
      });
    }
    var live = m.scan ? m.scan.tokens.length + ' tokens' : 'rule + input';
    if (m.chart)
      live +=
        m.chart.column < 0
          ? ' · seeded'
          : ' · chart ' + m.chart.column + '/' + m.chart.tokens.length;
    if (m.output !== null)
      live = 'accepted · ' + global.ParserModel.names[m.input.family];
    if (m.error)
      live = m.recognized ? 'syntax ✓ · result refused' : 'parse stopped';
    add(v.x, v.y, 1.6, live, '', C[m.input.family], 0);
    World.districts.forEach(function (d) {
      if (
        !labels ||
        (cam.scale < 0.34 &&
          d.id !== active &&
          [
            'native',
            'parser',
            'dgd',
            'grammar',
            'chart',
            'nativeOut',
            'parserOut',
            'dgdOut',
            'receipt',
          ].indexOf(d.id) < 0)
      )
        return;
      add(
        d.bx,
        d.by,
        2.25,
        d.name,
        d.id === active ? d.tag : '',
        d.color,
        d.id === active ? 1 : 2,
      );
    });
    queue.sort(function (a, b) {
      return a.priority - b.priority;
    });
    queue.forEach(function (p) {
      ctx.font = '600 12px system-ui';
      var w = ctx.measureText(p.title).width + 22;
      if (p.sub) {
        ctx.font = '10px ui-monospace, monospace';
        w = Math.max(w, ctx.measureText(p.sub).width + 22);
      }
      var h = p.sub ? 42 : 27,
        x = p.x - w / 2,
        y = p.y - h - 13;
      if (x + w < 8 || x > W - 8 || y + h < 70 || y > H - 45) return;
      for (var n = 0; n < 9; n++) {
        if (
          !placed.some(function (r) {
            return (
              x < r.x + r.w + 5 &&
              x + w + 5 > r.x &&
              y < r.y + r.h + 5 &&
              y + h + 5 > r.y
            );
          })
        )
          break;
        y -= h + 6;
      }
      if (n === 9 || y < 62) return;
      placed.push({ x: x, y: y, w: w, h: h });
      ctx.strokeStyle = Iso.rgba(p.color, 0.6);
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(p.x, p.y);
      ctx.lineTo(p.x, y + h);
      ctx.stroke();
      ctx.fillStyle = '#fffdf4';
      ctx.beginPath();
      ctx.roundRect(x, y, w, h, 4);
      ctx.fill();
      ctx.stroke();
      ctx.fillStyle = p.color;
      ctx.font = '600 12px system-ui';
      ctx.textAlign = 'center';
      ctx.fillText(p.title, p.x, y + 18);
      if (p.sub) {
        ctx.fillStyle = '#626568';
        ctx.font = '10px ui-monospace, monospace';
        ctx.fillText(p.sub, p.x, y + 33);
      }
    });
  }
  function draw(canvas, cam, clock, active, hover) {
    var ctx = canvas.getContext('2d'),
      m = Sim.state.model;
    if (!m) return;
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.fillStyle = '#f1eee5';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.setTransform(
      cam.dpr * cam.scale,
      0,
      0,
      cam.dpr * cam.scale,
      cam.dpr * cam.ox,
      cam.dpr * cam.oy,
    );
    ctx.fillStyle = '#e6e5d5';
    Iso.poly(
      ctx,
      [
        [0, 0],
        [World.GW, 0],
        [World.GW, World.GH],
        [0, World.GH],
      ].map(function (p) {
        return Iso.project(p[0], p[1], 0);
      }),
    );
    ctx.strokeStyle = 'rgba(133,138,122,.12)';
    ctx.lineWidth = 1;
    for (var x = 0; x <= World.GW; x += 2)
      Iso.polyLine(
        ctx,
        [Iso.project(x, 0, 0), Iso.project(x, World.GH, 0)],
        false,
      );
    for (var y = 0; y <= World.GH; y += 2)
      Iso.polyLine(
        ctx,
        [Iso.project(0, y, 0), Iso.project(World.GW, y, 0)],
        false,
      );
    World.districts.forEach(function (d) {
      ctx.fillStyle = Iso.rgba(d.color, d.id === active ? 0.13 : 0.055);
      Iso.disc(ctx, d.bx, d.by, 0, 3.4);
    });
    Object.keys(World.routes).forEach(function (name) {
      if (/Fail$/.test(name)) return;
      road(ctx, World.routes[name], '#c8c8ba', 50, false);
      road(ctx, World.routes[name], '#faf5e9', 43, false);
      var color = /^native/.test(name)
        ? C.native
        : /^parser/.test(name)
          ? C.parser
          : /^dgd/.test(name)
            ? C.dgd
            : C.shared;
      road(ctx, World.routes[name], Iso.rgba(color, 0.45), 2.3, true);
    });
    road(
      ctx,
      World.routes[Sim.van.routeName],
      Iso.rgba(m.error ? C.fail : C[m.input.family], 0.65),
      5,
      false,
    );
    var items = [];
    World.props.forEach(function (p) {
      if (p.kind === 'tree') tree(items, p.x, p.y, p.height);
      else
        box(
          items,
          p.x - 0.65,
          p.y - 0.65,
          0,
          1.3,
          1.3,
          p.height * 0.5,
          '#d0d0bf',
        );
    });
    World.districts.forEach(function (d) {
      // A landmark's roof and contents share its ground footprint.
      var pieces = [];
      landmark(pieces, d, m);
      items.push({
        depth: d.bx + d.by,
        draw: function (c) {
          pieces.forEach(function (piece) {
            piece.draw(c);
          });
        },
      });
    });
    var v = Sim.vanPosition();
    items.push({
      depth: v.x + v.y,
      draw: function (c) {
        drawCart(c, v, m);
      },
    });
    items.sort(function (a, b) {
      return a.depth - b.depth;
    });
    items.forEach(function (item) {
      item.draw(ctx);
    });
    if (hover) {
      var d = World.byId[hover];
      ctx.fillStyle = Iso.rgba(d.color, 0.12);
      Iso.disc(ctx, d.x, d.y, 0.03, 2.5);
    }
    plates(ctx, cam, active, v, m);
  }
  global.Renderer = {
    draw: draw,
    setLabels: function (value) {
      labels = value;
    },
  };
})(window);
