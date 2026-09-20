(function (global) {
  'use strict';
  var Iso = global.Iso, World = global.World, Sim = global.Sim, GC = global.GC;
  var P = Iso.project, C = World.palette, ctx, cam, labels = [], showLabels = true;
  function drawGround(w, h) {
    ctx.fillStyle = '#e6ebdf'; ctx.fillRect(0, 0, w, h);
    ctx.setTransform(cam.scale * cam.dpr, 0, 0, cam.scale * cam.dpr, cam.ox * cam.dpr, cam.oy * cam.dpr);
    var corners = [P(0, 0, 0), P(World.GW, 0, 0), P(World.GW, World.GH, 0), P(0, World.GH, 0)];
    ctx.fillStyle = '#d1d9bb'; Iso.poly(ctx, corners);
    ctx.strokeStyle = '#c1ccb0'; ctx.lineWidth = 0.5;
    for (var x = 0; x <= World.GW; x += 2) Iso.polyLine(ctx, [P(x, 0), P(x, World.GH)], false);
    for (var y = 0; y <= World.GH; y += 2) Iso.polyLine(ctx, [P(0, y), P(World.GW, y)], false);
    ctx.strokeStyle = '#95aa91'; ctx.lineWidth = 2; Iso.polyLine(ctx, corners, true);
    ctx.fillStyle = '#ebe6d5';
    Iso.poly(ctx, [P(27.6, 12), P(34.5, 12), P(34.5, 18.4), P(27.6, 18.4)]);
  }
  function drawZones(active) {
    World.districts.forEach(function (d) {
      ctx.fillStyle = Iso.rgba(d.color, d.id === active ? 0.13 : 0.05);
      Iso.disc(ctx, d.x, d.y, 0.01, d.r);
    });
  }
  function roadQuad(a, b, width, dz) {
    var dx = b.x - a.x, dy = b.y - a.y;
    var len = Math.hypot(dx, dy) || 1;
    var nx = -dy / len * width / 2, ny = dx / len * width / 2;
    var za = (a.z || 0) + (dz || 0), zb = (b.z || 0) + (dz || 0);
    Iso.poly(ctx, [
      P(a.x + nx, a.y + ny, za), P(b.x + nx, b.y + ny, zb),
      P(b.x - nx, b.y - ny, zb), P(a.x - nx, a.y - ny, za)
    ]);
  }

  function deckFascia(a, b, width, thick) {
    var dx = b.x - a.x, dy = b.y - a.y;
    var len = Math.hypot(dx, dy) || 1;
    var nx = -dy / len * width / 2, ny = dx / len * width / 2;

    var s = (nx + ny) > 0 ? 1 : -1;
    var ax = a.x + nx * s, ay = a.y + ny * s;
    var bx = b.x + nx * s, by = b.y + ny * s;
    var za = a.z || 0, zb = b.z || 0;
    ctx.fillStyle = '#a49c8c';
    Iso.poly(ctx, [
      P(ax, ay, za), P(bx, by, zb), P(bx, by, zb - thick), P(ax, ay, za - thick)
    ]);
  }

  function drawRoute(route, opts) {
    var width = opts.width, i, s;
    var elevated = false;
    for (i = 0; i < route.segs.length; i++) {
      if ((route.segs[i].a.z || 0) > 0.25) { elevated = true; break; }
    }

    if (elevated) {
      ctx.fillStyle = 'rgba(90,88,78,0.16)';
      for (i = 0; i < route.segs.length; i++) {
        s = route.segs[i];
        Iso.ribbon(ctx, s.a.x + 0.5, s.a.y + 0.5, s.b.x + 0.5, s.b.y + 0.5, width, 0.02);
      }
    }

    if (elevated) {
      for (i = 0; i < route.segs.length; i++) {
        s = route.segs[i];
        if ((s.a.z || 0) > 0.25 || (s.b.z || 0) > 0.25) deckFascia(s.a, s.b, width, 0.34);
      }
    }

    ctx.fillStyle = opts.shoulder || C.road;
    for (i = 0; i < route.segs.length; i++) {
      s = route.segs[i];
      roadQuad(s.a, s.b, width + 0.5, 0);
      Iso.disc(ctx, s.a.x, s.a.y, s.a.z || 0, (width + 0.5) / 2);
    }
    var last = route.pts[route.pts.length - 1];
    Iso.disc(ctx, last.x, last.y, last.z || 0, (width + 0.5) / 2);

    ctx.fillStyle = opts.surface || C.roadTop;
    for (i = 0; i < route.segs.length; i++) {
      s = route.segs[i];
      roadQuad(s.a, s.b, width, 0.005);
      Iso.disc(ctx, s.a.x, s.a.y, (s.a.z || 0) + 0.005, width / 2);
    }
    Iso.disc(ctx, last.x, last.y, (last.z || 0) + 0.005, width / 2);

    ctx.strokeStyle = opts.dash || 'rgba(96,90,78,0.35)';
    ctx.lineWidth = 1.3;
    ctx.setLineDash([6, 7]);
    ctx.beginPath();
    for (i = 0; i < route.pts.length; i++) {
      var p = P(route.pts[i].x, route.pts[i].y, (route.pts[i].z || 0) + 0.01);
      if (i === 0) ctx.moveTo(p.x, p.y); else ctx.lineTo(p.x, p.y);
    }
    ctx.stroke();
    ctx.setLineDash([]);
  }

  function drawRoads() {
    Object.keys(World.routes).forEach(function (name) {
      drawRoute(World.routes[name], { width: 1.65,
        surface: name === 'refuse' ? '#e6cfc0' : name === 'trace' ? '#dce5ce' : C.roadTop,
        dash: name === 'refuse' ? '#b7856e' : '#a3aa90' });
    });
    // The arrows make the work-loop direction visible even while paused.
    [[31, 11, 1, 0], [35, 15, 0, 1], [31, 19, -1, 0], [27, 15, 0, -1]].forEach(function (a) {
      var x = a[0], y = a[1], dx = a[2], dy = a[3];
      ctx.strokeStyle = '#6f967e'; ctx.lineWidth = 2;
      Iso.polyLine(ctx, [P(x - dx * .5 - dy * .3, y - dy * .5 + dx * .3, .04), P(x, y, .04),
        P(x - dx * .5 + dy * .3, y - dy * .5 - dx * .3, .04)], false);
    });
  }
  function box(x, y, z, w, d, h, color) { Iso.box(ctx, {x:x,y:y,z:z,w:w,d:d,h:h,color:color}); }
  function base(b, w, d) { box(b.x-w/2,b.y-d/2,0,w,d,.16,'#c1c3ac'); }
  function roofSign(b, text, z) {
    var p = P(b.x, b.y, z);
    ctx.font = '600 11px ui-monospace, monospace'; ctx.textAlign = 'center'; ctx.fillStyle = '#f9f7ee';
    ctx.fillText(text, p.x, p.y);
  }
  function drawRegistry(b) {
    base(b,3.8,2.8);
    box(b.x-1.7,b.y-1.1,.16,3.4,2.2,1.8,'#96b0b6');
    Iso.gableRoof(ctx,{x:b.x-1.85,y:b.y-1.25,z:1.96,w:3.7,d:2.5,h:.7,color:'#49787d'});
    for(var i=0;i<3;i++) box(b.x-1.15+i*.85,b.y+1.12,.45,.44,.04,.8,'#e6ecdc');
    roofSign(b,'ROOTS',2.9);
  }
  function drawGate(b) {
    base(b,3.5,2.4);
    box(b.x-1.4,b.y-.8,.16,2.8,1.6,1.65,'#d4b5a0');
    box(b.x-1.6,b.y-.9,1.8,3.2,1.8,.3,'#b36a53');
    var m=Sim.state.model;
    for(var i=0;i<3;i++) Iso.cylinder(ctx,{x:b.x-1+i,y:b.y+.9,z:.25,r:.23,h:i<m.options.pins?1.6:.25,color:i<m.options.pins?C.coral:'#acc6a2'});
    roofSign(b,m.options.pins+' PINS',2.4);
  }
  function drawDepot(b) {
    base(b,3.5,2.6);
    box(b.x-1.5,b.y-.8,.2,3,1.8,.65,'#c9b57e');
    Sim.state.model.roots.forEach(function(r,i){box(b.x-1.3+i*.65,b.y-.65,.85,.48,1.1,.65,'#ddc786');});
    box(b.x-1.65,b.y-.8,.2,.18,1.8,2.1,'#a39569');
    box(b.x+1.45,b.y-.8,.2,.18,1.8,2.1,'#a39569');
    box(b.x-1.75,b.y-.9,2.3,3.5,2,.2,'#bda571');
    roofSign(b,'LIFO',2.9);
  }
  function drawStack(b) {
    base(b,2.8,2.8);
    box(b.x-.8,b.y-.8,.2,1.6,1.6,.25,'#8d9984');
    var n=Sim.state.model.stack.length;
    for(var i=0;i<n;i++) box(b.x-.7,b.y-.7,.45+i*.23,1.4,1.4,.18,i%2?'#c4a55e':'#dfc57f');
    roofSign(b,n+' WAITING',Math.max(1,n*.23+.9));
  }
  function drawFoundry(b) {
    base(b,3.2,3.2);
    Iso.cylinder(ctx,{x:b.x,y:b.y,z:.2,r:1.2,h:1.6,color:'#75a390',ring:.45});
    Iso.cylinder(ctx,{x:b.x,y:b.y,z:1.8,r:.83,h:.55,color:'#b6d0ac'});
    box(b.x-.25,b.y-.25,2.35,.5,.5,.7,C.teal);
    roofSign(b,'TRACE',3.6);
  }
  function drawYard(b) {
    base(b,4,3.4);
    box(b.x-1.8,b.y-1.4,.2,3.6,2.8,.15,'#ba9e87');
    var n=Sim.state.model.removed.length;
    for(var i=0;i<n;i++) box(b.x-1.6+i%5*.65,b.y-1.15+Math.floor(i/5)*.65,.36,.52,.52,.3,'#b88066');
    box(b.x-1.8,b.y-1.5,.35,3.6,.15,.7,'#caad93');
    box(b.x-1.9,b.y-1.5,.35,.15,3,.7,'#caad93');
  }
  function drawOffice(b) {
    base(b,3.6,2.8);
    box(b.x-1.5,b.y-1,.16,3,2,1.6,b.kind==='report'?'#9dafbd':'#cbb0a0');
    Iso.gableRoof(ctx,{x:b.x-1.65,y:b.y-1.15,z:1.76,w:3.3,d:2.3,h:.7,color:b.color});
    box(b.x-.35,b.y+1.01,.16,.7,.04,1.2,'#687a6d');
    roofSign(b,b.kind==='report'?'REPORT':'REFUSED',2.9);
  }
  var KIND={request:drawRegistry,gate:drawGate,roots:drawDepot,take:drawStack,trace:drawFoundry,sweep:drawYard,report:drawOffice,refuse:drawOffice};
  function drawTree(p) {
    var n=Iso.hash2(p.x,p.y,12);
    Iso.cylinder(ctx,{x:p.x,y:p.y,z:0,r:.12,h:1.3,color:'#9b9573'});
    Iso.cylinder(ctx,{x:p.x,y:p.y,z:1.1,r:.65+n*.2,h:.7,color:'#94aa81'});
    Iso.cylinder(ctx,{x:p.x,y:p.y,z:1.8,r:.45+n*.15,h:.45,color:'#a6b990'});
  }
  function drawCell(o) {
    var m=Sim.state.model, status=GC.status(m,o.id), p=World.cellPosition(o.index);
    var colors={marked:'#65a58c',unmarked:'#cbbd97',retained:'#8c9fb6',reclaimed:'#e4dccc'};
    box(p.x-.46,p.y-.46,.04,.92,.92,.11,'#b9b8a2');
    box(p.x-.38,p.y-.38,.15,.76,.76,status==='reclaimed'?.05:.6,colors[status]);
    if(cam.scale>.65){
      var t=P(p.x,p.y,status==='reclaimed'?.3:.87);
      ctx.fillStyle='#344a3e';ctx.textAlign='center';ctx.font='9px ui-monospace,monospace';
      ctx.fillText(String(o.index+1),t.x,t.y);
    }
  }
  function drawVan(v) {
    var m=Sim.state.model, n=GC.counts(m), hx=v.dx,hy=v.dy,px=-hy,py=hx,z=v.z||0;
    ctx.fillStyle='#34432d28';Iso.disc(ctx,v.x,v.y,z+.02,1.1);
    function part(x,y,zv,len,wid,h,color){Iso.orientedBox(ctx,{x:x,y:y,z:z+zv,hx:hx,hy:hy,len:len,wid:wid,h:h,color:color});}
    part(v.x,v.y,.2,2.6,1.4,.3,'#536d60');
    part(v.x-hx*.35,v.y-hy*.35,.5,1.7,1.3,.5,'#eee8cd');
    part(v.x+hx*.85,v.y+hy*.85,.5,.85,1.22,.85,C.teal);
    part(v.x+hx*.9,v.y+hy*.9,1.35,.6,1.1,.12,'#8eb5a2');
    var count=m.stack.length;
    for(var i=0;i<count;i++){
      var col=i%3,row=Math.floor(i/3)%2,level=Math.floor(i/6);
      part(v.x-hx*(.92-col*.48)+px*(row?.32:-.32),v.y-hy*(.92-col*.48)+py*(row?.32:-.32),1+level*.21,.4,.5,.17,'#d6b75e');
    }
    if(m.current)part(v.x+hx*.85,v.y+hy*.85,1.5,.45,.5,.24,'#e5bf64');
    var side=px+py>0?1:-1,gx=v.x-hx*.35+px*side*.68,gy=v.y-hy*.35+py*side*.68,frac=n.marked/n.before;
    part(gx,gy,.6,1.5,.04,.28,'#718071');
    if(frac)part(gx-hx*.75*(1-frac),gy-hy*.75*(1-frac),.64,1.5*frac,.06,.2,'#9ed8a6');
    ctx.fillStyle='#354139';
    [[.8,.65],[.8,-.65],[-.8,.65],[-.8,-.65]].forEach(function(o){Iso.disc(ctx,v.x+hx*o[0]+px*o[1],v.y+hy*o[0]+py*o[1],z+.15,.22);});
  }
  function drawLabels() {

    ctx.setTransform(cam.dpr, 0, 0, cam.dpr, 0, 0);
    ctx.textBaseline = 'middle';

    labels.sort(function (a, b) { return (b.pri || 0) - (a.pri || 0); });

    var van = Sim.vanPosition(), footprint = P(van.x, van.y, van.z || 0);
    var placed = [{ ax: footprint.x * cam.scale + cam.ox,
      sy: footprint.y * cam.scale + cam.oy - 20 * cam.scale,
      boxW: 100 * cam.scale, boxH: 65 * cam.scale }];
    var i;
    for (i = 0; i < labels.length; i++) {
      var L = labels[i];
      var p = P(L.x, L.y, L.z);
      L.ax = p.x * cam.scale + cam.ox;
      L.ay = p.y * cam.scale + cam.oy;

      L.px = (L.size || 12) * Math.min(1.15, Math.max(0.92, cam.scale));
      ctx.font = (L.bold ? '600 ' : '') + L.px + 'px ' + fontOf(L);
      var wpx = ctx.measureText(L.text).width;
      ctx.font = (L.px * 0.85) + 'px ui-monospace, Menlo, Consolas, monospace';
      var subw = L.sub ? ctx.measureText(L.sub).width : 0;
      L.boxW = Math.max(wpx, subw) + 16;
      L.boxH = L.sub ? L.px * 2.4 : L.px * 1.75;

      L.sy = L.lift ? L.ay - L.lift - L.boxH / 2 : L.ay;

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
      if (Math.abs(L.ax - o.ax) < (L.boxW + o.boxW) / 2 + 2 &&
          Math.abs(L.sy - o.sy) < (L.boxH + o.boxH) / 2 + 2) return true;
    }
    return false;
  }

  function drawPlate(L) {
    var ax = L.ax, ay = L.ay, sy = L.sy, size = L.px;
    var boxW = L.boxW, boxH = L.boxH;
    ctx.textAlign = 'center';
    ctx.font = (L.bold ? '600 ' : '') + size + 'px ' + fontOf(L);

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

    ctx.fillStyle = 'rgba(96,84,66,0.26)';
    roundRect(ax - boxW / 2 + 1, sy - boxH / 2 + 2.5, boxW, boxH, 5);
    ctx.fill();

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
      ctx.font = (size * 0.85) + 'px ui-monospace, Menlo, Consolas, monospace';
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
    ctx=canvas.getContext('2d');cam=camera;labels.length=0;
    ctx.setTransform(cam.dpr,0,0,cam.dpr,0,0);
    drawGround(canvas.width/cam.dpr,canvas.height/cam.dpr);drawZones(active);drawRoads();
    var items=[],m=Sim.state.model;
    World.buildings.forEach(function(b){items.push({k:b.x+b.y,fn:KIND[b.kind],value:b});});
    World.props.forEach(function(p){items.push({k:p.x+p.y,fn:drawTree,value:p});});
    m.order.forEach(function(id,i){var p=World.cellPosition(i);items.push({k:p.x+p.y,fn:drawCell,value:{id:id,index:i}});});
    var v=Sim.vanPosition();items.push({k:v.x+v.y+.2,fn:drawVan,value:v});
    items.sort(function(a,b){return a.k-b.k;});items.forEach(function(o){o.fn(o.value);});
    if(showLabels)World.districts.forEach(function(d){
      var on=d.id===active||d.id===hover;
      if(cam.scale<.34&&!on)return;
      labels.push({x:d.x,y:d.y,z:2.8,lift:8,text:d.name,sub:on?d.tag:null,
        tint:d.color,size:on?15:12,bold:on,pri:on?2:1});
    });
    var n=GC.counts(m);
    labels.push({x:v.x,y:v.y,z:2.5,lift:8,text:m.admitted===false?'GC REFUSED':n.work+' WORK · '+n.marked+' MARKED',
      sub:m.current?GC.describe(m.current):m.swept?n.reclaimed+' cells reclaimed':'one tile = one work item',
      tint:m.admitted===false?C.coral:C.teal,size:12,mono:true,bold:true,pri:3});
    drawLabels();
  }
  global.Renderer={draw:draw,setLabels:function(v){showLabels=v;}};
})(window);
