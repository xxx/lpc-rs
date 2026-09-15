(function (global) {
  'use strict';
  var Iso = global.Iso;
  var C = {
    teal: '#28776f',
    blue: '#527e9d',
    gold: '#b58b39',
    rust: '#ae5b48',
    violet: '#81759c'
  };
  var OUT = Iso.makeRoute([
    [8, 9],
    [18, 9],
    [28, 9],
    [38, 9],
    [38, 17],
    [38, 25]
  ]);
  var RETRY = Iso.makeRoute([
    [38, 25],
    [38, 32],
    [24, 32],
    [8, 32],
    [8, 9]
  ]);
  var SUCCESS = Iso.makeRoute([
    [38, 25],
    [29, 25],
    [20, 25],
    [17, 25]
  ]);
  function station(route, index, id) {
    return { id: id, dist: route.cum[index], dwell: 1.8 };
  }
  var stations = {
    out: [
      station(OUT, 0, 'snapshot'),
      station(OUT, 1, 'read'),
      station(OUT, 2, 'write'),
      station(OUT, 3, 'effects'),
      station(OUT, 4, 'rival'),
      station(OUT, 5, 'commit')
    ],
    retry: [station(RETRY, 1, 'retry'), station(RETRY, 2, 'admission')],
    success: [station(SUCCESS, 1, 'deliver'), station(SUCCESS, 2, 'finish')]
  };
  var districts = [
    {
      id: 'snapshot',
      name: 'Snapshot Archive',
      x: 8,
      y: 9,
      bx: 7,
      by: 3.5,
      r: 4,
      color: C.blue,
      tag: 'Begin an attempt',
      short: 'An attempt starts with a fixed view of the committed world.',
      body: 'The blue cart is owning task A: one driver-started action, such as a player command. It takes a snapshot of the world and remembers its base version. Gold starts at 10 in this example. Other tasks can keep working while A evaluates; their commits do not change this snapshot. The real driver shares an immutable map instead of copying every object. Each retry comes back here for a fresh view and starts the action again.',
      source: 'snapshot.rs'
    },
    {
      id: 'read',
      name: 'Cell Library',
      x: 18,
      y: 9,
      bx: 17,
      by: 3.5,
      r: 4,
      color: C.blue,
      tag: 'Track dependencies',
      short: 'Reading committed gold makes gold a dependency of this attempt.',
      body: 'In Read + write mode, A reads gold from its snapshot and puts that cell in its read set. The blue tile on the cart counts this dependency: 1 tracked cell. The committer will later check whether anyone wrote it after the base version. Blind assignment and unread merge mode skip this read. An attempt also sees its own writes first; reading a value supplied entirely by its own write adds no dependency on the committed world.',
      source: 'changeset.rs'
    },
    {
      id: 'write',
      name: 'Private Workshop',
      x: 28,
      y: 9,
      bx: 27,
      by: 3.5,
      r: 4,
      color: C.gold,
      tag: 'Stage local changes',
      short:
        'The workshop changes the attempt; the public world stays as it was.',
      body: 'With the default inputs, A calculates 10 + 4 and privately writes 14. A nested apply shares this same transaction, so it can see the staged value and succeeds or rolls back with its owner. The gold tile on the cart is one pending write. Switch the operation to compare a blind assignment with a merge. A merge carries add(4), leaving its final value to the committer; reading that pending merge would turn it into a tracked read and a local write.',
      source: 'mod.rs'
    },
    {
      id: 'effects',
      name: 'Outbox Depot',
      x: 38,
      y: 9,
      bx: 37,
      by: 3.5,
      r: 4,
      color: C.violet,
      tag: 'Buffer a receipt',
      short:
        'Output waits inside the attempt until its changes have committed.',
      body: 'A records 1 receipt saying what it did. The violet envelope on the cart is that actual buffered effect, and the delivered-output panel is still empty. This matters because evaluation may run again. Sending the receipt now would announce work that could be discarded, then announce it again on a retry. The driver also defers effects such as physical object-map changes and call-out scheduling. Here, one text receipt stands in for that broader family of commit-dependent effects.',
      source: 'effects.rs'
    },
    {
      id: 'rival',
      name: 'Neighbour Yard',
      x: 38,
      y: 17,
      bx: 44,
      by: 14,
      r: 4,
      color: C.rust,
      tag: 'Another task commits',
      short: 'B can commit while A is still carrying its older snapshot.',
      body: 'The terracotta cart belongs to another owning task, B. By default B reads gold, adds 3, and commits through the same committer before A submits. The world now holds 13; A still carries its own proposal of 14 from base v0. Change B to write bells and watch the world version advance without invalidating A’s gold read. These are real model operations in a chosen schedule. Road travel is teaching time, and does not measure the driver’s parallel execution.',
      source: 'committer.rs'
    },
    {
      id: 'commit',
      name: 'Committer Hall',
      x: 38,
      y: 25,
      bx: 44,
      by: 23,
      r: 4.5,
      color: C.teal,
      tag: 'Validate + publish',
      short:
        'One committer checks dependencies and publishes accepted changes atomically.',
      body: 'For each commit newer than A’s base, the committer intersects the written cells with A’s read set. With the defaults, B wrote the 1 cell A read, so A is rejected and its buffers are discarded. If validation passes, writes apply in commit order and merges fold onto current values before publication. A blind write has no gold read to invalidate. No separate write-write check runs here. Validation and publication are one indivisible operation; the green branch means both have succeeded.',
      source: 'committer.rs'
    },
    {
      id: 'retry',
      name: 'Return Depot',
      x: 38,
      y: 32,
      bx: 44,
      by: 31.5,
      r: 4,
      color: C.rust,
      tag: 'Discard the attempt',
      short:
        'A conflict throws away the attempt’s work and takes the return road.',
      body: 'The cart’s compartments are empty because the rejected attempt lost its read set, private changes, and buffered receipt. Delivered output remains at 0. The owning action is still alive: it will evaluate again from its entry point. The red road makes that repetition visible. A second attempt must recompute from a new snapshot; resubmitting the old proposal would preserve the stale decision. In the real driver, retries and admission waits share the action’s original evaluation allowance.',
      source: 'retry.rs'
    },
    {
      id: 'admission',
      name: 'Retry Turnstile',
      x: 24,
      y: 32,
      bx: 24,
      by: 37,
      r: 4,
      color: C.gold,
      tag: 'Take a cell turn',
      short:
        'After a read conflict, A takes a retry turn for the conflicting cell.',
      body: 'The current driver admits 1 owning retry at a time per conflicting cell, in FIFO order. A takes its turn before opening the next snapshot, then holds it until the commit reply. Nested applies share their owner’s turn. Our queue has only A waiting, so it can enter immediately. First attempts still run freely and can invalidate an admitted retry. Raise Competing writes to see that happen again. Other rejection kinds use backoff; the committer remains the authority that accepts every commit.',
      source: 'admission.rs'
    },
    {
      id: 'deliver',
      name: 'Delivery Office',
      x: 29,
      y: 25,
      bx: 30,
      by: 19,
      r: 4,
      color: C.violet,
      tag: 'Release committed output',
      short: 'Only the successful attempt delivers its buffered receipt.',
      body: 'The world has already changed at Committer Hall. Now the attempt runner delivers the successful attempt’s 1 receipt and empties its outbox. Rejected attempts never reach this road. The retry turn has already been released, before effect delivery, so another retry can proceed. This model uses a reliable in-memory output list. Real effect delivery may fail after state commits, and concurrent tasks’ output order is not guaranteed to match commit order. The guarantee illustrated here is that discarded attempts do not emit their buffered output.',
      source: 'retry.rs'
    },
    {
      id: 'finish',
      name: 'World Square',
      x: 20,
      y: 25,
      bx: 20,
      by: 19,
      r: 4,
      color: C.teal,
      tag: 'One completed action',
      short:
        'Several evaluations can produce one committed action and one receipt.',
      body: 'With the starting inputs, B changes gold from 10 to 13. A’s first proposal of 14 is rejected, then its retry computes 13 + 4 and commits 17. Only one A receipt appears. Try B writing bells: A can commit against its older snapshot because the gold dependency is intact. Try a blind assignment: A overwrites gold in commit order. Try an unread merge: add(4) combines with the current gold at commit time. The branch comes from validation of actual state, not a scripted outcome.',
      source: 'committer.rs'
    }
  ];
  var byId = Object.create(null);
  districts.forEach(function (d, i) {
    d.number = i + 1;
    byId[d.id] = d;
  });
  var props = [];
  function distance(x, y) {
    var best = Infinity;
    [OUT, RETRY, SUCCESS].forEach(function (route) {
      route.segs.forEach(function (s) {
        var dx = s.b.x - s.a.x,
          dy = s.b.y - s.a.y;
        var u = Math.max(
          0,
          Math.min(1, ((x - s.a.x) * dx + (y - s.a.y) * dy) / (s.len * s.len))
        );
        best = Math.min(
          best,
          Math.hypot(x - s.a.x - u * dx, y - s.a.y - u * dy)
        );
      });
    });
    return best;
  }
  function build() {
    props.length = 0;
    for (var x = 3; x < 48; x += 4)
      for (var y = 3; y < 40; y += 4) {
        if (distance(x, y) < 3.3 || Iso.hash2(x, y, 9) < 0.4) continue;
        if (
          districts.some(function (d) {
            return Math.hypot(x - d.bx, y - d.by) < 4;
          })
        )
          continue;
        props.push({
          x: x,
          y: y,
          kind: 'tree',
          size: 0.65 + Iso.hash2(x, y, 2) * 0.35
        });
      }
  }
  function readSeconds(id) {
    var d = byId[id];
    return d
      ? Math.min(
          26,
          Math.max(9, (d.short + ' ' + d.body).split(/\s+/).length / 3.8 + 3.5)
        )
      : 9;
  }
  global.World = {
    GW: 50,
    GH: 42,
    routes: { out: OUT, retry: RETRY, success: SUCCESS },
    stations: stations,
    districts: districts,
    districtById: byId,
    stationToDistrict: {},
    readSeconds: readSeconds,
    props: props,
    palette: C,
    build: build
  };
})(window);
