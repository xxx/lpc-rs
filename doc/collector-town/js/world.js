(function (global) {
  'use strict';
  var Iso = global.Iso;
  var C = { ink: '#283e3d', teal: '#237c73', blue: '#547b95', gold: '#b38a3e',
    coral: '#ba6550', violet: '#847196', paper: '#f5f1e6', road: '#bcbca8', roadTop: '#e5dfcf' };
  var routes = {
    approach: Iso.makeRoute([[5, 7], [9, 7], [17, 7]]),
    admit: Iso.makeRoute([[17, 7], [24, 7], [27, 7], [27, 11]]),
    trace: Iso.makeRoute([[27, 11], [35, 11], [35, 19], [27, 19], [27, 11]]),
    sweep: Iso.makeRoute([[27, 11], [23, 11], [23, 25], [17, 25], [7, 25]]),
    refuse: Iso.makeRoute([[17, 7], [17, 15], [9, 15], [5, 15]])
  };
  function station(route, index, id) { return { dist: routes[route].cum[index], id: id, dwell: 1.8 }; }
  var stations = {
    approach: [station('approach', 1, 'request'), station('approach', 2, 'gate')],
    admit: [station('admit', 1, 'roots')],
    trace: [station('trace', 1, 'take'), station('trace', 2, 'trace')],
    sweep: [station('sweep', 3, 'sweep'), station('sweep', 4, 'report')],
    refuse: [station('refuse', 2, 'refuse')]
  };
  var districts = [
    { id: 'request', name: 'Root Registry', x: 9, y: 3.5, r: 3.1, color: C.blue,
      tag: 'Gather the starting points', short: 'A root is a reason to keep looking.',
      body: 'The driver gathers object-space cell IDs, live processes, and queued call-out functions before sending 1 GcPass message. These are starting points, not a list of everything alive. Our small world includes an object global pointing into an array and mapping. Untick “Global holds array” to cut that route into the graph. Choose a queued call-out to give the same closure an independent root. Changing the example starts a fresh request; it never edits a pass halfway through.',
      source: 'global', symbol: 'R' },
    { id: 'gate', name: 'Quiet Gate', x: 17, y: 3.5, r: 3, color: C.coral,
      tag: 'Check snapshot pins', short: 'Even one held snapshot sends collection down the refusal road.',
      body: 'The committer sums live snapshot pins across all versions. With 0 pins it can trace and sweep safely. With even 1 pin it replies GcRefused immediately, before changing the world. It does not wait for that transaction or take a global lock. Try raising “Snapshot pins” and watch the cart turn left. The accepted road that follows expands one atomic committer operation for teaching: no transaction can start or commit between its illustrated stops.',
      source: 'committer', symbol: '0' },
    { id: 'roots', name: 'Stack Depot', x: 24, y: 3.5, r: 3, color: C.gold,
      tag: 'Seed a LIFO work stack', short: 'The cart carries the actual work still to be traced.',
      body: 'The root entries are copied onto a last-in, first-out work stack. Our default request has 3 entries: two world cells and one live process. A process can be reached more than once; the marked cell set prevents its cells from being expanded again. Each gold tile on the cart represents 1 pending item, with the currently selected item set apart. The teal gauge measures marked world cells. Neither measure is bytes, collection time, nor a count of LPC objects.',
      source: 'committer', symbol: '≡' },
    { id: 'take', name: 'Workstack Tower', x: 35, y: 7, r: 3, color: C.gold,
      tag: 'Pop one work item', short: 'Another item means another lap around the same road.',
      body: 'The Rust collector uses an explicit work vector, so tracing does not consume a recursive call stack. Each lap pops 1 item: a world cell, a reference, a process, or a program image. The stack can grow when an item exposes more edges. Watch the selected item in the panel; it tells you what this lap actually visits. The road exits toward the sweep only when that stack is empty. This is a graph walk, not a fixed number of animation laps.',
      source: 'committer', symbol: '↓' },
    { id: 'trace', name: 'Reference Foundry', x: 39, y: 17, r: 3.2, color: C.teal,
      tag: 'Mark, then follow edges', short: 'Reachability follows the payload, including the less obvious links.',
      body: 'A new cell gets 1 mark before its edges enter the stack. Array elements and both mapping keys and values count. A function can lead to partial arguments, captured cells, and a retained program image; that image leads to its globals. Connection input callbacks and parser rules also expose functions. Already marked cells and visited image generations stop repeated expansion. That is why the linked cycle terminates, while a cycle with no path from a root receives no marks at all.',
      source: 'committer', symbol: '↗' },
    { id: 'sweep', name: 'Reclamation Yard', x: 17, y: 29, r: 3.3, color: C.coral,
      tag: 'Remove unmarked payload cells', short: 'An unreachable cycle has no special protection.',
      body: 'With 0 work items left, the committer scans its world. It removes unmarked Ref, Array, Mapping, and Image cells. The faded foundations in the courtyard are those removed cells. The default example reclaims an abandoned capture, an image slot, and a two-array cycle. Process identities, connection identities, and Rules cells are outside this removal filter. Our unrooted connection therefore stays without a mark. Object cleanup and destruction are separate behavior; collection does not unload every object that looks unused.',
      source: 'committer', symbol: '−' },
    { id: 'report', name: 'Receipt House', x: 7, y: 21, r: 3.4, color: C.blue,
      tag: 'Return the cell count', short: 'The receipt counts reclaimed world variables.',
      body: 'GcReport.reclaimed is the number of world cells removed by this pass. It is not a byte total, an RSS measurement, or the number of destroyed game objects. Compare 2 runs: cutting the global link makes its reachable subgraph collectible; keeping a callback rooted preserves that subgraph through the closure. The same graph walk computes both results. Road distances and reading pauses are illustrations. The source completes the admitted mark-and-sweep inside one committer message before replying.',
      source: 'committer', symbol: '✓' },
    { id: 'refuse', name: 'Return Office', x: 9, y: 11, r: 3, color: C.coral,
      tag: 'Refuse without collecting', short: 'A refusal leaves every world cell in place.',
      body: 'The reply records how many live snapshots prevented this pass. No cell was marked, and 0 cells were reclaimed. Lower “Snapshot pins” to request a new collection against a quiet world. The driver also offers gc_when_quiet: it retries refused passes after a caller-supplied delay, up to a caller-supplied limit. This tour shows one request at a time, so this road ends here. A long transaction can therefore delay reclamation even when much of the committed world is unreachable.',
      source: 'global', symbol: '↩' }
  ];
  var byId = {}, stationToDistrict = {};
  districts.forEach(function (d) { byId[d.id] = d; stationToDistrict[d.id] = d.id; });
  function readSeconds(id) {
    var d = byId[stationToDistrict[id] || id];
    return d ? Math.min(26, Math.max(9, (d.short + ' ' + d.body).split(/\s+/).length / 3.8 + 3.5)) : 9;
  }
  var buildings = [], props = [];
  function build() {
    buildings.length = 0; props.length = 0;
    districts.forEach(function (d) { buildings.push({ kind: d.id, x: d.x, y: d.y, color: d.color }); });
    [[3, 3], [4, 21], [3, 27], [12, 30], [21, 30], [41, 24], [40, 29], [30, 28], [31, 3], [40, 5]].forEach(function (p) {
      props.push({ kind: 'tree', x: p[0], y: p[1] });
    });
  }
  function cellPosition(index) { return { x: 28.4 + index % 5 * 1.24, y: 12.9 + Math.floor(index / 5) * 1.23 }; }
  global.World = { GW: 44, GH: 33, routes: routes, stations: stations, districts: districts,
    byId: byId, stationToDistrict: stationToDistrict, palette: C, readSeconds: readSeconds,
    buildings: buildings, props: props, build: build, cellPosition: cellPosition };
})(window);
