(function (global) {
  'use strict';
  var Iso = global.Iso;
  var C = {
    native: '#a35b35',
    parser: '#327c82',
    dgd: '#746397',
    shared: '#536b8d',
    success: '#507549',
    fail: '#ab534e',
    ink: '#343a3e',
    paper: '#eee9dc',
  };
  var routes = {},
    stations = {};
  function road(name, points, stops) {
    var route = Iso.makeRoute(points);
    routes[name] = route;
    stations[name] = (stops || []).map(function (s) {
      return { id: s[1], dist: route.cum[s[0]], dwell: 1.8 };
    });
  }
  road(
    'native',
    [
      [5, 7],
      [12, 7],
      [18, 14],
    ],
    [
      [0, 'native'],
      [2, 'groups'],
    ],
  );
  road(
    'parser',
    [
      [5, 22],
      [12, 22],
      [18, 14],
    ],
    [
      [0, 'parser'],
      [2, 'groups'],
    ],
  );
  road(
    'dgd',
    [
      [5, 37],
      [16, 37],
      [27, 21],
    ],
    [[0, 'dgd']],
  );
  road('lowered', [
    [18, 14],
    [27, 21],
  ]);
  road(
    'forge',
    [
      [27, 21],
      [37, 21],
    ],
    [
      [0, 'grammar'],
      [1, 'tokens'],
    ],
  );
  road(
    'chartIn',
    [
      [37, 21],
      [47, 21],
    ],
    [[1, 'chart']],
  );
  road(
    'loop',
    [
      [47, 21],
      [52, 21],
      [52, 30],
      [43, 30],
      [43, 21],
      [47, 21],
    ],
    [[5, 'chart']],
  );
  road(
    'forest',
    [
      [47, 21],
      [58, 21],
    ],
    [[1, 'trees']],
  );
  road(
    'nativeOut',
    [
      [58, 21],
      [58, 8],
      [66, 8],
    ],
    [[2, 'nativeOut']],
  );
  road(
    'parserOut',
    [
      [58, 21],
      [66, 21],
      [66, 30],
    ],
    [[2, 'parserOut']],
  );
  road(
    'dgdOut',
    [
      [58, 21],
      [58, 40],
      [66, 40],
    ],
    [[2, 'dgdOut']],
  );
  road(
    'nativeReceipt',
    [
      [66, 8],
      [73, 8],
      [73, 24],
    ],
    [[2, 'receipt']],
  );
  road(
    'parserReceipt',
    [
      [66, 30],
      [73, 30],
      [73, 24],
    ],
    [[2, 'receipt']],
  );
  road(
    'dgdReceipt',
    [
      [66, 40],
      [76, 40],
      [76, 24],
      [73, 24],
    ],
    [[3, 'receipt']],
  );
  road(
    'lexFail',
    [
      [37, 21],
      [37, 44],
      [73, 44],
    ],
    [[2, 'reject']],
  );
  road(
    'chartFail',
    [
      [47, 21],
      [47, 34],
      [73, 44],
    ],
    [[2, 'reject']],
  );
  road(
    'treeFail',
    [
      [58, 21],
      [61, 24],
      [61, 44],
      [73, 44],
    ],
    [[3, 'reject']],
  );
  road(
    'nativeFail',
    [
      [66, 8],
      [79, 8],
      [79, 44],
      [73, 44],
    ],
    [[3, 'reject']],
  );
  road(
    'parserFail',
    [
      [66, 30],
      [69, 34],
      [69, 44],
      [73, 44],
    ],
    [[3, 'reject']],
  );
  road(
    'dgdFail',
    [
      [66, 40],
      [66, 44],
      [73, 44],
    ],
    [[2, 'reject']],
  );

  var districts = [
    {
      id: 'native',
      name: 'Pattern Studio',
      x: 5,
      y: 7,
      bx: 5,
      by: 2,
      color: C.native,
      kind: 'entry',
      badge: '%o',
    },
    {
      id: 'parser',
      name: 'Verb Office',
      x: 5,
      y: 22,
      bx: 5,
      by: 17,
      color: C.parser,
      kind: 'entry',
      badge: 'OBJ',
    },
    {
      id: 'dgd',
      name: 'Grammar Studio',
      x: 5,
      y: 37,
      bx: 5,
      by: 32,
      color: C.dgd,
      kind: 'entry',
      badge: 'S:',
    },
    {
      id: 'groups',
      name: 'Group Junction',
      x: 18,
      y: 14,
      bx: 18,
      by: 9,
      color: C.shared,
      kind: 'groups',
    },
    {
      id: 'grammar',
      name: 'Grammar Foundry',
      x: 27,
      y: 21,
      bx: 27,
      by: 15,
      color: C.shared,
      kind: 'grammar',
    },
    {
      id: 'tokens',
      name: 'Token Works',
      x: 37,
      y: 21,
      bx: 37,
      by: 15,
      color: C.shared,
      kind: 'tokens',
    },
    {
      id: 'chart',
      name: 'Earley Yard',
      x: 47,
      y: 21,
      bx: 46,
      by: 25,
      color: C.shared,
      kind: 'chart',
    },
    {
      id: 'trees',
      name: 'Derivation Grove',
      x: 58,
      y: 21,
      bx: 55,
      by: 15,
      color: C.shared,
      kind: 'trees',
    },
    {
      id: 'nativeOut',
      name: 'Argument House',
      x: 66,
      y: 8,
      bx: 66,
      by: 3,
      color: C.native,
      kind: 'objects',
    },
    {
      id: 'parserOut',
      name: 'Permission Court',
      x: 66,
      y: 30,
      bx: 69,
      by: 20,
      color: C.parser,
      kind: 'gates',
    },
    {
      id: 'dgdOut',
      name: 'Action Workshop',
      x: 66,
      y: 40,
      bx: 63,
      by: 35,
      color: C.dgd,
      kind: 'actions',
    },
    {
      id: 'receipt',
      name: 'Result Square',
      x: 73,
      y: 24,
      bx: 77,
      by: 19,
      color: C.success,
      kind: 'receipt',
    },
    {
      id: 'reject',
      name: 'Return Office',
      x: 73,
      y: 44,
      bx: 76,
      by: 46,
      color: C.fail,
      kind: 'reject',
    },
  ];
  var copy = {
    native: [
      'Native → groups',
      'A compact pattern says which words and captures a command expects.',
      'The native family offers add_rule for player commands and parse_command for an explicit match. Here, give has 2 captures: %o names an object and %L names a living. Quoted words are literals. This entrance converts that pattern to groups; it does not resolve the sword yet. The cart follows one example rule at a time. The related add_action compatibility adapter also builds a grammar, but its verb-and-remainder API is outside this three-dialect tour.',
    ],
    parser: [
      'Rule → groups',
      'Named command tokens become the same groups used by native patterns.',
      'parse_add_rule("give", "OBJ to LIV") registers a verb separately from its argument pattern. OBJ becomes the native builder’s object capture, LIV its single-living capture, and to a literal group. The package also keeps metadata for names such as do_give_obj_to_liv. Those names stay outside the grammar engine. This example has 2 object slots. When a line arrives, rule selection consumes give first, so the engine receives red sword to bob. Switch entrances to see that smaller token stream.',
    ],
    dgd: [
      'Grammar text → builder',
      'DGD-style productions describe a language directly, without pattern groups.',
      'parse_string accepts token rules, a start production, and optional semantic actions. This example gives the start production an accept action and builds phrases using left recursion. The first production chooses the start symbol. Its 2 phrase occurrences are syntax, with no built-in object meaning. Unlike the two pattern entrances, this road goes straight to the grammar builder. The regexes and quoted constants define its vocabulary; the action stays in a separate table indexed by production.',
    ],
    groups: [
      'Shared pattern builder',
      'Two surface languages meet before a Grammar even exists.',
      'A group is a required word, a choice, an optional word, or a typed capture. Both native patterns and parser-package rules use this intermediate form. Their shared builder expands a capture into ordinary productions and records its slot label. For example, %o and OBJ each become one-or-more word-like tokens; neither promises that those words name an object. There is 1 important text difference: native %s permits an empty span, while package STR requires at least one word. Try the Chat lesson with just say.',
    ],
    grammar: [
      'One Grammar',
      'Every entrance leaves the same kind of object at the shared engine’s door.',
      'The builder produces token rules, a start symbol, and context-free productions. Each right-hand-side element is 1 of 3 things: a literal, a token class, or a nonterminal. Pattern captures also carry labels that the engine treats as opaque slot numbers. DGD actions and parser handler names remain frontend metadata. The small blue slabs beside this foundry count the actual productions in this run. One engine can now operate on any entrance’s grammar without knowing about swords, players, or LPC calls.',
    ],
    tokens: [
      'Text → token spans',
      'Tokenization preserves the pieces of source text that later results need.',
      'At each input position, the longest non-empty token match wins; the earliest token rule breaks a tie. Whitespace rules consume characters without adding a token. The boxes on the cart count retained tokens, and the inspector shows their original spans. The native word rules classify 42 as a number and 42x as a word. DGD constants precede regex rules on ties. Our small DGD grammar accepts lowercase letters only; try punctuation to see a lexical failure take the return road before the chart.',
    ],
    chart: [
      'Predict · scan · complete',
      'The same Earley loop advances every grammar, one token boundary at a time.',
      'A chart item remembers a production, a dot, and its origin. Predict adds rules for a required nonterminal. Scan advances over a matching token. Complete advances earlier items waiting for a finished nonterminal; nullable prediction can advance without consuming input. These operations interleave until the column closes. The cart circles once for each later column, including the final boundary. With n tokens there are n + 1 columns. Blue rack heights and the live dot rules are computed from this run, not a scripted animation.',
    ],
    trees: [
      'Chart → derivations',
      'A complete start production proves syntax; a derivation explains how it matched.',
      'Recognition succeeds only when a start production covers the whole input, beginning at boundary 0. Completed chart spans support the derivation trees. The driver enumerates trees lazily, using production order and longest child spans first; the browser materializes a bounded set for inspection. Labels recover exact capture text with internal spacing intact. The grammar has still called no object-identification hook. A phrase like blue sword may parse perfectly and fail later, when its words are compared with the objects in scope.',
    ],
    nativeOut: [
      'Captures → arguments',
      'The native adapter turns labeled spans into the handler’s arguments.',
      'The noun resolver compares capture text with object ids and adjectives in scope. In our two-object room, red sword names the sword and bob names a living. Native single-object captures choose the first match in scope order. Only after every capture succeeds does add_rule call its handler; a return of 0 lets dispatch try another rule. Uncheck Sword in scope: the chart still recognizes the sentence, but this road ends at the Return Office. parse_command instead writes destinations only after a successful match.',
    ],
    parserOut: [
      'can → choose → do',
      'The parser package adds an object-selection and permission protocol around the parse.',
      'For 2 object slots, can_ begins with both unchosen. A direct_ call tests the sword with its own slot filled, then indirect_ tests bob with both slots filled. Once selection finishes, both checks run again with the final arguments before do_. Raw noun phrases follow the object arguments. The inspector shows those calls in order. Uncheck Handler accepts to make can_ refuse. A successful do_ counts as handled regardless of its return value; this tour supplies accepting direct_ and indirect_ implementations.',
    ],
    dgdOut: [
      'Tree → action values',
      'DGD actions consume tree values; object lookup is a separate concern.',
      'Tokens contribute strings. A production without an action flattens its children’s values; an action receives that array and can replace it with another array. Actions run bottom-up, left to right. The example accept action returns its input array when enabled, or 0 when disabled. That 0 rejects the derivation, so another can be tried. Removing the sword from scope changes nothing here: this grammar never asks for objects. Its result contains the word sword, while the native route would pass a resolved object.',
    ],
    receipt: [
      'One engine, three contracts',
      'A shared parse engine leaves each frontend in charge of its public result.',
      'All 3 entrances share tokenization, context-free recognition, and derivation machinery. Their public contracts remain different: a native rule delivers capture arguments, the parser package runs its permission protocol, and parse_string returns semantic values. Choose another frontend with the same sentence to compare the grammar and token spans. Use Fit to see where the roads meet, then Follow to ride with the cart again. These numbers describe this JavaScript teaching model; road lengths and tour speed say nothing about Rust runtime performance.',
    ],
    reject: [
      'Failure has a boundary',
      'An unsuccessful command does not always mean its grammar was wrong.',
      'A failure may occur before a chart exists, while recognizing syntax, when resolving a noun, or when a frontend action refuses a derivation. The live outcome tells you which happened on this run. For a single parser-package rule and no master error message, syntax failure returns -1, handler refusal -2, and unresolved objects -3; an unknown verb returns 0. Native dispatch can continue to another rule, and parse_string returns 0 when no derivation succeeds. This town follows only one registered rule.',
    ],
  };
  var byId = {};
  districts.forEach(function (d) {
    var c = copy[d.id];
    d.r = 3.5;
    d.tag = c[0];
    d.short = c[1];
    d.body = c[2];
    byId[d.id] = d;
  });
  function readSeconds(id) {
    var d = byId[id];
    return d
      ? Math.min(
          26,
          Math.max(9, (d.short + ' ' + d.body).split(/\s+/).length / 3.8 + 3.5),
        )
      : 9;
  }
  function routeDistance(x, y) {
    var best = Infinity;
    Object.keys(routes).forEach(function (key) {
      routes[key].segs.forEach(function (s) {
        var dx = s.b.x - s.a.x,
          dy = s.b.y - s.a.y,
          t = Math.max(
            0,
            Math.min(
              1,
              ((x - s.a.x) * dx + (y - s.a.y) * dy) / (s.len * s.len),
            ),
          );
        best = Math.min(
          best,
          Math.hypot(x - s.a.x - t * dx, y - s.a.y - t * dy),
        );
      });
    });
    return best;
  }
  var props = [];
  function build() {
    props = [];
    for (var x = 3; x < 81; x += 4)
      for (var y = 3; y < 49; y += 4) {
        if (
          Iso.hash2(x, y, 4) > 0.45 ||
          routeDistance(x, y) < 2.8 ||
          districts.some(function (d) {
            return Math.hypot(x - d.bx, y - d.by) < 5;
          })
        )
          continue;
        props.push({
          x: x,
          y: y,
          kind: Iso.hash2(x, y, 8) > 0.3 ? 'tree' : 'house',
          height: 1 + Iso.hash2(x, y, 12),
        });
      }
    global.World.props = props;
  }
  global.World = {
    GW: 83,
    GH: 51,
    C: C,
    routes: routes,
    stations: stations,
    districts: districts,
    byId: byId,
    stationToDistrict: {},
    readSeconds: readSeconds,
    build: build,
    props: props,
  };
})(window);
