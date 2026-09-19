/* A bounded teaching implementation of the command grammar engine. */
(function (global) {
  'use strict';
  var MAX_WORK = 20000,
    MAX_TREES = 32,
    MAX_DEPTH = 64;
  var FAMILIES = ['native', 'parser', 'dgd'];
  var NAMES = {
    native: 'Native patterns',
    parser: 'Parser package',
    dgd: 'DGD grammars',
  };
  function symbol(type, value, label) {
    return { type: type, value: value, label: label };
  }
  function nt(name, label) {
    return symbol('nt', name, label);
  }
  function lit(text) {
    return symbol('lit', text);
  }
  function tok(name) {
    return symbol('tok', name);
  }
  function grammar() {
    return { start: 'S', rules: [], tokens: [], kinds: [], nullable: {} };
  }
  function rule(g, lhs, rhs, action) {
    g.rules.push({ lhs: lhs, rhs: rhs, action: action });
  }
  function token(g, name, pattern, skip) {
    g.tokens.push({ name: name, pattern: pattern, skip: !!skip });
  }
  function prepare(g) {
    g.byLhs = {};
    g.rules.forEach(function (p, i) {
      (g.byLhs[p.lhs] || (g.byLhs[p.lhs] = [])).push(i);
    });
    g.rules.forEach(function (p) {
      p.rhs.forEach(function (s) {
        if (s.type === 'nt' && !g.byLhs[s.value])
          throw new Error('Undefined production: ' + s.value);
        if (
          s.type === 'tok' &&
          !g.tokens.some(function (t) {
            return t.name === s.value;
          })
        )
          throw new Error('Undefined token: ' + s.value);
      });
    });
    var changed = true;
    while (changed) {
      changed = false;
      g.rules.forEach(function (p) {
        if (
          !g.nullable[p.lhs] &&
          p.rhs.every(function (s) {
            return s.type === 'nt' && g.nullable[s.value];
          })
        ) {
          g.nullable[p.lhs] = true;
          changed = true;
        }
      });
    }
    g.tokens.forEach(function (t) {
      t.re = new RegExp('^(?:' + t.pattern + ')$');
    });
    return g;
  }
  function nativeGroups(text) {
    var out = [],
      re = /\s*('([^']+)'|\[([^\]]+)\]|%([wsdoL])|\/)/g,
      pos = 0,
      m,
      alternative = false;
    while (pos < text.length) {
      if (!text.slice(pos).trim()) break;
      re.lastIndex = pos;
      m = re.exec(text);
      if (!m || m.index !== pos)
        throw new Error(
          'This lesson supports quoted words, [word], /, %w, %s, %d, %o and %L.'
        );
      pos = re.lastIndex;
      if (m[1] === '/') {
        if (!out.length || alternative || !out[out.length - 1].words)
          throw new Error('Bad alternative');
        alternative = true;
        continue;
      }
      if (m[4]) {
        if (alternative) throw new Error('Alternatives must be words');
        out.push({
          kind: { w: 'word', s: 'text', d: 'number', o: 'object', L: 'living' }[
            m[4]
          ],
          empty: m[4] === 's',
        });
      } else {
        var word = m[2] || m[3];
        if (/\s/.test(word)) throw new Error('Expected one word');
        if (alternative) {
          out[out.length - 1].words.push(word);
          out[out.length - 1].optional = out[out.length - 1].optional || !!m[3];
          alternative = false;
        } else out.push({ words: [word], optional: !!m[3] });
      }
    }
    if (alternative) throw new Error('Missing alternative');
    return out;
  }
  function parserGroups(text) {
    var kinds = { OBJ: 'object', LIV: 'living', WRD: 'word', STR: 'text' };
    var groups = text.trim()
      ? text
          .trim()
          .split(/\s+/)
          .map(function (w) {
            if (/^(OBS|LVS)/.test(w))
              throw new Error('Many-object captures are outside this lesson.');
            if (/^(OBJ|LIV|WRD|STR)/.test(w) && !kinds[w])
              throw new Error('Token inside a word');
            return kinds[w] ? { kind: kinds[w], empty: false } : { words: [w] };
          })
      : [];
    if (
      groups.filter(function (g) {
        return g.kind === 'text';
      }).length > 1
    )
      throw new Error('Only one STR is allowed');
    if (
      groups.filter(function (g) {
        return g.kind === 'object' || g.kind === 'living';
      }).length > 2
    )
      throw new Error('Only two object slots are allowed');
    return groups;
  }
  function compileGroups(groups) {
    var g = grammar(),
      rhs = [],
      built = {};
    token(g, 'whitespace', '\\s+', true);
    token(g, 'number', '[0-9]+');
    token(g, 'word', '\\S+');
    function words(type) {
      if (!built.word) {
        rule(g, 'word_like', [tok('word')]);
        rule(g, 'word_like', [tok('number')]);
        built.word = true;
      }
      if (type === 'word') return nt('word_like');
      var name = type === 'star' ? 'words_star' : 'words_plus';
      if (!built[name]) {
        rule(g, name, type === 'star' ? [] : [nt('word_like')]);
        rule(g, name, [nt(name), nt('word_like')]);
        built[name] = true;
      }
      return nt(name);
    }
    groups.forEach(function (group, i) {
      var s;
      if (group.words) {
        if (group.words.length === 1 && !group.optional)
          s = lit(group.words[0]);
        else {
          var name = 'choice_' + i;
          group.words
            .filter(function (w, j, a) {
              return a.indexOf(w) === j;
            })
            .forEach(function (w) {
              rule(g, name, [lit(w)]);
            });
          if (group.optional) rule(g, name, []);
          s = nt(name);
        }
      } else {
        s =
          group.kind === 'number'
            ? tok('number')
            : words(
                group.kind === 'word' ? 'word' : group.empty ? 'star' : 'plus'
              );
        s.label = g.kinds.length;
        g.kinds.push(group.kind);
      }
      rhs.push(s);
    });
    rule(g, 'S', rhs);
    return prepare(g);
  }
  /* Only the line-oriented DGD subset printed by the two lessons is accepted. */
  function compileDgd(text) {
    var g = grammar(),
      productions = [],
      definitions = [],
      constants = [];
    text
      .split('\n')
      .filter(function (l) {
        return l.trim();
      })
      .forEach(function (line) {
        var m = /^\s*(\w+)\s*=\s*\/(.*)\/\s*$/.exec(line);
        if (m) {
          if (['[ \\t]+', '[a-z]+', '[0-9]+'].indexOf(m[2]) < 0)
            throw new Error('Regexp is outside the lesson subset');
          definitions.push({ name: m[1], pattern: m[2] });
          return;
        }
        m = /^\s*(\w+)\s*:\s*(.*)$/.exec(line);
        if (!m) throw new Error('Expected a DGD token rule or production');
        var body = m[2],
          action = /\s*\?\s*(\w+)\s*$/.exec(body);
        if (action) {
          body = body.slice(0, action.index);
          if (action[1] !== 'accept')
            throw new Error('Only the lesson action accept is available');
        }
        var pieces = body.match(/'[^']*'|\w+/g) || [];
        if (pieces.join('') !== body.replace(/\s+/g, ''))
          throw new Error('Unsupported production syntax');
        pieces.forEach(function (piece) {
          if (piece[0] === "'" && constants.indexOf(piece) < 0)
            constants.push(piece);
        });
        productions.push({
          lhs: m[1],
          pieces: pieces,
          action: action && action[1],
        });
      });
    if (!definitions.length || !productions.length)
      throw new Error('Tokens and a start production are required');
    constants.forEach(function (c) {
      token(g, c, c.slice(1, -1).replace(/[.*+?^${}()|[\]\\]/g, '\\$&'));
    });
    definitions.forEach(function (d) {
      token(g, d.name, d.pattern, d.name === 'whitespace');
    });
    var names = productions.map(function (p) {
      return p.lhs;
    });
    g.start = names[0];
    productions.forEach(function (p) {
      rule(
        g,
        p.lhs,
        p.pieces.map(function (s) {
          return names.indexOf(s) >= 0 ? nt(s) : tok(s);
        }),
        p.action
      );
    });
    return prepare(g);
  }
  function specification(family, lesson) {
    var give = lesson !== 'chat',
      verb = give ? 'give' : 'say';
    var nativeText = give ? "'give' %o 'to' %L" : "'say' %s";
    var parserText = give ? 'OBJ to LIV' : 'STR';
    var dgdText =
      'whitespace = /[ \\t]+/\nword = /[a-z]+/\n' +
      (give
        ? "Command: 'give' Phrase 'to' Phrase ? accept\n"
        : "Command: 'say' Phrase ? accept\n") +
      'Phrase: word\nPhrase: Phrase word';
    return {
      family: family,
      verb: verb,
      text:
        family === 'native'
          ? nativeText
          : family === 'parser'
            ? parserText
            : dgdText,
      display:
        family === 'native'
          ? 'add_rule("' + nativeText + '", "do_' + verb + '");'
          : family === 'parser'
            ? 'parse_add_rule("' + verb + '", "' + parserText + '");'
            : dgdText,
    };
  }
  /* Checking every prefix implements maximal munch even for overlapping alternatives. */
  function tokenize(g, input) {
    var tokens = [],
      skipped = [],
      pos = 0;
    while (pos < input.length) {
      var best = null;
      g.tokens.forEach(function (t, rank) {
        for (var end = pos + 1; end <= input.length; end++) {
          if (t.re.test(input.slice(pos, end)) && (!best || end > best.end))
            best = {
              text: input.slice(pos, end),
              start: pos,
              end: end,
              className: t.name,
              rank: rank,
              skip: t.skip,
            };
        }
      });
      if (!best)
        return {
          tokens: tokens,
          skipped: skipped,
          error: 'No token rule matches at character ' + pos + '.',
          offset: pos,
        };
      (best.skip ? skipped : tokens).push(best);
      pos = best.end;
    }
    return { tokens: tokens, skipped: skipped, error: null };
  }
  function matches(s, t) {
    return (
      !!t &&
      (s.type === 'lit'
        ? s.value === t.text
        : s.type === 'tok' && s.value === t.className)
    );
  }
  function chart(g, tokens) {
    var c = {
      g: g,
      tokens: tokens,
      sets: [],
      seen: [],
      column: -1,
      work: 0,
      counts: { seed: 0, predict: 0, scan: 0, complete: 0, nullable: 0 },
      error: null,
      events: [],
    };
    for (var i = 0; i <= tokens.length; i++) {
      c.sets.push([]);
      c.seen.push({});
    }
    (g.byLhs[g.start] || []).forEach(function (p) {
      add(c, 0, { prod: p, dot: 0, origin: 0 }, 'seed');
    });
    return c;
  }
  function add(c, at, item, reason) {
    if (c.error) return;
    if (++c.work > MAX_WORK) {
      c.error = 'Teaching work limit reached';
      return;
    }
    var key = item.prod + '/' + item.dot + '/' + item.origin;
    if (c.seen[at][key]) return;
    c.seen[at][key] = true;
    c.sets[at].push(item);
    c.counts[reason]++;
    c.events.push({ at: at, item: item, reason: reason });
  }
  function advanceColumn(c) {
    if (c.error || c.column >= c.tokens.length) return false;
    var at = ++c.column,
      g = c.g;
    for (var i = 0; i < c.sets[at].length && !c.error; i++) {
      var item = c.sets[at][i],
        p = g.rules[item.prod],
        next = p.rhs[item.dot];
      if (!next) {
        c.sets[item.origin].slice().forEach(function (waiting) {
          var s = g.rules[waiting.prod].rhs[waiting.dot];
          if (s && s.type === 'nt' && s.value === p.lhs)
            add(
              c,
              at,
              {
                prod: waiting.prod,
                dot: waiting.dot + 1,
                origin: waiting.origin,
              },
              'complete'
            );
        });
      } else if (next.type === 'nt') {
        g.byLhs[next.value].forEach(function (pid) {
          add(c, at, { prod: pid, dot: 0, origin: at }, 'predict');
        });
        if (g.nullable[next.value])
          add(
            c,
            at,
            { prod: item.prod, dot: item.dot + 1, origin: item.origin },
            'nullable'
          );
      } else if (matches(next, c.tokens[at]))
        add(
          c,
          at + 1,
          { prod: item.prod, dot: item.dot + 1, origin: item.origin },
          'scan'
        );
    }
    return true;
  }
  function accepted(c) {
    return (
      !c.error &&
      c.column === c.tokens.length &&
      c.sets[c.tokens.length].some(function (item) {
        var p = c.g.rules[item.prod];
        return (
          p.lhs === c.g.start && item.origin === 0 && item.dot === p.rhs.length
        );
      })
    );
  }
  /* Derive from completed spans in production order, trying the longest child span first. */
  function trees(c) {
    if (!accepted(c)) return [];
    var work = 0,
      memo = {};
    function derive(name, start, end, active, depth) {
      if (++work > MAX_WORK || depth > MAX_DEPTH)
        throw new Error('Teaching derivation limit reached');
      var key = name + '/' + start + '/' + end;
      if (active.indexOf(key) >= 0) return [];
      if (memo[key]) return memo[key];
      var stack = active.concat(key),
        out = [];
      (c.g.byLhs[name] || []).forEach(function (pid) {
        if (!c.seen[end][pid + '/' + c.g.rules[pid].rhs.length + '/' + start])
          return;
        var p = c.g.rules[pid];
        function lay(index, pos, children) {
          if (++work > MAX_WORK)
            throw new Error('Teaching derivation limit reached');
          if (out.length >= MAX_TREES) return;
          if (index === p.rhs.length) {
            if (pos === end)
              out.push({
                prod: pid,
                start: start,
                end: end,
                children: children,
              });
            return;
          }
          var s = p.rhs[index];
          if (s.type !== 'nt') {
            if (pos < end && matches(s, c.tokens[pos]))
              lay(
                index + 1,
                pos + 1,
                children.concat({ token: pos, start: pos, end: pos + 1 })
              );
          } else {
            var lo = index + 1 === p.rhs.length ? end : pos;
            for (var mid = end; mid >= lo && out.length < MAX_TREES; mid--) {
              derive(s.value, pos, mid, stack, depth + 1).forEach(
                function (child) {
                  lay(index + 1, mid, children.concat(child));
                }
              );
            }
          }
        }
        lay(0, start, []);
      });
      memo[key] = out.slice(0, MAX_TREES);
      return memo[key];
    }
    return derive(c.g.start, 0, c.tokens.length, [], 0);
  }
  function spanText(m, start, end) {
    return start === end
      ? ''
      : m.engineInput.slice(
          m.scan.tokens[start].start,
          m.scan.tokens[end - 1].end
        );
  }
  function captures(m, tree) {
    var out = [];
    function walk(node) {
      if (node.token != null) return;
      var p = m.g.rules[node.prod];
      node.children.forEach(function (child, i) {
        if (p.rhs[i].label != null)
          out.push({
            slot: p.rhs[i].label,
            kind: m.g.kinds[p.rhs[i].label],
            text: spanText(m, child.start, child.end),
            start: child.start,
            end: child.end,
          });
        walk(child);
      });
    }
    walk(tree);
    return out.sort(function (a, b) {
      return a.slot - b.slot;
    });
  }
  function create(input) {
    var family = FAMILIES.indexOf(input.family) >= 0 ? input.family : 'native';
    var lesson = input.lesson === 'chat' ? 'chat' : 'give';
    var line = String(
      input.line == null
        ? lesson === 'give'
          ? 'give red sword to bob'
          : 'say hello there'
        : input.line
    ).slice(0, 120);
    return {
      input: {
        family: family,
        lesson: lesson,
        line: line,
        sword: input.sword !== false,
        allow: input.allow !== false,
      },
      spec: specification(family, lesson),
      groups: null,
      g: null,
      scan: null,
      chart: null,
      derivations: [],
      captures: [],
      calls: [],
      output: null,
      error: null,
      recognized: false,
      status: 'Waiting at the frontend',
      engineInput: line,
    };
  }
  function lower(m) {
    if (m.input.family !== 'dgd')
      m.groups =
        m.input.family === 'native'
          ? nativeGroups(m.spec.text)
          : parserGroups(m.spec.text);
  }
  function compile(m) {
    m.g =
      m.input.family === 'dgd'
        ? compileDgd(m.spec.text)
        : compileGroups(m.groups);
    m.status = 'One Grammar representation';
  }
  function scan(m) {
    if (m.input.family !== 'dgd') {
      var word = /^\s*(\S+)\s*/.exec(m.input.line);
      if (!word || word[1] !== m.spec.verb) {
        m.error =
          m.input.family === 'parser'
            ? 'No registered rule for that first word (parse_sentence returns 0).'
            : 'The selected native rule does not match the first word.';
        m.stopKind = 'verb';
        return;
      }
      if (m.input.family === 'parser')
        m.engineInput = m.input.line.slice(word[0].length);
    }
    m.scan = tokenize(m.g, m.engineInput);
    if (m.scan.error) {
      m.error = m.scan.error;
      m.stopKind = 'lexical';
    } else if (m.scan.tokens.length > 24) {
      m.error = 'This lesson is limited to 24 tokens.';
      m.stopKind = 'limit';
    } else m.chart = chart(m.g, m.scan.tokens);
    m.status = m.error || 'Tokens retain their source spans';
  }
  function recognize(m) {
    if (!m.chart) return;
    advanceColumn(m.chart);
    m.recognized = accepted(m.chart);
    if (m.chart.error) {
      m.error = m.chart.error;
      m.stopKind = 'limit';
    }
    m.status =
      m.error ||
      'Closed chart column ' + m.chart.column + ' of ' + m.chart.tokens.length;
  }
  function derive(m) {
    m.recognized = accepted(m.chart);
    if (!m.recognized) {
      m.error = 'No complete start production spans the input.';
      m.status = m.error;
      return;
    }
    try {
      m.derivations = trees(m.chart);
    } catch (err) {
      m.error = err.message;
      m.stopKind = 'limit';
    }
    if (!m.error && !m.derivations.length) {
      m.error = 'No acyclic derivation in the teaching subset.';
      m.stopKind = 'limit';
    }
    if (!m.error) m.captures = captures(m, m.derivations[0]);
    m.status = m.error || 'Syntax recognized; meaning is still undecided';
  }
  function objects(m, phrase, living) {
    var scope = [
      {
        name: '/room/sword',
        ids: ['sword'],
        adjectives: ['red'],
        living: false,
      },
      { name: '/players/bob', ids: ['bob'], adjectives: [], living: true },
    ];
    return scope.filter(function (o) {
      if ((o.name === '/room/sword' && !m.input.sword) || (living && !o.living))
        return false;
      var words = phrase.trim().split(/\s+/),
        noun = words.pop();
      return (
        o.ids.indexOf(noun) >= 0 &&
        words.every(function (a) {
          return o.adjectives.indexOf(a) >= 0;
        })
      );
    });
  }
  function interpret(m) {
    if (m.error) return;
    var family = m.input.family,
      verb = m.spec.verb,
      success = false;
    m.derivations.some(function (tree) {
      var args = [],
        raw = [],
        kinds = [],
        unresolved = false;
      m.captures = captures(m, tree);
      if (family === 'dgd') {
        function fold(node) {
          if (node.token != null) return [m.scan.tokens[node.token].text];
          var values = [];
          node.children.forEach(function (child) {
            values = values.concat(fold(child));
          });
          if (m.g.rules[node.prod].action)
            m.calls.push(
              'accept(' +
                JSON.stringify(values) +
                ') → ' +
                (m.input.allow ? 'array' : '0')
            );
          return values;
        }
        args = fold(tree);
        if (m.input.allow) {
          m.output = args;
          success = true;
        }
        return success;
      }
      m.captures.forEach(function (c) {
        if (c.kind === 'object' || c.kind === 'living') {
          args.push(0);
          raw.push(c.text);
          kinds.push(c.slot);
        } else args.push(c.kind === 'number' ? Number(c.text) : c.text);
      });
      var slug = verb === 'give' ? 'obj_to_liv' : 'str';
      if (family === 'parser') {
        m.calls.push(
          'can_' +
            verb +
            '_' +
            slug +
            '(' +
            args.concat(raw).map(format).join(', ') +
            ') → ' +
            (m.input.allow ? '1' : '0')
        );
        if (!m.input.allow) return false;
      }
      m.captures.forEach(function (c) {
        if (c.kind !== 'object' && c.kind !== 'living') return;
        if (unresolved) return;
        var found = objects(m, c.text, c.kind === 'living');
        if (!found.length) {
          unresolved = true;
          return;
        }
        args[c.slot] = { object: found[0].name };
        if (family === 'parser')
          m.calls.push(
            (kinds.indexOf(c.slot) === 0 ? 'direct_' : 'indirect_') +
              verb +
              '_' +
              slug +
              '(' +
              args.concat(raw).map(format).join(', ') +
              ') → 1'
          );
      });
      if (unresolved) {
        m.failureKind = 'noun';
        return false;
      }
      if (family === 'parser') {
        kinds.forEach(function (slot, i) {
          m.calls.push(
            (i === 0 ? 'direct_' : 'indirect_') +
              verb +
              '_' +
              slug +
              '(' +
              args.concat(raw).map(format).join(', ') +
              ') → 1 (all slots filled)'
          );
        });
        m.calls.push(
          'do_' +
            verb +
            '_' +
            slug +
            '(' +
            args.concat(raw).map(format).join(', ') +
            ')'
        );
        m.output = 1;
        success = true;
      } else {
        m.calls.push(
          'do_' +
            verb +
            '(' +
            args.map(format).join(', ') +
            ') → ' +
            (m.input.allow ? '1' : '0')
        );
        success = m.input.allow;
        if (success) m.output = args;
      }
      return success;
    });
    if (!success)
      m.error =
        m.failureKind === 'noun'
          ? 'Syntax matched, but a noun named no object in scope.'
          : family === 'dgd'
            ? 'accept returned 0, rejecting every derivation.'
            : family === 'parser'
              ? 'can_ refused the command before object selection.'
              : 'The native handler returned 0; dispatch may try another rule.';
    m.status = success
      ? family === 'dgd'
        ? 'parse_string returns token values'
        : family === 'parser'
          ? 'do_ ran; parse_sentence returns 1'
          : 'The native handler accepted its resolved arguments'
      : m.error;
  }
  function format(value) {
    return value && value.object ? value.object : JSON.stringify(value);
  }
  function run(input) {
    var m = create(input);
    lower(m);
    compile(m);
    scan(m);
    if (m.chart)
      while (m.chart.column < m.chart.tokens.length && !m.error) recognize(m);
    if (!m.error) derive(m);
    if (!m.error) interpret(m);
    return m;
  }
  function itemText(g, item) {
    var p = g.rules[item.prod],
      words = p.rhs.map(function (s) {
        return s.type === 'lit' ? "'" + s.value + "'" : s.value;
      });
    words.splice(item.dot, 0, '•');
    return p.lhs + ' → ' + words.join(' ') + '   [' + item.origin + ']';
  }
  global.ParserModel = {
    families: FAMILIES,
    names: NAMES,
    create: create,
    specification: specification,
    lower: lower,
    compile: compile,
    scan: scan,
    recognize: recognize,
    derive: derive,
    interpret: interpret,
    run: run,
    nativeGroups: nativeGroups,
    parserGroups: parserGroups,
    compileGroups: compileGroups,
    compileDgd: compileDgd,
    tokenize: tokenize,
    chart: chart,
    advanceColumn: advanceColumn,
    accepted: accepted,
    trees: trees,
    prepare: prepare,
    captures: captures,
    itemText: itemText,
    format: format,
  };
})(window);
