import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';
import { mkdir } from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
const require = createRequire(pathToFileURL(process.cwd() + '/'));
const { chromium } = require('playwright');
const args = process.argv.slice(2);
function flag(name, fallback) {
  const i = args.indexOf('--' + name);
  return i < 0 ? fallback : args[i + 1];
}
const url =
  args[0] && !args[0].startsWith('--')
    ? args[0]
    : new URL('../index.html', import.meta.url).href;
const out = path.resolve(flag('out', 'smoke-output'));
const dpr = Number(flag('dpr', '1'));
await mkdir(out, { recursive: true });
const browser = await chromium.launch(
  process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE
    ? { executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE }
    : {}
);
const errors = [];
async function open(viewport = { width: 1440, height: 1000 }) {
  const page = await browser.newPage({ viewport, deviceScaleFactor: dpr });
  page.on('pageerror', (e) => errors.push(e.message));
  page.on('console', (m) => {
    if (m.type() === 'error' || m.type() === 'warning') errors.push(m.text());
  });
  page.on('requestfailed', (r) => errors.push(r.url()));
  await page.goto(url, { waitUntil: 'load' });
  await page.waitForFunction(() => window.Sim && Sim.state.model.tx);
  return page;
}
async function next(page) {
  await page.locator('#speed').fill('8');
  await page.click('#btn-step');
  await page.waitForFunction(
    () => Sim.state.paused || Sim.state.finished,
    {},
    { timeout: 10000 }
  );
  await page.waitForTimeout(160);
  return page.evaluate(() => ({
    station: Sim.state.station,
    finished: Sim.state.finished
  }));
}
async function finish(page, capture = false) {
  const seen = [await page.evaluate(() => Sim.state.station)];
  for (let i = 0; i < 65; i++) {
    const s = await next(page);
    seen.push(s.station);
    if (capture && s.station === 'commit')
      await page.screenshot({
        path: path.join(
          out,
          'commit-' +
            (await page.evaluate(() => Sim.state.model.attempts)) +
            '.png'
        )
      });
    if (s.finished) return seen;
  }
  throw new Error('Tour did not finish');
}
try {
  const page = await open();
  await page.screenshot({ path: path.join(out, 'start.png') });
  const seen = await finish(page, true);
  const expected = await page.evaluate(() =>
    Object.values(World.stations)
      .flat()
      .map((s) => s.id)
  );
  assert.deepEqual(
    expected.filter((id) => !seen.includes(id)),
    []
  );
  assert.deepEqual(
    await page.evaluate(() => ({
      gold: Sim.state.model.world.values.gold,
      conflicts: Sim.state.model.conflicts,
      output: Sim.state.model.world.output.length
    })),
    { gold: 17, conflicts: 1, output: 1 }
  );
  await page.click('#zoom-fit');
  await page.waitForTimeout(200);
  await page.screenshot({ path: path.join(out, 'town.png') });
  await page.selectOption('#operation', 'blind');
  await finish(page);
  assert.equal(await page.evaluate(() => Sim.state.model.world.values.gold), 4);
  await page.selectOption('#operation', 'merge');
  await finish(page);
  assert.equal(await page.evaluate(() => Sim.state.model.conflicts), 0);
  assert.equal(
    await page.evaluate(() => Sim.state.model.world.values.gold),
    17
  );
  await page.uncheck('#same-cell');
  await page.selectOption('#operation', 'read');
  await finish(page);
  assert.deepEqual(
    await page.evaluate(() => ({
      gold: Sim.state.model.world.values.gold,
      bells: Sim.state.model.world.values.bells,
      conflicts: Sim.state.model.conflicts
    })),
    { gold: 14, bells: 3, conflicts: 0 }
  );
  await page.click('#btn-reset');
  await page.click('#btn-about');
  assert.equal(await page.evaluate(() => Sim.state.paused), true);
  await page.keyboard.press('Escape');
  assert.equal(await page.locator('#about').isHidden(), true);
  await page.click('#btn-step');
  await page.waitForFunction(() => Sim.state.paused);
  const before = await page.evaluate(() => Sim.state.station);
  await page.keyboard.press('s');
  await page.waitForFunction(
    (old) => Sim.state.paused && Sim.state.station !== old,
    before
  );
  await page.click('[data-district="retry"]');
  assert.equal(await page.locator('#stage-name').textContent(), 'Return Depot');
  assert.equal(await page.evaluate(() => Sim.state.paused), true);
  await page.click('#btn-live');
  await page.click('#btn-panel');
  assert.equal(await page.locator('#inspector').isVisible(), false);
  await page.click('#btn-panel');
  await page.close();
  for (const [name, viewport] of [
    ['phone', { width: 360, height: 780 }],
    ['landscape', { width: 844, height: 390 }]
  ]) {
    const p = await open(viewport);
    await p.screenshot({ path: path.join(out, name + '.png') });
    await p.click('#sheet-handle');
    await p.waitForTimeout(200);
    assert.equal(await p.locator('#stage-body').isVisible(), true);
    assert.equal(await p.locator('.zoomer').isVisible(), false);
    assert.equal(await p.evaluate(() => Sim.state.paused), true);
    await p.screenshot({ path: path.join(out, name + '-details.png') });
    await p.click('#sheet-handle');
    await p.click('#btn-tune');
    await p.locator('#amount').fill('8');
    assert.equal(await p.evaluate(() => Sim.state.input.amount), 8);
    await p.screenshot({ path: path.join(out, name + '-controls.png') });
    assert.equal(
      await p.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth
      ),
      true
    );
    await p.close();
  }
  assert.deepEqual(errors, []);
  console.log(
    'PASS: all 10 stations; all operation branches; controls; modal; phone and landscape; DPR ' +
      dpr +
      '. Screenshots: ' +
      out
  );
} finally {
  await browser.close();
}
