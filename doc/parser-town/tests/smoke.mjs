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
const out = path.resolve(flag('out', 'local/parser-town-smoke'));
const dpr = Number(flag('dpr', '1'));
await mkdir(out, { recursive: true });
const browser = await chromium.launch(
  process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE
    ? { executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE }
    : {},
);
const errors = [];
async function open(
  viewport = { width: 1440, height: 1000 },
  reducedMotion = 'no-preference',
) {
  const p = await browser.newPage({
    viewport,
    deviceScaleFactor: dpr,
    reducedMotion,
  });
  p.on('pageerror', (e) => errors.push(e.message));
  p.on('console', (m) => {
    if (['error', 'warning'].includes(m.type())) errors.push(m.text());
  });
  p.on('requestfailed', (r) => errors.push(r.url()));
  p.on('request', (r) => {
    if (/^https?:/.test(r.url()) && !url.startsWith('http'))
      errors.push('Unexpected network request: ' + r.url());
  });
  await p.goto(url, { waitUntil: 'load' });
  await p.waitForFunction(() => window.Sim && Sim.state.model);
  return p;
}
async function next(p) {
  await p.locator('#speed').fill('8');
  await p.click('#btn-step');
  await p.waitForFunction(
    () => Sim.state.paused || Sim.state.finished,
    {},
    { timeout: 10000 },
  );
  await p.waitForTimeout(110);
  return p.evaluate(() => ({
    station: Sim.state.station,
    finished: Sim.state.finished,
  }));
}
async function finish(p, screenshot = false) {
  const seen = [await p.evaluate(() => Sim.state.station)];
  for (let i = 0; i < 65; i++) {
    const s = await next(p);
    seen.push(s.station);
    if (
      screenshot &&
      ['grammar', 'tokens', 'chart', 'trees', 'nativeOut'].includes(s.station)
    ) {
      const col = await p.evaluate(() => Sim.state.model.chart?.column ?? -1);
      await p.screenshot({
        path: path.join(out, s.station + '-' + col + '.png'),
      });
    }
    if (s.finished) return seen;
  }
  throw new Error('Tour did not finish');
}
try {
  const p = await open();
  await p.screenshot({ path: path.join(out, 'start.png') });
  const seen = new Set(await finish(p, true));
  assert.equal(await p.evaluate(() => Sim.state.model.error), null);
  await p.click('#zoom-fit');
  await p.waitForTimeout(200);
  await p.screenshot({ path: path.join(out, 'town.png') });
  for (const family of ['parser', 'dgd']) {
    await p.click(`[data-family="${family}"]`);
    for (const id of await finish(p)) seen.add(id);
    assert.equal(await p.evaluate(() => Sim.state.model.error), null);
    await p.screenshot({ path: path.join(out, family + '-result.png') });
  }
  await p.uncheck('#sword');
  await finish(p);
  assert.equal(await p.evaluate(() => Sim.state.model.error), null);
  await p.click('[data-family="native"]');
  for (const id of await finish(p)) seen.add(id);
  assert.equal(await p.evaluate(() => Sim.state.model.failureKind), 'noun');
  await p.screenshot({ path: path.join(out, 'noun-failure.png') });
  const expected = await p.evaluate(() => World.districts.map((d) => d.id));
  assert.deepEqual(
    expected.filter((id) => !seen.has(id)),
    [],
  );
  await p.check('#sword');
  await p.uncheck('#allow');
  await p.click('[data-family="parser"]');
  await finish(p);
  assert.equal(await p.evaluate(() => Sim.state.model.calls.length), 1);
  assert.match(await p.locator('#result-text').textContent(), /-2/);
  await p.check('#allow');
  await p.selectOption('#lesson', 'chat');
  await p.locator('#sentence').fill('say');
  await p.locator('#sentence').press('Enter');
  await finish(p);
  assert.match(await p.locator('#result-text').textContent(), /-1/);
  await p.click('[data-family="native"]');
  await finish(p);
  assert.deepEqual(await p.evaluate(() => Sim.state.model.output), ['']);
  await p.click('[data-family="dgd"]');
  await p.locator('#sentence').fill('say !');
  await p.locator('#sentence').press('Enter');
  await finish(p);
  assert.equal(await p.evaluate(() => Sim.state.model.chart), null);
  await p.click('#btn-reset');
  await p.click('#btn-about');
  assert.equal(await p.evaluate(() => Sim.state.paused), true);
  await p.keyboard.press('Escape');
  assert.equal(await p.locator('#about').isHidden(), true);
  await p.click('[data-district="chart"]');
  assert.equal(await p.locator('#stage-name').textContent(), 'Earley Yard');
  assert.equal(await p.evaluate(() => Sim.state.paused), true);
  await p.click('#btn-live');
  await p.click('#btn-panel');
  assert.equal(await p.locator('#inspector').isVisible(), false);
  await p.click('#btn-panel');
  // Keyboard stepping is exercised with focus outside a form control.
  await p.locator('#btn-step').click();
  await p.waitForFunction(() => Sim.state.paused);
  const before = await p.evaluate(() => Sim.state.station);
  await p.evaluate(() => document.activeElement.blur());
  await p.keyboard.press('s');
  await p.waitForFunction(
    (old) => Sim.state.paused && Sim.state.station !== old,
    before,
  );
  for (const details of ['grammar-details', 'chart-details', 'tree-details'])
    await p.locator('#' + details).evaluate((e) => (e.open = true));
  await p.screenshot({ path: path.join(out, 'details.png') });
  await p.close();
  for (const [name, viewport] of [
    ['phone', { width: 360, height: 780 }],
    ['landscape', { width: 844, height: 390 }],
  ]) {
    const page = await open(viewport);
    await page.screenshot({ path: path.join(out, name + '.png') });
    await page.click('#sheet-handle');
    assert.equal(await page.locator('#stage-body').isVisible(), true);
    assert.equal(await page.locator('.zoomer').isVisible(), false);
    assert.equal(await page.evaluate(() => Sim.state.paused), true);
    await page.screenshot({ path: path.join(out, name + '-details.png') });
    await page.click('#sheet-handle');
    await page.click('#btn-tune');
    await page.selectOption('#lesson', 'chat');
    assert.equal(await page.evaluate(() => Sim.state.input.lesson), 'chat');
    await page.screenshot({ path: path.join(out, name + '-controls.png') });
    assert.equal(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
      true,
    );
    await page.close();
  }
  const reduced = await open(undefined, 'reduce');
  assert.equal(await reduced.evaluate(() => Sim.state.paused), true);
  await reduced.close();
  assert.deepEqual(errors, []);
  console.log(
    'PASS: all 13 districts; three frontends; failure routes; controls; phone and landscape; ' +
      (url.startsWith('file:') ? 'offline' : 'HTTP') +
      '; DPR ' +
      dpr +
      '. Screenshots: ' +
      out,
  );
} finally {
  await browser.close();
}
