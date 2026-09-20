import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';
import { mkdir } from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';

const require = createRequire(pathToFileURL(process.cwd() + '/'));
const { chromium } = require('playwright');
const args = process.argv.slice(2);
function flag(name, fallback) {
  const index = args.indexOf('--' + name);
  return index < 0 ? fallback : args[index + 1];
}
const url = args[0] && !args[0].startsWith('--') ? args[0] : new URL('../index.html', import.meta.url).href;
const out = path.resolve(flag('out', 'local/collector-town-smoke'));
const dpr = Number(flag('dpr', '1'));
await mkdir(out, { recursive: true });
const browser = await chromium.launch(process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE
  ? { executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE } : {});
const errors = [], visited = new Set();
async function open(viewport = { width: 1440, height: 1000 }, options = {}) {
  const page = await browser.newPage({ viewport, deviceScaleFactor: dpr, ...options });
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => {
    if (['error', 'warning'].includes(message.type())) errors.push(message.text());
  });
  page.on('requestfailed', request => errors.push('Failed request: ' + request.url()));
  page.on('request', request => {
    if (url.startsWith('file:') && /^https?:/.test(request.url())) errors.push('Offline page requested ' + request.url());
  });
  await page.goto(url, { waitUntil: 'load' });
  await page.waitForFunction(() => window.Sim && Sim.state.model);
  assert.equal(await page.locator('canvas').count(), 1);
  return page;
}
async function shot(page, name) { await page.screenshot({ path: path.join(out, name + '.png') }); }
async function next(page) {
  await page.locator('#speed').fill('8');
  await page.locator('#btn-step').click();
  await page.waitForFunction(() => Sim.state.paused || Sim.state.finished, {}, { timeout: 10000 });
  return page.evaluate(() => ({ station: Sim.state.station, finished: Sim.state.finished }));
}
async function finish(page, capture = false) {
  const seen = [];
  const first = await page.evaluate(() => Sim.state.station);
  if (first) { seen.push(first); visited.add(first); }
  let captured = false;
  for (let i = 0; i < 150; i++) {
    const result = await next(page);
    seen.push(result.station); visited.add(result.station);
    if (capture && !captured && result.station === 'trace' && await page.evaluate(() => Sim.state.model.visits >= 10)) {
      await page.waitForTimeout(700); await shot(page, 'tracing'); captured = true;
    }
    if (result.finished) return seen;
  }
  throw new Error('The request never finished');
}
async function report(page) { return page.evaluate(() => Sim.state.model.report); }
try {
  const page = await open();
  await page.waitForFunction(() => Sim.state.station === 'request');
  await page.locator('#btn-play').click();
  await shot(page, 'start');
  assert.equal(await page.evaluate(() => Sim.state.paused), true);
  const oldDwell = await page.evaluate(() => Sim.state.dwellLeft);
  await page.waitForTimeout(150);
  assert.equal(await page.evaluate(() => Sim.state.dwellLeft), oldDwell);
  const accepted = await finish(page, true);
  assert.ok(!accepted.includes('refuse'));
  assert.deepEqual(await report(page), { reclaimed: 4 });
  await shot(page, 'done');
  await page.locator('#zoom-fit').click();
  await shot(page, 'map');
  await page.locator('#linked').uncheck();
  await finish(page);
  assert.deepEqual(await report(page), { reclaimed: 11 });
  for (const callback of ['callout', 'input', 'rule']) {
    await page.locator('#callback').selectOption(callback);
    await finish(page);
    assert.deepEqual(await report(page), { reclaimed: 4 }, callback);
  }
  await page.locator('#cycles').fill('3');
  await finish(page);
  assert.deepEqual(await report(page), { reclaimed: 8 });
  await page.locator('#pins').fill('2');
  const refused = await finish(page);
  assert.ok(!refused.includes('roots') && !refused.includes('trace') && !refused.includes('sweep'));
  assert.deepEqual(await report(page), { refused: 2 });
  assert.equal(await page.evaluate(() => GC.counts(Sim.state.model).remaining), 22);
  await shot(page, 'refused');
  const expected = await page.evaluate(() => Object.values(World.stations).flat().map(st => st.id));
  assert.deepEqual(expected.filter(id => !visited.has(id)), []);

  await page.locator('#btn-about').click();
  assert.equal(await page.locator('#about').isVisible(), true);
  await page.keyboard.press('Shift+Tab');
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Reader and maintainer notes');
  await page.keyboard.press('Escape');
  assert.equal(await page.locator('#about').isVisible(), false);
  assert.equal(await page.evaluate(() => document.activeElement.id), 'btn-about');
  await page.locator('#btn-run').click();
  await page.locator('#btn-play').click();
  await page.evaluate(() => document.activeElement.blur());
  await page.keyboard.press('s');
  await page.waitForFunction(() => Sim.state.paused);
  const reading = await page.evaluate(() => Sim.state.reading);
  assert.equal(reading, false, 'New run preserves reading history');
  await page.keyboard.press('r');
  await page.waitForFunction(() => Sim.state.station === 'request');
  assert.equal(await page.evaluate(() => Sim.state.reading), true);
  await page.keyboard.press(' ');
  assert.equal(await page.evaluate(() => Sim.state.paused), true);
  const follow = await page.locator('#follow').isChecked();
  await page.keyboard.press('f');
  assert.equal(await page.locator('#follow').isChecked(), !follow);
  await page.keyboard.press('l');
  assert.equal(await page.locator('#labels').isChecked(), false);
  await page.keyboard.press('l');
  await page.locator('[data-station="trace"]').click();
  assert.equal(await page.locator('#district-title').textContent(), 'Reference Foundry');
  assert.equal(await page.evaluate(() => Sim.state.paused), true);
  await page.locator('#btn-unpin').click();
  await page.close();

  for (const [name, viewport] of [['phone', { width: 360, height: 800 }], ['landscape', { width: 844, height: 390 }]]) {
    const mobile = await open(viewport, { isMobile: true, hasTouch: true });
    await mobile.waitForTimeout(700);
    assert.equal(await mobile.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true, name + ' overflow');
    await shot(mobile, name);
    if (name === 'phone') {
      await mobile.locator('#sheet-toggle').click();
      assert.equal(await mobile.locator('#district-body').isVisible(), true);
      await shot(mobile, 'phone-notes');
      await mobile.locator('#sheet-toggle').click();
    }
    await mobile.locator('#btn-settings').click();
    await mobile.locator('#pins').fill('1');
    assert.equal(await mobile.evaluate(() => Sim.state.model.options.pins), 1);
    await shot(mobile, name + '-settings');
    await mobile.locator('#btn-settings').click();
    await mobile.locator('#btn-about').click();
    await shot(mobile, name + '-about');
    await mobile.locator('#about-close').click();
    await mobile.close();
  }
  const reduced = await open({ width: 1280, height: 900 }, { reducedMotion: 'reduce' });
  assert.equal(await reduced.evaluate(() => Sim.state.paused), true);
  await reduced.close();
  assert.deepEqual(errors, []);
  console.log('PASS: DPR ' + dpr + ', all stations, refusal, each callback owner, world controls, keyboard, dialog, phone, landscape, reduced motion.');
  console.log('Stations: ' + [...visited].join(' → '));
  console.log('Screenshots: ' + out);
} finally {
  await browser.close();
}
