// Drive the demo page in headless Chrome over the DevTools protocol: type a
// command, switch themes, check the result and save screenshots.
// usage: node web/test/browser.mjs CHROME http://127.0.0.1:PORT TOKEN OUTDIR
import { spawn } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const [chromePath, base, token, outDir] = process.argv.slice(2);
const check = (label, ok) => {
  console.log(`${ok ? "ok  " : "FAIL"} ${label}`);
  if (!ok) process.exitCode = 1;
};
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const port = 9300 + Math.floor(Math.random() * 500);
const chrome = spawn(chromePath, [
  "--no-first-run", `--user-data-dir=${mkdtempSync(join(tmpdir(), "slt-chrome-"))}`,
  `--remote-debugging-port=${port}`, "about:blank"], { stdio: "ignore" });

try {
  let targets;
  for (let i = 0; i < 100 && !targets; i++) {
    try { targets = await (await fetch(`http://127.0.0.1:${port}/json`)).json(); } catch { await sleep(100); }
  }
  const page = targets.find((t) => t.type === "page");
  const cdp = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise((r) => (cdp.onopen = r));
  let nextId = 0;
  const pending = new Map();
  cdp.onmessage = (e) => {
    const m = JSON.parse(e.data);
    pending.get(m.id)?.(m.result);
    pending.delete(m.id);
  };
  const send = (method, params = {}) => new Promise((resolve) => {
    const id = ++nextId;
    pending.set(id, resolve);
    cdp.send(JSON.stringify({ id, method, params }));
  });
  const evaluate = async (expression) =>
    (await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true })).result.value;
  const screenshot = async (name) => {
    const shot = await send("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(outDir, name), Buffer.from(shot.data, "base64"));
  };
  const terminalText = () => evaluate("document.getElementById('term').terminal.text()");

  await send("Page.enable");
  if (process.env.SLT_DEBUG) {
    await send("Page.addScriptToEvaluateOnNewDocument", { source: `
      window.__sent = [];
      window.__received = [];
      const OriginalWebSocket = WebSocket;
      window.WebSocket = class extends OriginalWebSocket {
        constructor(...args) {
          super(...args);
          this.addEventListener("message", (e) => {
            const m = JSON.parse(e.data);
            window.__received.push(m.t === "frame" ? "frame:" + m.rows.length : e.data.slice(0, 80));
          });
        }
      };
      const original = WebSocket.prototype.send;
      WebSocket.prototype.send = function (data) {
        window.__sent.push([this.readyState, String(data).slice(0, 80)]);
        return original.call(this, data);
      };` });
  }
  await send("Emulation.setDeviceMetricsOverride", { width: 900, height: 440, deviceScaleFactor: 2, mobile: false });
  await send("Page.navigate", { url: `${base}/?token=${token}` });
  for (let i = 0; i < 100 && !(await terminalText())?.includes("$"); i++) await sleep(50);
  check("page connects and shows a prompt", (await terminalText()).includes("$"));

  for (const key of "printf '\\033[1;31mbold red\\033[0m \\033[4munder\\033[0m \\033[38;2;255;128;0mtrue\\033[0m 中文 é\\n'") {
    await send("Input.dispatchKeyEvent", { type: "keyDown", key, text: key });
  }
  await send("Input.dispatchKeyEvent", { type: "keyDown", key: "Enter", code: "Enter", windowsVirtualKeyCode: 13 });
  for (let i = 0; i < 100 && !/\nbold red under true/.test(await terminalText()); i++) await sleep(50);
  check("keyboard input runs a command", /\nbold red under true 中文 é/.test(await terminalText()));
  const sizeOf = () => evaluate("[document.getElementById('term').terminal.rows, document.getElementById('term').terminal.cols]");
  let size = await sizeOf();
  for (let i = 0; i < 40 && !(size[0] > 8 && size[1] > 50); i++) { await sleep(50); size = await sizeOf(); }
  check("terminal is sized to the page (not the previous client's 8x50)", size[0] > 8 && size[1] > 50);
  await screenshot("web-default.png");
  if (process.env.SLT_DEBUG) {
    console.log(await evaluate(`JSON.stringify((({rows, cols, cursor}) => ({rows, cols, cursor}))(document.getElementById('term').terminal))`));
    console.log(await evaluate(`JSON.stringify([document.getElementById('term').terminal.root.clientWidth, document.getElementById('term').terminal.root.clientHeight, document.getElementById('term').terminal._cell, document.getElementById('term').terminal.socket?.readyState])`));
    console.log("measure", await evaluate("(() => { const t = document.getElementById('term').terminal; return JSON.stringify([t.root.clientWidth, t.root.clientHeight, t._cell, t._requestedSize, t.rows, t.cols, document.getElementById('term').getBoundingClientRect().height]); })()"));
    console.log("received", await evaluate("JSON.stringify(window.__received)"));
    console.log("sent", await evaluate("JSON.stringify(window.__sent.filter(([, d]) => !d.includes('key')))"));
    await evaluate("document.getElementById('term').terminal.fit()");
    await sleep(800);
    console.log("after fit", await evaluate(`JSON.stringify((({rows, cols, cursor}) => ({rows, cols, cursor}))(document.getElementById('term').terminal))`));
  }

  await evaluate(`(() => { const s = document.getElementById("theme");
    s.value = "Solarized Dark"; s.dispatchEvent(new Event("change")); })()`);
  await sleep(200);
  const background = await evaluate(
    "getComputedStyle(document.getElementById('term').shadowRoot.querySelector('.slt-root')).backgroundColor");
  check("theme switch recolors the terminal", background === "rgb(0, 43, 54)");
  await screenshot("web-solarized.png");

  // A program's color changes (OSC 11) show over the theme, and resetting
  // them (OSC 111) brings the theme back.
  const typeLine = async (line) => {
    for (const key of line) await send("Input.dispatchKeyEvent", { type: "keyDown", key, text: key });
    await send("Input.dispatchKeyEvent", { type: "keyDown", key: "Enter", code: "Enter", windowsVirtualKeyCode: 13 });
  };
  const rootBackground = () => evaluate(
    "getComputedStyle(document.getElementById('term').shadowRoot.querySelector('.slt-root')).backgroundColor");
  await typeLine("printf '\\033]11;#123456\\007'");
  let changed = await rootBackground();
  for (let i = 0; i < 60 && changed !== "rgb(18, 52, 86)"; i++) { await sleep(50); changed = await rootBackground(); }
  check("OSC 11 recolors the background", changed === "rgb(18, 52, 86)");
  await typeLine("printf '\\033]111\\007'");
  let restored = await rootBackground();
  for (let i = 0; i < 60 && restored !== "rgb(0, 43, 54)"; i++) { await sleep(50); restored = await rootBackground(); }
  check("OSC 111 restores the theme's background", restored === "rgb(0, 43, 54)");
  cdp.close();
} finally {
  chrome.kill();
}
process.exit(process.exitCode ?? 0);
