// Protocol checks against a running slt web frontend.
// usage: node web/test/protocol.mjs http://127.0.0.1:PORT TOKEN
const [base, token] = process.argv.slice(2);
const wsBase = base.replace(/^http/, "ws");
const check = (label, ok) => {
  console.log(`${ok ? "ok  " : "FAIL"} ${label}`);
  if (!ok) process.exitCode = 1;
};
const until = async (predicate, ms = 5000) => {
  for (const end = Date.now() + ms; Date.now() < end; await new Promise((r) => setTimeout(r, 20))) {
    if (predicate()) return true;
  }
  return false;
};

let response = await fetch(`${base}/`);
check("page without token is refused", response.status === 403);
response = await fetch(`${base}/?token=${token}`);
check("page with token is served", response.status === 200 && (await response.text()).includes("slt-terminal"));
response = await fetch(`${base}/slt-terminal.js`);
check("module is served cross-origin",
      response.status === 200 && response.headers.get("access-control-allow-origin") === "*");

const refused = await new Promise((resolve) => {
  const socket = new WebSocket(`${wsBase}/ws?token=wrong`);
  socket.onopen = () => { socket.close(); resolve(false); };
  socket.onerror = () => resolve(true);
});
check("socket with a wrong token is refused", refused);

const socket = new WebSocket(`${wsBase}/ws?token=${token}`);
const lines = [];
let hello = null;
let tracking = null;
let images = null;
const withPixels = new Set(); // images whose pixels some message carried
let cursor = null;
const cursorSeen = { shown: false, hidden: false };
const opened = [];
const progress = [];
const text = () => lines.map((runs) => (runs || []).map((run) => run[0]).join("").trimEnd()).join("\n");
socket.onmessage = (event) => {
  const message = JSON.parse(event.data);
  if (message.t === "hello") hello = message;
  if (message.t === "mouse") tracking = message.tracking;
  if (message.t === "images") {
    images = message;
    for (const source of message.sources) if (source.rgba) withPixels.add(source.id);
  }
  if (message.t === "open") opened.push(message.url);
  if (message.t === "progress") progress.push([message.state, message.value]);
  if (message.t === "frame") {
    cursor = message.cursor;
    cursorSeen[cursor[2] ? "shown" : "hidden"] = true;
  }
  if (message.t === "frame") for (const [row, runs] of message.rows) lines[row] = runs;
};
await new Promise((resolve) => (socket.onopen = resolve));
check("hello received", await until(() => hello?.v === 1));
check("initial frame shows a prompt", await until(() => text().includes("$")));

socket.send(JSON.stringify({ t: "resize", rows: 8, cols: 50 }));
for (const char of "printf '\\033[31mred\\033[0m %s\\n' ok") {
  socket.send(JSON.stringify({ t: "key", char, keysym: char, state: 0 }));
}
socket.send(JSON.stringify({ t: "key", char: "", keysym: "Return", state: 0 }));
check("command output arrives", await until(() => /\nred ok/.test(text())));
const red = lines.flat().find((run) => run && run[0] === "red");
check("colors are sent as grid tags (palette 1)", red?.[1] === (0x01000000 | 1));
check("resize is applied (50 columns)",
      await until(() => lines.some((runs) => runs && runs.map((run) => run[0]).join("").length === 50)));
socket.send(JSON.stringify({ t: "paste", text: "echo pasted\n" }));
check("paste reaches the shell", await until(() => /\npasted/.test(text())));

// A client with 10 x 20 cells: a 2 x 1 cell image covers 20 x 20 pixels.
socket.send(JSON.stringify({ t: "resize", rows: 8, cols: 50, cellWidth: 10.4, cellHeight: 20 }));
socket.send(JSON.stringify({ t: "paste",
  text: "printf '\\033_Ga=T,f=32,s=1,v=1,c=2,r=1;AAAA/w==\\033\\\\'; echo\n" }));
check("images are laid out in the client's cells",
      await until(() => images?.cw === 10 && images?.ch === 20
                  && images.items.some((item) => item.w === 20 && item.h === 20)));
check("the image's pixels come with it", withPixels.has(images?.sources?.[0]?.id));
// Typing redraws the grid and resends the images, but not their pixels.
const first = images;
socket.send(JSON.stringify({ t: "key", char: "x", keysym: "x", state: 0 }));
socket.send(JSON.stringify({ t: "key", char: "", keysym: "BackSpace", state: 0 }));
check("pixels the client has are not sent again",
      await until(() => images !== first && images.sources.length === 1
                  && images.sources[0].rgba === undefined));
socket.send(JSON.stringify({ t: "paste", text: "clear\n" }));

socket.send(JSON.stringify({ t: "paste", text: "printf '\\033[?1000h\\033[?1006h'; cat -v\n" }));
check("mouse tracking is announced", await until(() => tracking === true));
socket.send(JSON.stringify({ t: "mouse", action: "press", row: 2, col: 4, button: 1, state: 0 }));
socket.send(JSON.stringify({ t: "mouse", action: "release", row: 2, col: 4, button: 1, state: 0 }));
socket.send(JSON.stringify({ t: "key", char: "", keysym: "Return", state: 0 }));
check("mouse reports reach the program",
      await until(() => text().includes("^[[<0;5;3M^[[<0;5;3m")));
const paste = (text) => socket.send(JSON.stringify({ t: "paste", text }));
const key = (char, keysym, state = 0) => socket.send(JSON.stringify({ t: "key", char, keysym, state }));
const rowOf = (needle) => text().split("\n").findIndex((line) => line.includes(needle));
key("c", "c", 4);
paste("printf '\\033[?1000l\\033[?1006l'; clear\n");
check("mouse tracking is off again", await until(() => tracking === false));

// Emoji sequences are one wide cell (grapheme clusters).
paste("printf '\\360\\237\\221\\250\\342\\200\\215\\360\\237\\221\\251\\342\\200\\215\\360\\237\\221\\247x\\n'\n");
const family = "\u{1F468}‍\u{1F469}‍\u{1F467}";
check("an emoji ZWJ sequence arrives as one wide cell",
      await until(() => lines.flat().some((run) => run && run[0] === family && (run[3] & 0x100))));

// The pointer underlines the URL under it; Command-click opens it here.
paste("echo see https://example.com/x ok\n");
const urlLine = () => text().split("\n").findIndex((line) => line.startsWith("see https"));
check("a URL is printed", await until(() => urlLine() >= 0));
const urlRow = urlLine();
socket.send(JSON.stringify({ t: "mouse", action: "motion", row: urlRow, col: 10, button: 0, state: 0 }));
check("the URL under the pointer is underlined",
      await until(() => (lines[urlRow] || []).some(
        (run) => run[0].includes("https://example.com/x") && (run[3] & 0x8))));
socket.send(JSON.stringify({ t: "mouse", action: "press", row: urlRow, col: 10, button: 1, state: 0x40 }));
check("Command-click opens the URL in the browser",
      await until(() => opened.includes("https://example.com/x")));

// OSC 9;4 progress reaches the browser.
paste("printf '\\033]9;4;1;42\\007'\n");
check("progress reaches the browser", await until(() => progress.some(([s, v]) => s === 1 && v === 42)));

// Programs asking for pixels (mode 1016) get them, in the server's cells;
// the cell size report gives this client's cells (10 x 20, sent above).
paste("clear; printf '\\033[16t\\033[?1000h\\033[?1006h\\033[?1016hRE''ADY'; cat -v\n");
check("pixel mouse mode is set", await until(() => text().includes("READY")));
socket.send(JSON.stringify({ t: "mouse", action: "press", row: 2, col: 4, button: 1, state: 0, x: 45, y: 50 }));
socket.send(JSON.stringify({ t: "mouse", action: "release", row: 2, col: 4, button: 1, state: 0, x: 45, y: 50 }));
key("", "Return");
check("the cell size report and pixel mouse reports reach the program",
      await until(() => text().includes("^[[6;20;10t") && text().includes("^[[<0;45;50M^[[<0;45;50m")));
key("c", "c", 4);
paste("printf '\\033[?1000l\\033[?1006l\\033[?1016l'; clear\n");

// Narrowing the terminal rewraps its text (lispvt reflows, as ghostty).
paste("clear; printf 'ab%.0s' 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; echo\n");
const thirty = "ab".repeat(15);
check("a 30-cell line is printed", await until(() => text().includes(thirty)));
socket.send(JSON.stringify({ t: "resize", rows: 8, cols: 20, cellWidth: 10, cellHeight: 20 }));
check("narrowing to 20 columns rewraps it",
      await until(() => text().includes("ab".repeat(10) + "\n" + "ab".repeat(5))));
socket.send(JSON.stringify({ t: "resize", rows: 8, cols: 50, cellWidth: 10, cellHeight: 20 }));
check("widening joins it again", await until(() => text().includes(thirty)));

// A blinking cursor (DECSCUSR 1) blinks for the browser too.
paste("printf '\\033[1 q'\n");
cursorSeen.shown = cursorSeen.hidden = false;
check("the cursor blinks", await until(() => cursorSeen.shown && cursorSeen.hidden, 4000));
paste("printf '\\033[2 q'\n");

// Leave the shell as we found it for the browser checks.
socket.send(JSON.stringify({ t: "key", char: "c", keysym: "c", state: 4 }));
socket.send(JSON.stringify({ t: "paste", text: "printf '\\033[?1000l\\033[?1006l'; clear\n" }));
check("mouse tracking is withdrawn", await until(() => tracking === false));
socket.close();
process.exit(process.exitCode ?? 0);
