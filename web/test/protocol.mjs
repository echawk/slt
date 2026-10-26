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
const text = () => lines.map((runs) => (runs || []).map((run) => run[0]).join("").trimEnd()).join("\n");
socket.onmessage = (event) => {
  const message = JSON.parse(event.data);
  if (message.t === "hello") hello = message;
  if (message.t === "mouse") tracking = message.tracking;
  if (message.t === "images") images = message;
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
socket.send(JSON.stringify({ t: "paste", text: "clear\n" }));

socket.send(JSON.stringify({ t: "paste", text: "printf '\\033[?1000h\\033[?1006h'; cat -v\n" }));
check("mouse tracking is announced", await until(() => tracking === true));
socket.send(JSON.stringify({ t: "mouse", action: "press", row: 2, col: 4, button: 1, state: 0 }));
socket.send(JSON.stringify({ t: "mouse", action: "release", row: 2, col: 4, button: 1, state: 0 }));
socket.send(JSON.stringify({ t: "key", char: "", keysym: "Return", state: 0 }));
check("mouse reports reach the program",
      await until(() => text().includes("^[[<0;5;3M^[[<0;5;3m")));
// Leave the shell as we found it for the browser checks.
socket.send(JSON.stringify({ t: "key", char: "c", keysym: "c", state: 4 }));
socket.send(JSON.stringify({ t: "paste", text: "printf '\\033[?1000l\\033[?1006l'; clear\n" }));
check("mouse tracking is withdrawn", await until(() => tracking === false));
socket.close();
process.exit(process.exitCode ?? 0);
