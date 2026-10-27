# slt web frontend

`slt-terminal.js` is a dependency-free ES module that renders an slt
terminal in the browser. slt's `web` frontend serves it, together with a
WebSocket carrying the screen to the browser and input back to slt. You can
use the bundled page, or embed the terminal in your own web app.

## Running

```sh
slt --frontend web                       # prints http://127.0.0.1:8080/?token=...
slt --frontend web --web-port 9000
```

From Lisp:

```lisp
(setf slt:*web-port* 9000)
(slt:launch-terminal :frontend :web)
```

## Security

Anyone who can open the socket can run commands as you, so:

- The server listens on `127.0.0.1` by default.
- Every page and socket request must carry the access token. A random token is
  generated per run unless `slt:*web-token*` is set.
- `slt:*web-allowed-origins*` can restrict which pages may open the socket,
  for example `'("https://myapp.example")`.
- The connection is plain HTTP and WS, meant for localhost. Put a TLS proxy in
  front of it before exposing it anywhere else.
- `slt-terminal.js` contains no secrets. It is served with
  `Access-Control-Allow-Origin: *` so other origins can import it.

## Embedding

Import the module from the slt server, or copy `slt-terminal.js` into your
app. It has no dependencies and needs no build step.

```js
import { SltTerminal } from "http://127.0.0.1:8080/slt-terminal.js";

const term = new SltTerminal(document.getElementById("terminal"), {
  url: "ws://127.0.0.1:8080/ws?token=TOKEN",
  theme: { background: "#1e1e2e", foreground: "#cdd6f4", cursor: "#f5e0dc" },
});
term.addEventListener("title", (e) => (document.title = e.detail));
term.addEventListener("exit", () => console.log("shell exited"));
```

Or use the custom element. Importing the module registers `<slt-terminal>`:

```html
<script type="module" src="http://127.0.0.1:8080/slt-terminal.js"></script>
<slt-terminal url="ws://127.0.0.1:8080/ws?token=TOKEN"
              theme='{"background": "#002b36", "foreground": "#839496"}'
              style="height: 400px"></slt-terminal>
```

The terminal fills its container and resizes the remote terminal to fit.
Give the container a size.

### API

| | |
|---|---|
| `new SltTerminal(container, {url, theme, autoConnect, autoResize})` | mounts into an element or shadow root |
| `connect(url)` / `disconnect()` / `dispose()` | connection lifecycle |
| `setTheme(partialTheme)` | merges into the current theme and redraws; no server round trip |
| `fit()` / `resize(rows, cols)` | size to the container, or to an explicit size; both send the cell size too |
| `paste(text)` | bracketed when the program requested it |
| `scroll(rows)` | scroll back (negative) or forward through history; the mouse wheel does this |
| `sendKey(char, keysym, state)` | raw key input (X11 keysym names and modifier bits) |
| `focus()`, `text()`, `rows`, `cols`, `title`, `theme` | |
| `drawImages(sources, items)` | draws image placements; override it to draw them yourself (below) |
| `progress` | the last progress a program reported (OSC 9;4), `{state, value}`, or null |
| events `open`, `close`, `title` (detail: string), `bell`, `exit`, `frame` | |
| event `progress` (detail: `{state, value}`) | state 0 none, 1 `value` percent, 2 error, 3 indeterminate, 4 paused |
| event `link` (detail: URL), cancelable | a link the user Command-clicked (Control-clicked elsewhere); unless cancelled, `http`, `https`, `mailto` and `ftp` links open in a new tab |

The `<slt-terminal>` element re-dispatches the same events, exposes the
instance as `.terminal`, and reacts to changes of its `url` and `theme`
attributes.

### Images

The terminal sends the server its cell size with every resize (and resizes
again when only the cell size changes, after a font change), so programs
size images for the font in use, and the server lays images out in those
cells. The client scales the layout to its exact cell size, since the server
works in whole pixels.

To draw images yourself, subclass `SltTerminal` and override
`drawImages(sources, items)`. It is called with every images message, and
again when the cell size changes:

- `sources`: each image once, as `{id, iw, ih, rgba}`, the RGBA pixels in
  base64.
- `items`: the placements in drawing order, as `{id, x, y, w, h, z, sx, sy,
  sw, sh, iw, ih}`. Each draws the rectangle `sx, sy, sw, sh` of image `id`
  into `x, y, w, h`, in CSS pixels from the terminal's top left, already
  scaled to its cells. `z` below 0 is under the text.

`drawImages` is part of the API; the methods starting with `_` are not.

### Theming

The server sends cell colors unresolved (default, palette index or RGB), so
themes are applied entirely in the browser and can be switched live. A theme
is any subset of:

```js
{
  foreground: "#e5e5e5", background: "#000000",
  cursor: "#e5e5e5", cursorText: null,           // cursorText defaults to background
  selection: "rgba(128, 160, 255, 0.35)",
  palette: [/* 16 ANSI colors, or all 256 */],
  boldIsBright: true,                             // bold text uses colors 8-15 for 0-7
  fontFamily: null, fontSize: null, lineHeight: null,
}
```

You can also set fonts with CSS custom properties on the container:
`--slt-font-family`, `--slt-font-size` and `--slt-line-height`.

## Protocol

The server and browser exchange JSON text frames over `/ws?token=...`.
`src/web/web-frontend.lisp` documents the messages. In short, the server sends
`hello`, `frame` (changed rows as runs of `[text, fg, bg, attrs]` plus the
cursor), `images`, `colors`, `mouse`, `resize`, `title`, `bell` and `exit`.
The client sends `key`, `paste`, `scroll`, `mouse` and `resize`. Any client
that speaks this protocol can drive slt.

The client reports where the pointer is (`mouse` motion) also while the
program does not track the mouse, so that the link under it is underlined,
and a press with Super (Command) on a link makes the server answer `open`
with its URL, for the browser to open. `mouse` messages may carry the
pointer's pixel, `x` and `y` in the server's cells, for programs that ask
for pixels (mode 1016). `progress` passes on OSC 9;4.

A client should put its cell size in CSS pixels in `resize`, as `cellWidth`
and `cellHeight`. Without them the server assumes 8 x 16 cells, so images
drift from the text whenever the font's cells are another size. `images`
messages give the cell size their placements were laid out with, as `cw` and
`ch`: scale `x`, `y`, `w` and `h` by your cell size over it.

## Testing

`make web-test` (or `scripts/web-test`) starts a throwaway server, then runs:

- `test/protocol.mjs`: checks the protocol and security with Node's WebSocket.
- `test/browser.mjs`: if headless Chrome is available, drives the bundled page
  over the DevTools protocol. That covers keyboard input, resizing and theme
  switching, and saves screenshots in `build/web-test/`.

Set `SLT_CHROME`, or run `scripts/fetch-chrome-headless` to download Google's
`chrome-headless-shell` into `.deps/`.
