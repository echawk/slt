// slt-terminal.js — browser client for slt's web backend.
//
// Usage from any web app:
//
//   import { SltTerminal } from "./slt-terminal.js";
//   const term = new SltTerminal(document.querySelector("#term"), {
//     url: "ws://127.0.0.1:8080/ws?token=...",
//     theme: { background: "#1e1e2e", foreground: "#cdd6f4" },
//   });
//   term.addEventListener("title", (e) => (document.title = e.detail));
//
// or declaratively:
//
//   <script type="module" src="./slt-terminal.js"></script>
//   <slt-terminal url="ws://127.0.0.1:8080/ws?token=..."></slt-terminal>
//
// Theming: pass `theme` (see DEFAULT_THEME) to the constructor or setTheme();
// fonts can also come from the CSS custom properties --slt-font-family,
// --slt-font-size and --slt-line-height on the container.
//
// No dependencies; the protocol is documented in src/web/web-backend.lisp.

export const DEFAULT_THEME = Object.freeze({
  // A soft, low-glare dark theme (the same as slt's native default).
  foreground: "#c8ccd4",
  background: "#1c1f24",
  cursor: "#d8dee9",
  cursorText: null, // defaults to the background color
  selection: "rgba(143, 188, 187, 0.28)",
  // ANSI colors 0-15; 16-255 follow the xterm cube and grayscale ramp.
  palette: [
    "#2b2f37", "#d77a7a", "#9fbf7f", "#e0c285", "#7aa6da", "#c397d8", "#70c0b1", "#c8ccd4",
    "#5c6370", "#e88f8f", "#b5d495", "#ecd39b", "#93b8e3", "#d2aee3", "#8fd3c8", "#e6e9ef",
  ],
  boldIsBright: true,
  fontFamily: null, // falls back to --slt-font-family, then a monospace stack
  fontSize: null, // CSS length; falls back to --slt-font-size, then 14px
  lineHeight: null, // unitless factor; falls back to --slt-line-height, then 1.2
});

const ATTR = {
  bold: 1, faint: 2, italic: 4, underline: 8, blink: 16, inverse: 32,
  invisible: 64, strike: 128, wide: 256, wideSpacer: 512, doubleUnderline: 1024,
};

const KEYSYMS = {
  ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right",
  Home: "Home", End: "End", PageUp: "Prior", PageDown: "Next",
  Insert: "Insert", Delete: "Delete", Backspace: "BackSpace", Tab: "Tab",
  Enter: "Return", Escape: "Escape",
};
for (let i = 1; i <= 12; i++) KEYSYMS["F" + i] = "F" + i;

const MOD_SHIFT = 0x01, MOD_CONTROL = 0x04, MOD_META = 0x08, MOD_SUPER = 0x40;

function xtermPalette(base) {
  const colors = base.slice(0, 16);
  const level = (n) => (n === 0 ? 0 : 55 + n * 40);
  const hex = (r, g, b) => "#" + [r, g, b].map((v) => v.toString(16).padStart(2, "0")).join("");
  for (let i = 0; i < 216; i++) {
    colors.push(hex(level(Math.floor(i / 36)), level(Math.floor(i / 6) % 6), level(i % 6)));
  }
  for (let i = 0; i < 24; i++) {
    const v = 8 + i * 10;
    colors.push(hex(v, v, v));
  }
  return colors;
}

function rgbHex(value) {
  return "#" + (value & 0xffffff).toString(16).padStart(6, "0");
}

const STYLE = `
.slt-root { display: block; width: 100%; height: 100%; box-sizing: border-box; position: relative; overflow: hidden; outline: none;
  font-family: var(--slt-resolved-font-family); font-size: var(--slt-resolved-font-size);
  line-height: var(--slt-cell-h); background: var(--slt-bg); color: var(--slt-fg);
  cursor: text; }
.slt-screen { position: absolute; inset: 0; white-space: pre; font-variant-ligatures: none;
  user-select: text; }
.slt-screen ::selection { background: var(--slt-selection); }
.slt-row { height: var(--slt-cell-h); overflow: hidden; }
.slt-row span { display: inline-block; height: var(--slt-cell-h); vertical-align: top; }
.slt-wide { width: calc(2 * var(--slt-cell-w)); text-align: center; overflow: hidden; }
.slt-measure { position: absolute; visibility: hidden; white-space: pre; }
.slt-input { position: absolute; left: -9999px; top: 0; width: 1px; height: 1px; opacity: 0; }
`;

export class SltTerminal extends EventTarget {
  /**
   * @param {HTMLElement|ShadowRoot} container where the terminal is rendered
   * @param {{url?: string, theme?: object, autoConnect?: boolean, autoResize?: boolean}} options
   */
  constructor(container, options = {}) {
    super();
    this.container = container;
    this.url = options.url ?? null;
    this.autoResize = options.autoResize ?? true;
    this.rows = 0;
    this.cols = 0;
    this.title = "";
    this.cursor = [0, 0, true, "block"];
    this.lines = []; // runs per row, as received
    this.socket = null;
    this._theme = { ...DEFAULT_THEME };
    this._palette = xtermPalette(this._theme.palette);
    this._cell = { w: 8, h: 16 };
    this._build();
    this.setTheme(options.theme ?? {});
    if (this.url && options.autoConnect !== false) this.connect(this.url);
  }

  // ---- public API --------------------------------------------------------

  connect(url = this.url) {
    this.disconnect();
    this.url = url;
    this._requestedSize = null;
    const socket = new WebSocket(url);
    this.socket = socket;
    socket.addEventListener("open", () => this._emit("open"));
    socket.addEventListener("message", (event) => this._onMessage(JSON.parse(event.data)));
    socket.addEventListener("close", () => {
      if (this.socket === socket) this.socket = null;
      this._emit("close");
    });
    return this;
  }

  disconnect() {
    if (this.socket) {
      const socket = this.socket;
      this.socket = null;
      socket.close();
    }
  }

  dispose() {
    this.disconnect();
    this._resizeObserver?.disconnect();
    this.root.remove();
  }

  /** Merge THEME into the current theme and redraw. */
  setTheme(theme) {
    this._theme = { ...this._theme, ...theme };
    if (theme.palette) {
      const base = DEFAULT_THEME.palette.map((c, i) => theme.palette[i] ?? c);
      this._palette = xtermPalette(base);
      // Allow full 256-entry palettes too.
      for (let i = 16; i < Math.min(256, theme.palette.length); i++) this._palette[i] = theme.palette[i];
    }
    const style = this.root.style;
    const computed = getComputedStyle(this.container instanceof ShadowRoot ? this.container.host : this.container);
    const fontFamily = this._theme.fontFamily
      || computed.getPropertyValue("--slt-font-family").trim()
      || 'ui-monospace, "SF Mono", Menlo, "DejaVu Sans Mono", Consolas, monospace';
    const fontSize = this._theme.fontSize
      || computed.getPropertyValue("--slt-font-size").trim() || "14px";
    const lineHeight = Number(this._theme.lineHeight
      || computed.getPropertyValue("--slt-line-height").trim() || 1.2);
    style.setProperty("--slt-resolved-font-family", fontFamily);
    style.setProperty("--slt-resolved-font-size", fontSize);
    style.setProperty("--slt-fg", this._theme.foreground);
    style.setProperty("--slt-bg", this._theme.background);
    style.setProperty("--slt-selection", this._theme.selection);
    this._lineHeight = lineHeight;
    this._measure();
    this._redrawAll();
    this.fit();
  }

  get theme() {
    return { ...this._theme };
  }

  /** Recompute rows/columns from the container size and tell the server. */
  fit() {
    const width = this.root.clientWidth;
    const height = this.root.clientHeight;
    if (!width || !height) return;
    const cols = Math.max(1, Math.floor(width / this._cell.w));
    const rows = Math.max(1, Math.floor(height / this._cell.h));
    const pending = this._requestedSize;
    if (pending ? pending[0] === rows && pending[1] === cols
                : rows === this.rows && cols === this.cols) return;
    this.resize(rows, cols);
  }

  resize(rows, cols) {
    if (this._send({ t: "resize", rows, cols })) this._requestedSize = [rows, cols];
  }

  /** Scroll ROWS lines (negative is back through history). */
  scroll(rows) {
    if (rows) this._send({ t: "scroll", rows });
  }

  /** Send TEXT as if pasted (bracketed when the program asked for it). */
  paste(text) {
    this._send({ t: "paste", text });
  }

  /** Send a key in slt's backend form: CHAR, X11 KEYSYM and modifier STATE. */
  sendKey(char, keysym, state = 0) {
    this._send({ t: "key", char, keysym, state });
  }

  focus() {
    this.input.focus({ preventScroll: true });
  }

  /** The screen contents as plain text, one line per row. */
  text() {
    return this.lines.map((runs) => (runs || []).map((r) => r[0]).join("").trimEnd()).join("\n");
  }

  // ---- internals ---------------------------------------------------------

  _build() {
    const root = document.createElement("div");
    root.className = "slt-root";
    root.tabIndex = 0;
    const style = document.createElement("style");
    style.textContent = STYLE;
    const screen = document.createElement("div");
    screen.className = "slt-screen";
    const measure = document.createElement("span");
    measure.className = "slt-measure";
    measure.textContent = "MMMMMMMMMMMMMMMMMMMM";
    const input = document.createElement("textarea");
    input.className = "slt-input";
    input.setAttribute("autocapitalize", "off");
    input.setAttribute("autocomplete", "off");
    input.setAttribute("spellcheck", "false");
    root.append(style, screen, measure, input);
    this.container.append(root);
    this.root = root;
    this.screen = screen;
    this.measureEl = measure;
    this.input = input;

    root.addEventListener("mousedown", () => setTimeout(() => {
      if (!window.getSelection()?.toString()) this.focus();
    }));
    root.addEventListener("focus", () => this.focus());
    input.addEventListener("keydown", (e) => this._onKeyDown(e));
    input.addEventListener("input", () => {
      // Text from IME composition or dead keys arrives here.
      if (input.value && !this._composing) {
        this.paste(input.value);
        input.value = "";
      }
    });
    input.addEventListener("compositionstart", () => (this._composing = true));
    input.addEventListener("compositionend", () => {
      this._composing = false;
      if (input.value) this.paste(input.value);
      input.value = "";
    });
    let wheelRemainder = 0;
    root.addEventListener("wheel", (e) => {
      e.preventDefault();
      const lines = e.deltaMode === 1 ? e.deltaY
        : e.deltaMode === 2 ? e.deltaY * this.rows
        : e.deltaY / this._cell.h;
      wheelRemainder += lines;
      const whole = Math.trunc(wheelRemainder);
      wheelRemainder -= whole;
      this.scroll(whole);
    }, { passive: false });
    input.addEventListener("paste", (e) => {
      e.preventDefault();
      const text = e.clipboardData?.getData("text/plain");
      if (text) this.paste(text);
    });

    if (typeof ResizeObserver !== "undefined") {
      let pending = null;
      this._resizeObserver = new ResizeObserver(() => {
        if (!this.autoResize) return;
        clearTimeout(pending);
        pending = setTimeout(() => this.fit(), 50);
      });
      this._resizeObserver.observe(root);
    }
  }

  _measure() {
    const rect = this.measureEl.getBoundingClientRect();
    const fontSize = parseFloat(getComputedStyle(this.measureEl).fontSize) || 14;
    this._cell = {
      w: rect.width / this.measureEl.textContent.length || 8,
      h: Math.ceil(fontSize * this._lineHeight),
    };
    this.root.style.setProperty("--slt-cell-w", this._cell.w + "px");
    this.root.style.setProperty("--slt-cell-h", this._cell.h + "px");
  }

  _send(message) {
    if (this.socket && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(JSON.stringify(message));
      return true;
    }
    return false;
  }

  _emit(type, detail) {
    this.dispatchEvent(new CustomEvent(type, { detail }));
  }

  _onMessage(message) {
    switch (message.t) {
      case "hello":
        this._setSize(message.rows, message.cols);
        this.title = message.title || "";
        this._emit("title", this.title);
        // Size to our container only once the server's size is known.
        this.fit();
        break;
      case "resize":
        this._setSize(message.rows, message.cols);
        break;
      case "frame": {
        const oldCursorRow = this.cursor[0];
        this.cursor = message.cursor;
        const touched = new Set([oldCursorRow, this.cursor[0]]);
        for (const [row, runs] of message.rows) {
          this.lines[row] = runs;
          touched.add(row);
        }
        for (const row of touched) this._drawRow(row);
        this._emit("frame");
        break;
      }
      case "title":
        this.title = message.title;
        this._emit("title", this.title);
        break;
      case "bell":
        this._emit("bell");
        break;
      case "exit":
        this._emit("exit");
        break;
    }
  }

  _setSize(rows, cols) {
    this.rows = rows;
    this.cols = cols;
    if (this._requestedSize?.[0] === rows && this._requestedSize?.[1] === cols) {
      this._requestedSize = null;
    }
    this.lines.length = rows;
    while (this.screen.childElementCount < rows) {
      const row = document.createElement("div");
      row.className = "slt-row";
      this.screen.append(row);
    }
    while (this.screen.childElementCount > rows) this.screen.lastElementChild.remove();
  }

  _color(value, foreground, bold) {
    const tag = value >>> 24;
    if (tag === 1) {
      let index = value & 0xff;
      if (foreground && bold && this._theme.boldIsBright && index < 8) index += 8;
      return this._palette[index];
    }
    if (tag === 2) return rgbHex(value);
    return foreground ? this._theme.foreground : this._theme.background;
  }

  _styleRun(span, fg, bg, attrs, cursorHere) {
    let fgColor = this._color(fg, true, attrs & ATTR.bold);
    let bgColor = this._color(bg, false, false);
    if (attrs & ATTR.inverse) [fgColor, bgColor] = [bgColor, fgColor];
    if (attrs & ATTR.invisible) fgColor = bgColor;
    if (cursorHere) {
      bgColor = this._theme.cursor;
      fgColor = this._theme.cursorText || this._theme.background;
    }
    const s = span.style;
    s.color = fgColor;
    if (bgColor !== this._theme.background || cursorHere) s.backgroundColor = bgColor;
    if (attrs & ATTR.bold) s.fontWeight = "bold";
    if (attrs & ATTR.italic) s.fontStyle = "italic";
    if (attrs & ATTR.faint) s.opacity = "0.6";
    const lines = [];
    if (attrs & (ATTR.underline | ATTR.doubleUnderline)) lines.push("underline");
    if (attrs & ATTR.strike) lines.push("line-through");
    if (lines.length) {
      s.textDecorationLine = lines.join(" ");
      if (attrs & ATTR.doubleUnderline) s.textDecorationStyle = "double";
    }
  }

  _drawRow(row) {
    const element = this.screen.children[row];
    if (!element) return;
    const runs = this.lines[row] || [];
    const [cursorRow, cursorCol, cursorVisible, cursorShape] = this.cursor;
    const cursorColumn = cursorVisible && cursorRow === row ? cursorCol : -1;
    const fragment = document.createDocumentFragment();
    let column = 0;
    const addSpan = (text, fg, bg, attrs, cursorHere) => {
      const span = document.createElement("span");
      span.textContent = text;
      if (attrs & ATTR.wide) span.className = "slt-wide";
      this._styleRun(span, fg, bg, attrs, cursorHere && cursorShape === "block");
      if (cursorHere && cursorShape !== "block") {
        span.style.boxShadow = cursorShape === "bar"
          ? `inset 2px 0 0 ${this._theme.cursor}`
          : `inset 0 -2px 0 ${this._theme.cursor}`;
      }
      fragment.append(span);
    };
    for (const [text, fg, bg, attrs] of runs) {
      const width = attrs & ATTR.wide ? 2 : [...text].length;
      if (cursorColumn >= column && cursorColumn < column + width) {
        if (attrs & ATTR.wide) {
          addSpan(text, fg, bg, attrs, true);
        } else {
          const chars = [...text];
          const offset = cursorColumn - column;
          if (offset > 0) addSpan(chars.slice(0, offset).join(""), fg, bg, attrs, false);
          addSpan(chars[offset], fg, bg, attrs, true);
          if (offset + 1 < chars.length) addSpan(chars.slice(offset + 1).join(""), fg, bg, attrs, false);
        }
      } else {
        addSpan(text, fg, bg, attrs, false);
      }
      column += width;
    }
    if (cursorColumn >= column) {
      // Cursor beyond the last run: pad with blanks up to it.
      if (cursorColumn > column) addSpan(" ".repeat(cursorColumn - column), 0, 0, 0, false);
      addSpan(" ", 0, 0, 0, true);
    }
    element.replaceChildren(fragment);
  }

  _redrawAll() {
    for (let row = 0; row < this.rows; row++) this._drawRow(row);
  }

  _onKeyDown(e) {
    if (e.isComposing || this._composing) return;
    // Leave the platform's copy/paste and other shortcuts alone.
    if (e.metaKey && !e.ctrlKey) return;
    let state = 0;
    if (e.ctrlKey) state |= MOD_CONTROL;
    if (e.altKey) state |= MOD_META;
    if (e.metaKey) state |= MOD_SUPER;
    const keysym = KEYSYMS[e.key];
    if (keysym) {
      if (e.shiftKey) state |= MOD_SHIFT;
      this.sendKey("", keysym, state);
    } else if (e.key.length === 1 || [...e.key].length === 1) {
      let char = e.key;
      // With Alt (Option on macOS) the browser composes characters; use the
      // physical key instead so Alt behaves as Meta.
      if (e.altKey && /^Key[A-Z]$/.test(e.code)) {
        char = e.code.slice(3);
        if (!e.shiftKey) char = char.toLowerCase();
      } else if (e.altKey && /^Digit[0-9]$/.test(e.code)) {
        char = e.code.slice(5);
      }
      this.sendKey(char, char, state);
    } else {
      return; // Modifier-only and unknown keys.
    }
    e.preventDefault();
  }
}

/** <slt-terminal url="ws://..." theme='{"background":"#111"}'> */
export class SltTerminalElement extends HTMLElement {
  static get observedAttributes() {
    return ["url", "theme"];
  }

  connectedCallback() {
    if (!this.terminal) {
      const shadow = this.attachShadow({ mode: "open" });
      if (!this.style.display) this.style.display = "block";
      this.terminal = new SltTerminal(shadow, {
        url: this.getAttribute("url"),
        theme: this._parseTheme(),
      });
      for (const type of ["open", "close", "title", "bell", "exit", "frame"]) {
        this.terminal.addEventListener(type, (e) =>
          this.dispatchEvent(new CustomEvent(type, { detail: e.detail })));
      }
    }
  }

  disconnectedCallback() {
    this.terminal?.dispose();
    this.terminal = null;
    this.shadowRoot?.replaceChildren();
  }

  attributeChangedCallback(name, oldValue, value) {
    if (!this.terminal || oldValue === value) return;
    if (name === "url" && value) this.terminal.connect(value);
    if (name === "theme") this.terminal.setTheme(this._parseTheme());
  }

  _parseTheme() {
    try {
      return JSON.parse(this.getAttribute("theme") || "{}");
    } catch {
      return {};
    }
  }
}

if (typeof customElements !== "undefined" && !customElements.get("slt-terminal")) {
  customElements.define("slt-terminal", SltTerminalElement);
}
