# Escape sequences

Terminal programs control the screen with ECMA-48 (ISO 6429) control
functions: C0 controls, escape sequences (`ESC` …), control sequences
(`CSI` = `ESC [` …) and control strings (`OSC` = `ESC ]`, `DCS` = `ESC P`,
`APC` = `ESC _`, `PM`, `SOS`). On top of the standard sit the DEC private
modes and many extensions from xterm, kitty, iTerm2 and others.

slt divides this work in two:

- **The engine** (ghostty, lispvt or libvterm) is the VT emulator: the
  screen, cursor, scrolling, SGR attributes, DEC modes, DA/DSR replies and
  the title (OSC 0/2). Each engine implements this on its own. `make
  conformance` runs the same byte streams through every engine and compares
  the screens.
- **slt itself**, in front of the engine, implements what should behave the
  same whatever engine runs and what concerns the window rather than the
  screen: images, OSC strings, the Kitty keyboard protocol, synchronized
  output and a few queries.

## How output flows

```
child ─► reader thread ─► ecma48 scanner ─► interceptor ─┬─► engine (in order)
                                                       └─► slt's handlers
```

1. **`lib/ecma48`** scans the bytes into events: text runs, C0 controls,
   escape sequences, control sequences and control strings. It follows the
   DEC/xterm parser conventions: Paul Williams' state machine, BEL ending
   OSC, and CAN/SUB aborting. It is incremental: a sequence split across
   reads is reported once, whole. It also allocates nothing for ordinary
   output.
   - Every event carries its raw bytes.
   - UTF-8 continuation bytes are text, never C1 controls.
   - A C0 control inside a sequence is reported before the sequence, as
     terminals execute it at once.
2. **The interceptor** (`src/sequences.lisp`) does one of four things with
   each event:
   - passes it to the engine;
   - consumes it (images, OSC strings slt handles, keyboard-protocol
     sequences, queries slt answers);
   - passes it and then acts on it, so that the engine's state is current:
     modes, erasures, RIS;
   - or acts on it and then passes it (hyperlinked text).

   Passed bytes are coalesced into large slices of the input, so the engine
   sees its usual stream.
3. **The handlers** (`src/controls.lisp`, `src/images.lisp`) act, and write
   any reply to the child.

## What slt implements

### Graphics

| sequence | |
|---|---|
| `DCS P1;P2;P3 q … ST` | SIXEL images, decoded with libsixel (`lib/cl-libsixel`) |
| `APC G … ST` | the Kitty graphics protocol (`lib/kitty-graphics`): every transmission medium and format, placements, relative placements, Unicode placeholders, deletion and animation |
| `CSI ? 80 h/l`, `CSI ? 8452 h/l` | DECSDM and sixel-scrolls-right, observed |
| `CSI ? 80 $ p`, `CSI ? 8452 $ p` | DECRQM for them, answered |
| `CSI ? Pi ; Pa S` | XTSMGRAPHICS: color registers and sixel geometry |
| `CSI c` | DA1, answered with sixel support (`?62;4;22c`) |
| `CSI J`, `CSI K`, `CSI X`, `ESC c`, `CSI ? 1047/1049 h` | observed, to erase images |

See the README's *Terminal graphics* section for behavior.

### OSC strings (`lib/terminal-osc`)

| OSC | | |
|---|---|---|
| 4 | palette colors | set and query (`?`); per window |
| 10, 11, 12, 17, 19 | foreground, background, cursor, selection background, selection foreground | set and query; 19 is queried as the foreground |
| 104, 110, 111, 112, 117, 119 | reset colors | |
| 7 | working directory (`file://host/path`) | remembered for local hosts; new windows (Command+N) open there |
| 8 | hyperlinks | underlined under the pointer; Command-click (Super-click) opens them, while their text is still on screen; only `*hyperlink-schemes*` (http, https, mailto) |
| 9, 777, 99 | desktop notifications (iTerm2, rxvt, kitty) | osascript on macOS, notify-send elsewhere; rate-limited; kitty's support query is answered |
| 9;4 | progress (ConEmu) | `frontend-set-progress`: the Dock icon's badge on macOS (a percentage, `!` for an error, `…` while busy) |
| 52 | clipboard | writing by default; reading only when `*clipboard-access*` is `:read-write` |
| 133 | shell-integration marks | prompts (A), output starts (C) and ends with exit status (D) are remembered; Super+Up/Down (Control+Shift+Up/Down) jumps between prompts, with Shift (Meta) between those of failed commands; Super+Shift+O (Control+Shift+O) copies the last command's output, read from the scrollback |

Other OSC strings (the title among them) go to the engine.

### Keyboard (`lib/kitty-keyboard`)

The [Kitty keyboard protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/):

- `CSI = flags ; mode u`, `CSI > flags u`, `CSI < n u` and `CSI ? u` are
  handled, with separate flag stacks for the main and alternate screens.
- While flags are set, keys are encoded by the protocol instead of by the
  engine.
- Frontends report presses, repeats and releases, and modifier keys alone
  (the ltk frontend presses only).  Releases are sent only for keys whose
  press reached the child.

### Other

| sequence | |
|---|---|
| `CSI ? 2026 h/l` | synchronized output: renders wait until `l`, at most `*synchronized-output-timeout-ms*` (150) |
| `CSI ? 1016 h/l` | SGR-Pixels: mouse reports in pixels from the grid's top left, from 0 (as Ghostty) |
| `CSI ? 2031 h/l` | color scheme notifications: `CSI ? 997 ; 1 n` (dark) or `2` (light) when the background's lightness crosses over |
| `CSI ? 2048 h/l` | in-band resize: `CSI 48 ; rows ; columns ; height ; width t` (pixels) on every resize and when enabled |
| `CSI ? 1016/2026/2031/2048 $ p` | DECRQM for them |
| `CSI ? 996 n` | the color scheme: `CSI ? 997 ; 1 n` dark, `2` light |
| `CSI 14 t`, `16 t`, `18 t`, `19 t` | XTWINOPS: the text area (`4;h;w`) and a cell (`6;h;w`) in pixels, the text area in cells (`8;rows;columns`, `9;...`) |
| `DCS + q Pt ST` | XTGETTCAP: `TN` (slt), `Co`/`colors` (256), `RGB`, `Tc`, `Su`, `Smulx`, `Setulc`, `setrgbf`, `setrgbb`, `Ms`, `Ss`, `Se`, `Sync`, `BE`, `BD`, `PS`, `PE` |
| `DCS $ q Pt ST` | DECRQSS: `m` (SGR), `r` (margins) and `SP q` (cursor style) from lispvt; other engines answer it themselves |
| `CSI > q` | XTVERSION: `DCS > \| slt VERSION ST` |

## Settings

| variable | default | |
|---|---|---|
| `*sixel-images*`, `*kitty-graphics-images*` | `t` | `nil` passes that protocol to the engine untouched |
| `*clipboard-access*` | `:write` | `nil`, `:write` or `:read-write` for OSC 52 |
| `*clipboard-limit*` | 1 MiB | most characters OSC 52 may set |
| `*notifications*` | `:unfocused` | `:always`, `:unfocused` or `nil` |
| `*synchronized-output-timeout-ms*` | 150 | |

## Security

Output can come from any program, including over SSH:

- **Clipboard:** OSC 52 cannot read the clipboard unless you allow it.
- **Notifications:** they are rate-limited (five per ten seconds), and
  their text is passed to the notifier as an argument, never through a
  shell.
- **Working directory:** OSC 7 from a remote host is ignored.
- **Hyperlinks:** their targets are hidden behind their text, so only
  `*hyperlink-schemes*` (http, https, mailto) are opened, and only by your
  click. `file:` and `ssh:` links can run programs.
- **Replies:** nothing a program sends is echoed back in a reply, except
  OSC 99 ids limited to the protocol's identifier characters.
- **Sizes:** OSC strings are collected up to 4 MiB, and OSC 52 data is
  checked against `*clipboard-limit*` before it is decoded.
- **Synchronized output:** repeating `CSI ? 2026 h` does not extend the
  timeout.
- **Graphics:** see `lib/kitty-graphics/README.md` for its file and
  shared-memory checks, and for its size limits.

## Underlines

The engines keep underline styles (`SGR 4:1` to `4:5`: single, double,
curly, dotted, dashed) and colors (`SGR 58`/`59`), which every frontend but
ltk draws:

| engine | styles | colors |
|---|---|---|
| lispvt, ghostty | all | yes |
| libvterm | single, double, curly | no |

## The web frontend's colors

The web frontend keeps its own theme for the colors a program has not
changed: OSC 4 and 10-19 changes are sent to it, the defaults are not.
