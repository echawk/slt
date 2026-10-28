# slt

A terminal emulator in Common Lisp. The terminal emulation layer (the
*engine*) and the user interface (the *frontend*) are separate pieces, and
either can be swapped, configured or extended from Lisp.

## Building and running

Needs SBCL, [ocicl](https://github.com/ocicl/ocicl) and a C compiler (GNU make;
`gmake` on the BSDs).

```sh
make                 # build bin/slt and the libraries it loads
bin/slt              # run it (or: make run ARGS='--engine libvterm')
make check           # unit tests and engine conformance
make install         # into /usr/local; PREFIX and DESTDIR are honored
make lean            # only this system's GUI and the lispvt engine (see below)
make lean-gtk        # the same with another frontend: gl gtk clx ltk web cocoa
make tiny            # the same as make lean-ltk
make app             # macOS: build/slt.app (make lean app for the lean build)
make install-app     # macOS: install slt.app into ~/Applications (APPDIR)
```

`make` fetches the Lisp dependencies into `ocicl/`, builds the native engines,
and dumps the executable into `bin/` together with its libraries. `bin/` is
self-contained: copy or tar it anywhere. `make install` puts it in
`$(LIBDIR)/slt` (default `/usr/local/lib/slt`) and links `$(BINDIR)/slt` to
it. `make uninstall` removes both.

| variable | default | |
|---|---|---|
| `PREFIX` | `/usr/local` | `BINDIR` and `LIBDIR` default to `$(PREFIX)/bin` and `$(PREFIX)/lib` |
| `DESTDIR` | | staging root for packages |
| `ENGINES` | `ghostty libvterm` | native engines to build in; `ENGINES=` gives a pure-Lisp build |
| `FRONTENDS` | `gl` off macOS | optional frontends to build in |
| `SBCL`, `OCICL` | `sbcl`, `ocicl` | |

For a package, run `make deps` while the network is available (building
ghostty also downloads Zig and Ghostty), then
`make && make install DESTDIR="$pkgdir" PREFIX=/usr`.

A lean build has one frontend, the pure-Lisp `lispvt` engine and the headless
frontend, without the other frontends, the REPL servers or other engines. It
fetches only the Lisp libraries its build loads, at the versions pinned in
`ocicl.csv`, into `build/lean-FRONTEND/ocicl`, and downloads no Zig or
Ghostty.

| command | frontend | needs at run time |
|---|---|---|
| `make lean` (`LEAN=1`) | the native one: cocoa on macOS, gl elsewhere | |
| `make lean-cocoa` (`LEAN=cocoa`) | cocoa (macOS only) | |
| `make lean-gl` (`LEAN=gl`) | gl | GLFW 3.3 or later; FreeType and fontconfig off macOS |
| `make lean-gtk` (`LEAN=gtk`) | gtk | GTK 3 |
| `make lean-clx` (`LEAN=clx`) | clx | an X server with RENDER; FreeType and fontconfig |
| `make lean-ltk` (`LEAN=ltk`, `make tiny`, `TINY=1`) | ltk | Tk's `wish` |
| `make lean-web` (`LEAN=web`) | web | a browser |

`make lean FRONTENDS=gtk` is the same as `make lean-gtk`. Add the target's
`LEAN=` setting to the other steps to use the lean build: `make lean-gtk
install`, `make lean-gl app` on macOS. For a package, pass it to every step:
`make deps LEAN=gtk`, then
`make LEAN=gtk && make install LEAN=gtk DESTDIR="$pkgdir" PREFIX=/usr`.

`scripts/lisp` runs SBCL with ASDF limited to this project, for working at a
REPL: everything is resolved from `ocicl.csv`, `ocicl/` and `lib/`.

## Engines

| engine | what it is | setup |
|---|---|---|
| `ghostty` | Ghostty's libghostty-vt, via cl-ghostty-vt; fastest, with scrollback and reflow | built by `make` (downloads Zig) |
| `lispvt` | pure Lisp, with scrollback and reflow; scanned with NEON on arm64 and SSE2 or AVX2 on x86-64 when SBCL has sb-simd (`lib/lispvt`, standalone) | none |
| `libvterm` | libvterm 0.3.3, the library Neovim uses (`lib/cl-libvterm`); truncates lines on resize | built by `make` |

By default slt uses the first engine in this order that is available.

Choose one with `--engine NAME`, `SLT_ENGINE=NAME`, `(launch-terminal :engine
:NAME)` or `(setf slt:*default-engine* :NAME)` in your init file. Engines with
native libraries live in their own `slt/NAME` systems, which load on first use.

Output is parsed on a reader thread, and the UI draws at most once per frame,
so heavy output never waits on rendering. Ghostty copies only changed rows
into the grid through a small C helper (`src/native/slt-ghostty.c`), which is
compiled on first use. On a 23 MB `cat`, ghostty manages about 140 MB/s
and libvterm about 12 MB/s. In the `slt/bench` throughput benchmark lispvt
keeps pace with ghostty: faster on full-screen redraws, somewhat slower on
plain scrolling text.

Scrollback (ghostty, lispvt): use the mouse wheel or trackpad, Shift+PageUp/PageDown,
and Shift+Home/End. Typing returns to the live screen. On the alternate
screen (vim, less), the wheel sends arrow keys instead.

Smooth scrolling (cocoa, optional): with `--smooth-scroll`,
`SLT_SMOOTH_SCROLL=1` or `(setf slt:*smooth-scrolling* t)` in the init file,
a trackpad moves the scrollback pixel by pixel, and the view settles on a
whole row when the gesture ends. Programs on the alternate screen or that
track the mouse still get whole rows.

On macOS 14 and later the cocoa frontend draws in step with the display (a
`CADisplayLink`, idle when nothing changes), once per refresh.

Selection and the clipboard (cocoa, gl and web frontends): drag to select,
double-click for a word (paths and URLs count as one) and triple-click for a
line. Copy with Command+C on macOS (Super+C elsewhere) or Control+Shift+C,
and paste with Command+V / Super+V or Control+Shift+V. Typing, scrolling or
output that changes the selected text clears the selection. Set
`*copy-on-select*` to copy whenever a selection is made. The bindings are
`clipboard-key-handler` on `*key-hook*`, and the selection color is the
palette's `:selection`.

`make conformance` (part of `make check`) runs the same byte streams through every available engine
and compares the resulting screens. `slt/bench` measures throughput.

## Frontends

| frontend | |
|---|---|
| `cocoa` | macOS default: AppKit window, Metal renderer, CoreText glyphs |
| `clx` | X11 window through [CLX](https://github.com/sharplispers/clx), the Lisp X protocol library; the `gl` frontend's FreeType glyphs, drawn with the RENDER extension (`slt/clx`; needs an X server) |
| `gl` | Linux/BSD default: GLFW window (X11 or Wayland), OpenGL 3.3 or OpenGL ES 3.0, FreeType glyphs found through font-discovery; also runs on macOS with CoreText glyphs |
| `gtk` | GTK 3 window drawn with Cairo and Pango, via [cl-cffi-gtk](https://github.com/crategus/cl-cffi-gtk) (`slt/gtk`; needs GTK 3) |
| `ltk` | Tk; portable fallback (needs `wish`) where `gl` cannot run |
| `web` | serves a browser terminal; see [web/README.md](web/README.md) |
| `headless` | no UI; for tests, scripting and remote frontends |

Choose one with `--frontend NAME` or `SLT_FRONTEND=NAME`.

## Escape sequences

The engines implement the VT core. In front of them, slt implements the
same way for every engine:

- **graphics:** SIXEL and Kitty;
- **colors:** OSC 4/10/11/12, set and query, per window;
- **clipboard:** OSC 52 (writing only, unless `*clipboard-access*` is
  `:read-write`);
- **hyperlinks:** OSC 8, underlined under the pointer and opened with
  Command-click;
- **working directory:** OSC 7 (new windows open there);
- **prompt marks:** OSC 133, with Super+Up/Down or Control+Shift+Up/Down to
  jump between prompts (add Shift, or Meta with Control+Shift, for failed
  commands) and Super+Shift+O or Control+Shift+O to copy the last command's
  output;
- **desktop notifications:** OSC 9/777/99;
- **the Kitty keyboard protocol,** with key repeats, releases and modifier
  keys;
- **synchronized output:** mode 2026;
- **XTVERSION.**

[doc/escape-sequences.md](doc/escape-sequences.md) describes how output
flows, the settings and the security choices. The reusable pieces live in
`lib/`:

- [`ecma48`](lib/ecma48/) scans control sequences;
- [`terminal-osc`](lib/terminal-osc/) parses OSC payloads;
- [`simd-base64`](lib/simd-base64/) decodes base64 into octets or RGBA;
- [`kitty-keyboard`](lib/kitty-keyboard/) implements the keyboard protocol.

## Terminal graphics

slt recognizes SIXEL DCS sequences, Kitty graphics APC sequences and
iTerm2 inline images (OSC 1337) before they reach the selected VT engine,
so graphics work with every engine.

SIXEL is decoded through `libsixel`. An image leaves the cursor on the line
below it, as in mlterm, konsole and libvte terminals. Images honor
transparent backgrounds (`P2=1`), DECSDM (`?80`) and sixel-scrolls-right
(`?8452`, both reportable with DECRQM), and slt answers XTSMGRAPHICS; the primary device attributes advertise sixel support. As in
xterm and foot, text written or erased over a SIXEL image replaces it in
those cells, and an opaque image replaces the older ones it covers. Pixels
that the raster attributes declare non-square are stretched to squares.

Kitty supports chunked direct, file, temporary file, and POSIX shared-memory
transfers; RGB, RGBA, PNG and palette-indexed images; zlib compression;
cropping, scaling (aspect-preserving), offsets, z-order, image and placement
IDs, image numbers, queries, cursor movement, relative placements, Unicode
placeholders, every deletion selector, and animation (frames, frame
composition, and terminal- or client-driven playback). Placeholder cells
pick their placement by underline color, palette-indexed images follow
palette changes (OSC 4), and images with negative z are drawn under the
text: below cell backgrounds that are not the default for z under
-1073741824, above them otherwise.

iTerm2 inline images (`imgcat` and the tools that use its protocol) may be
PNG, baseline JPEG (through cl-jpeg, which does not decode progressive
JPEG) or GIF, animated GIFs included, sent whole (`File=`) or in parts
(`MultipartFile=`, `FilePart=`, `FileEnd`). Files without `inline=1` are
downloads, which slt ignores. `width` and `height` take cells, pixels
(`px`), a percentage of the screen or `auto`; with `preserveAspectRatio=0`
the image fills both. The cursor then moves past the image as after a Kitty
image, unless `doNotMoveCursor=1` (from WezTerm).

Placements scroll with the text, into the scrollback and within scroll
regions, and are cleared by `ED 2`/`ED 3`, RIS and entering the alternate
screen. Images are rendered by the cocoa/Metal, GL, GTK/Cairo, CLX, Tk,
web, and headless frontends. Set `*sixel-images*`, `*kitty-graphics-images*` or
`*iterm2-images*` to `nil` to pass that protocol through untouched.

On macOS, install the SIXEL decoder with `brew install libsixel`. The reusable
bindings and Kitty codec live in [`lib/cl-libsixel`](lib/cl-libsixel/) and
[`lib/kitty-graphics`](lib/kitty-graphics/).

With the native macOS (`cocoa`) frontend, Command+N (or **Shell ▸ New
Window**) opens another independent terminal window. Each window has its own
shell and remains open until it or its child process exits; slt quits after
the last window closes.

The `gl` frontend uses [cl-glfw3](https://github.com/AlexCharlton/cl-glfw3)
with the system's GLFW (3.3 or later; Homebrew's `glfw` on macOS), and
cl-opengl. GLFW chooses X11 or Wayland when it starts.
`make gl-test` renders styled text in a hidden window and checks the pixels.
[doc/gpu-frontends.md](doc/gpu-frontends.md) describes the design and what
Vulkan would add.

The `gtk` frontend uses the system's GTK 3 (on macOS, Homebrew's `gtk+3`).
Build it in with `FRONTENDS=gtk`, or on its own with `make lean-gtk`.
`make gtk-test` checks its drawing the same way `make gl-test` does. It
has no input method support yet, and its font menu lists no families.

The `clx` frontend speaks the X protocol in Lisp, so it needs no X
libraries, only an X server with the RENDER extension. It finds and
rasterizes fonts as the `gl` frontend does (FreeType and fontconfig, the
shared `slt/glyphs` system), packs the grid as the GPU frontends do, and
draws glyph runs with RENDER, so text and colors match the other frontends.
It scales with the `Xft.dpi` resource. Build it in with `FRONTENDS=clx`, or
on its own with `make lean-clx`. `make clx-test` checks its drawing.

## Connecting from SLIME or SLY

slt can start a Swank server (for SLIME and other clients of its protocol)
or a Slynk server (for SLY) inside itself. You can then inspect, redefine and
hook into the running terminal from your editor. To toggle the server:

- choose **Lisp ▸ Start Swank Server** in the macOS menu bar (cocoa frontend);
- press Command+Shift+L on macOS (Super+Shift+L elsewhere) or Control+Shift+L;
- or start it at launch with `--repl-server swank` or `--repl-server slynk`.

The server listens on `localhost:4005`, so `M-x slime-connect` or
`M-x sly-connect` works with the default answers. Change the port with
`--repl-port` or `*repl-server-port*`, and the default server with `(setf
*repl-server* :slynk)`. You can also call `(start-repl-server)` from your
init file. Stopping the server only stops new connections; editors already
connected stay connected.

The key binding is `repl-server-key-handler` on `*key-hook*`. If a program
you run needs Control+Shift+L, remove the handler and use the menu instead.
Other menu entries can be added to `*menu-items*`.

Both servers, and the contribs their clients ask for, are built into
`bin/slt`, because the executable cannot load systems while it runs. SLIME
and SLY warn when their version differs from the server's (SLIME 2.32 and
the SLY of 2026-10-13 here). You can usually continue.
`sly-retro` cannot be loaded, since it would rename Slynk's packages
over Swank's. Your code runs on the server's threads, not the UI thread.

## Configuration

As with StumpWM, the init file is plain Lisp loaded at startup, in the
`slt` package, so it can run anything. slt loads the first of `$SLT_CONFIG`,
`~/.sltrc`, `~/.slt.d/init.lisp` and `~/.config/slt/init.lisp` that exists;
`--config FILE` names one, and `--no-config` skips it. An error stops the
rest of the file but not slt: it is printed in red in the terminal. Reload
the file with **slt ▸ Reload Settings** (Command+Shift+R; Super+Shift+R or
Control+Shift+R elsewhere), and open it in your editor with **slt ▸
Settings…** (Command+,), which creates it from a commented template first.
For example:

```lisp
(setf *default-engine* :ghostty)
;; The default is a soft dark theme; :ansi :xterm restores classic colors.
(setf *palette* (make-palette :foreground #xcdd6f4 :background #x1e1e2e))
(add-hook '*title-change-hook* (lambda (emulator title) (declare (ignore emulator)) (print title)))
(add-hook '*key-hook* (lambda (emulator key modifiers)   ; return true to consume the key
                        (declare (ignore emulator))
                        (and (eql key #\q) (member :super modifiers))))
```

## Shortcuts and the window

On macOS the menus hold the usual commands: Copy, Paste and Select All
(Command+C, V, A), Clear Scrollback (Command+K), Bigger, Smaller and Actual
Size (Command+=, -, 0), Enter Full Screen (Control+Command+F), Close Window
(Command+W), plus the Lisp menu. The right button opens a menu with the
Edit commands unless the program tracks the mouse. On the other frontends
`command-key-handler` on `*key-hook*` gives the same commands on Super.
`*commands*` maps each menu command to the function it calls.

Links are underlined under the pointer, and Command-click (Super-click)
opens them: OSC 8 hyperlinks (`*hyperlink-schemes*`) and URLs in the text,
of the schemes in `*url-schemes*` (http, https, ftp, ftps, sftp, file,
mailto, ssh, git, gemini, gopher, news, nntp, irc, ircs, xmpp, magnet, tel)
or starting `www.`. A URL that wraps onto the next row is found whole, and
punctuation or a closing bracket after it is left out unless it is the
URL's own. Files dropped on the window (cocoa, gl, gtk and clx) are
pasted as shell-quoted names. The bell beeps (`*audible-bell*`) and bounces
the Dock icon while slt is in the background (`*bell-hook*` holds
`ring-bell`). Programs that ask for focus events (mode 1004) get them.
`*window-padding*` sets the space around the grid in points.

Characters the font lacks are drawn from the first family in
`*font-fallback-families*` that has them, then from any installed font.
Its default holds Symbols Nerd Font Mono, which slt ships in `fonts/`
(next to the executable), so the icons Emacs, Neovim and shell prompts
print need no patched font. On macOS, CoreText's own fallbacks (emoji, CJK)
come after the list. Elsewhere color fonts (Noto Color Emoji) draw in
color too.

The cursor blinks when the program asks for it (DECSCUSR 1, 3 or 5, or
mode 12), only in the focused window, and stays shown while you type. Set
`*cursor-blink*` to `t` to always blink or `nil` never to, and
`*cursor-blink-interval-ms*` (600) for the pace.

Children see `COLORTERM=truecolor`, `TERM_PROGRAM=slt` and, when the
environment has no locale, `LANG=en_US.UTF-8`; add your own with
`*environment-variables*`. `--directory DIR` starts the shell in DIR and
`--login` makes it a login shell.

## macOS application

`make app` wraps `bin/` into `build/slt.app` (icon, `Info.plist`, ad-hoc
signature) and `make install-app` copies it into `~/Applications` (set
`APPDIR=/Applications` for everyone). Started from the Finder or the Dock,
slt runs a login shell in your home directory, as Terminal does
(`*login-shell*`, `*working-directory*`). The icon is `macos/slt.svg`;
`make icon` regenerates `macos/slt.icns` from it (needs `rsvg-convert`).

## Distribution

`bin/` is built with [Deploy](https://shinmera.com/docs/deploy), which copies
each foreign library the image loaded next to the executable and opens it
from there at startup. `src/deploy.lisp` lists the system libraries to leave
out. The implementation is `scripts/build`.

On macOS, the build also:

- Uses a copy of Homebrew's SBCL runtime whose zstd reference points at
  `@executable_path`. The finished executable cannot be edited afterwards,
  because the Lisp image is appended to it.
- Ad-hoc signs the libraries and the executable, since Apple Silicon kills
  binaries whose signatures are missing or invalid. To distribute outside
  your own machines, sign with a Developer ID and notarize.

LTK is always in, but needs `wish` on the target machine.
