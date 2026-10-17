# slt

A terminal emulator in Common Lisp. The terminal emulation layer (the
*engine*) and the user interface (the *backend*) are separate pieces, and
either can be swapped, configured or extended from Lisp.

## Building and running

Needs SBCL, [ocicl](https://github.com/ocicl/ocicl) and a C compiler (GNU make;
`gmake` on the BSDs).

```sh
make                 # build bin/slt and the libraries it loads
bin/slt              # run it (or: make run ARGS='--engine libvterm')
make check           # unit tests and engine conformance
make install         # into /usr/local; PREFIX and DESTDIR are honored
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
| `BACKENDS` | `gl` off macOS | optional backends to build in |
| `SBCL`, `OCICL` | `sbcl`, `ocicl` | |

For a package, run `make deps` while the network is available (building
ghostty also downloads Zig and Ghostty), then
`make && make install DESTDIR="$pkgdir" PREFIX=/usr`.

`scripts/lisp` runs SBCL with ASDF limited to this project, for working at a
REPL: everything is resolved from `ocicl.csv`, `ocicl/` and `lib/`.

## Engines

| engine | what it is | setup |
|---|---|---|
| `ghostty` | Ghostty's libghostty-vt, via cl-ghostty-vt; fastest, with scrollback | built by `make` (downloads Zig) |
| `libvterm` | libvterm 0.3.3, the library Neovim uses (`lib/cl-libvterm`) | built by `make` |
| `3bst` | pure Lisp; a fork of 3b's port of st (`lib/3bst`) | none |

By default slt uses the first engine in this order that is available.

Choose one with `--engine NAME`, `SLT_ENGINE=NAME`, `(launch-terminal :engine
:NAME)` or `(setf slt:*default-engine* :NAME)` in your init file. Engines with
native libraries live in their own `slt/NAME` systems, which load on first use.

Output is parsed on a reader thread, and the UI draws at most once per frame,
so heavy output never waits on rendering. Ghostty copies only changed rows
into the grid through a small C helper (`src/native/slt-ghostty.c`), which is
compiled on first use. On a 23 MB `cat`, ghostty manages about 140 MB/s,
libvterm about 12 MB/s and 3bst about 2.6 MB/s.

Scrollback (ghostty): use the mouse wheel or trackpad, Shift+PageUp/PageDown,
and Shift+Home/End. Typing returns to the live screen. On the alternate
screen (vim, less), the wheel sends arrow keys instead.

`make conformance` (part of `make check`) runs the same byte streams through every available engine
and compares the resulting screens. `slt/bench` measures throughput.

## Backends

| backend | |
|---|---|
| `cocoa` | macOS default: AppKit window, Metal renderer, CoreText glyphs |
| `gl` | Linux/BSD default: GLFW window (X11 or Wayland), OpenGL 3.3 or OpenGL ES 3.0, FreeType glyphs found through font-discovery; also runs on macOS with CoreText glyphs |
| `ltk` | Tk; portable fallback (needs `wish`) where `gl` cannot run |
| `sdl2` | SDL2 renderer (`slt/sdl2`; needs SDL2, SDL2_ttf and SDL2_image) |
| `web` | serves a browser terminal; see [web/README.md](web/README.md) |
| `headless` | no UI; for tests, scripting and remote frontends |

Choose one with `--backend NAME` or `SLT_BACKEND=NAME`.

The `gl` backend uses Shirakumo's [glfw](https://codeberg.org/shirakumo/glfw)
bindings, which ship GLFW 3.4 for Linux and macOS (the BSDs use the system's
`libglfw`), and cl-opengl. GLFW chooses X11 or Wayland when it starts.
`make gl-test` renders styled text in a hidden window and checks the pixels.
[doc/gpu-backends.md](doc/gpu-backends.md) describes the design and what
Vulkan would add.

## Configuration

`~/.config/slt/init.lisp` is loaded at startup (skip it with `--no-config`).
It runs in the `slt` package:

```lisp
(setf *default-engine* :ghostty)
;; The default is a soft dark theme; :ansi :xterm restores classic colors.
(setf *palette* (make-palette :foreground #xcdd6f4 :background #x1e1e2e))
(add-hook '*title-change-hook* (lambda (emulator title) (declare (ignore emulator)) (print title)))
(add-hook '*key-hook* (lambda (emulator key modifiers)   ; return true to consume the key
                        (declare (ignore emulator))
                        (and (eql key #\q) (member :super modifiers))))
```

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

The SDL2 backend is left out because Homebrew's SDL2 brings a chain of
further libraries (`make BACKENDS=sdl2` builds it in). LTK is always in, but
needs `wish` on the target machine.
